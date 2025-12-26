#!/usr/bin/env python3
"""
Enhanced PDF Highlighter - Simplified and Improved Version (compatible)
- Highlights direct variables and supporting quotes with minimal logic
- Normalizes text for better matching
- Skips References section pages
- Sets annotation title to variable name; empty content
"""

import pandas as pd
import fitz  # PyMuPDF
from pathlib import Path
import re
import sys
import logging
from datetime import datetime
from typing import List, Dict, Optional, Tuple
import unicodedata

class PDFHighlighter:
    def __init__(self):
        logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
        self.logger = logging.getLogger(__name__)
        self.DIRECT_VARS = {
            'Country', 'City', 'State_or_Region', 'Data_Collection_Period',
            'Crime_Type', 'Model_Type', 'Data_Sources',
            'Number_of_Data_Sources', 'Crime_Incidents', 'Software_Used'
        }
        self.COLORS = {
            'location': [1, 0.8, 0],
            'temporal': [0, 0.8, 1],
            'crime': [1, 0.4, 0.4],
            'method': [0.6, 0.8, 1],
            'rationale': [1, 1, 0.4],
            'analysis': [0.8, 1, 0.6],
            'future': [0.9, 0.7, 1],
            'spatial': [0, 1, 0.8]
        }

    def get_variable_color(self, variable_name: str):
        mapping = {
            'Country': 'location', 'City': 'location', 'State_or_Region': 'location',
            'Data_Collection_Period': 'temporal',
            'Crime_Type': 'crime', 'Crime_Incidents': 'crime', 'Crime_Type_Group': 'crime',
            'Model_Type': 'method', 'Data_Sources': 'method', 'Number_of_Data_Sources': 'method', 'Software_Used': 'method',
            'Unit_Selection_Rationale': 'rationale',
            'MAUP_Discussion': 'analysis', 'Sensitivity_Analysis': 'analysis', 'Data_Limitations': 'analysis',
            'Future_Suggestions': 'future',
            'Spatial_Unit_Name': 'spatial', 'Unit_Size': 'spatial'
        }
        return self.COLORS.get(mapping.get(variable_name, 'method'))

    def normalize_text(self, text: str) -> str:
        if pd.isna(text) or not text:
            return ""
        t = unicodedata.normalize('NFKD', str(text).strip())
        t = t.replace('“', '"').replace('”', '"').replace('‘', "'").replace('’', "'")
        t = t.replace('–', '-').replace('—', '-')
        t = re.sub(r'\s+', ' ', t)
        return t

    def extract_quotes(self, text: str) -> List[str]:
        if not text or pd.isna(text):
            return []
        t = self.normalize_text(text)
        out: List[str] = []
        # 1) explicit quoted segments
        out.extend([q.strip() for q in re.findall(r'"([^"]*)"', t) if q.strip()])
        # 2) inline dash-delimited quotes: "..." - "..." -> split
        if not out and '" - "' in t:
            parts = [p.strip(' "') for p in t.split('" - "') if p.strip(' "')]
            out.extend([p for p in parts if len(p) > 10])
        # 3) bullet/newline style
        if not out:
            lines = re.split(r'[\r\n]+|\s*[\-•*]\s+', t)
            out.extend([ln.strip(' "') for ln in lines if ln and len(ln.strip()) > 10])
        # 4) fallback whole text
        if not out and len(t) > 10:
            out.append(t)
        # dedupe while preserving order
        seen = set(); ans = []
        for q in out:
            k = q.lower()
            if k not in seen:
                seen.add(k); ans.append(q)
        return ans[:12]

    def _find_reference_heading_y(self, page) -> Optional[float]:
        pats = (r"^\s*references\s*$", r"^\s*bibliograph(?:y|ies)\s*$", r"^\s*works\s+cited\s*$")
        try:
            lines = (page.get_text("text") or "").splitlines()[:40]
            for i, ln in enumerate(lines):
                t = ln.strip().lower()
                if any(re.match(p, t) for p in pats):
                    # rough y for 25% page height if heading detected
                    return page.rect.height * 0.25
        except Exception:
            return None
        return None

    def find_text_in_pdf(self, doc, search_text: str, max_pages: int = 10) -> Optional[Tuple[int, fitz.Rect]]:
        s = self.normalize_text(search_text)
        if not s or len(s) < 5:
            return None
        variants = [s, s.lower(), re.sub(r'[^\w\s]', '', s)]
        for p in range(min(max_pages, len(doc))):
            page = doc[p]
            # simple references skip: if heading appears on this or prior page, skip rest
            yref = self._find_reference_heading_y(page)
            if yref is not None and p > 0:
                break
            for v in variants:
                if not v: continue
                try:
                    # ignore-case if supported via kwargs, fallback to default
                    try:
                        rects = page.search_for(v, flags=1)
                    except TypeError:
                        rects = page.search_for(v)
                    if rects:
                        return p, rects[0]
                except Exception:
                    continue
        return None

    def highlight_text(self, page, rect: fitz.Rect, color, variable_name: str, value: str) -> bool:
        try:
            annot = page.add_highlight_annot(rect)
            annot.set_colors(stroke=color)
            annot.set_opacity(0.7)
            # Policy: title-only, empty content
            try:
                annot.set_info(title=variable_name)
                try:
                    annot.set_contents("")
                except AttributeError:
                    try:
                        annot.set_content("")
                    except Exception:
                        pass
            except Exception:
                pass
            annot.update()
            return True
        except Exception as e:
            self.logger.error(f"Error creating highlight: {e}")
            return False

    def process_study(self, study_id, pdf_folder="Review_articles", combined_csv=None):
        try:
            self.logger.info(f"Processing Study {study_id}")
            if combined_csv is None:
                combined_csv = "20250820_Analysis & Results/20250820_combined_dataset.csv"
            df = pd.read_csv(combined_csv)
            rowq = df[df['Study_ID'] == int(study_id)]
            if rowq.empty:
                raise ValueError(f"Study {study_id} not found in CSV")
            row = rowq.iloc[0]
            # locate PDF
            patterns = [
                f"*{int(study_id):02d}*.pdf", f"*{study_id}*.pdf", f"*{study_id}_*.pdf",
                (f"0{study_id}_*.pdf" if int(study_id) < 10 else f"{study_id}_*.pdf"),
            ]
            pdf_file: Optional[Path] = None
            for pat in patterns:
                m = list(Path(pdf_folder).glob(pat))
                if m:
                    pdf_file = m[0]; break
            if not pdf_file or not pdf_file.exists():
                raise FileNotFoundError(f"PDF not found for study {study_id}")
            doc = fitz.open(pdf_file)
            self.logger.info(f"Opened PDF: {pdf_file} ({len(doc)} pages)")
            highlights = 0
            # Direct variables
            for var_name in self.DIRECT_VARS:
                if var_name not in row or pd.isna(row[var_name]):
                    continue
                val = str(row[var_name]).strip()
                if not val:
                    continue
                res = self.find_text_in_pdf(doc, val)
                if res:
                    p, rect = res
                    color = self.get_variable_color(var_name)
                    if self.highlight_text(doc[p], rect, color, var_name, val):
                        highlights += 1
            # Supporting quotes
            quote_cols = [c for c in row.index if c.startswith('Supporting_quotes_for__')]
            for qcol in quote_cols:
                if pd.isna(row[qcol]) or not row[qcol]:
                    continue
                var_name = qcol.replace('Supporting_quotes_for__', '').rstrip('_')
                quotes = self.extract_quotes(row[qcol])
                for q in quotes:
                    res = self.find_text_in_pdf(doc, q)
                    if res:
                        p, rect = res
                        color = self.get_variable_color(var_name)
                        if self.highlight_text(doc[p], rect, color, var_name, q):
                            highlights += 1
            # save
            outdir = Path(f"{datetime.now().strftime('%Y%m%d')}_Highlighted_PDFs")
            outdir.mkdir(exist_ok=True)
            out = outdir / f"Study_{study_id}_Highlighted.pdf"
            doc.save(out)
            doc.close()
            self.logger.info(f"Saved: {out}")
            self.logger.info(f"Total highlights created: {highlights}")
            return {"success": True, "highlights_created": highlights, "output_file": str(out)}
        except Exception as e:
            self.logger.error(f"Error processing study {study_id}: {e}")
            return {"success": False, "error": str(e)}


def main():
    if len(sys.argv) < 2:
        print("Usage: python pdf_highlighter.py <study_id> [pdf_folder] [combined_csv]")
        sys.exit(1)
    study_id = sys.argv[1]
    pdf_folder = sys.argv[2] if len(sys.argv) > 2 else "Review_articles"
    combined_csv = sys.argv[3] if len(sys.argv) > 3 else "20250820_Analysis & Results/20250820_combined_dataset.csv"
    hl = PDFHighlighter()
    result = hl.process_study(study_id, pdf_folder, combined_csv)
    if result.get('success'):
        print(f"✅ Successfully processed Study {study_id}")
        print(f"   Highlights created: {result.get('highlights_created', 0)}")
        print(f"   Output file: {result.get('output_file', 'unknown')}")
    else:
        print(f"❌ Failed: {result.get('error')}")

if __name__ == "__main__":
    main()
