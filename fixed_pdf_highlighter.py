#!/usr/bin/env python3
"""
Fixed PDF Highlighter - Addresses partial highlighting and overlap issues
Key fixes:
1. Better multi-line text detection and highlighting
2. Overlap prevention system
3. Improved text boundary detection
4. Full quote highlighting instead of partial matches
"""

import pandas as pd
import fitz  # PyMuPDF
from pathlib import Path
import re
import sys
import logging
from datetime import datetime
from typing import List, Dict, Optional, Tuple, Set, Union
import unicodedata
import json
import threading
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
import difflib
from dataclasses import dataclass, asdict
import hashlib

@dataclass
class HighlightResult:
    """Container for highlight operation results"""
    study_id: str
    variable_name: str
    quote_text: str
    found: bool
    page_number: int = -1
    confidence: float = 0.0
    match_method: str = ""
    processing_time: float = 0.0
    highlight_rect: str = ""  # String representation of rect for debugging

@dataclass
class StudyProcessingResult:
    """Container for entire study processing results"""
    study_id: str
    success: bool
    highlights_created: int = 0
    quotes_found: int = 0
    processing_time: float = 0.0
    output_file: str = ""
    error_message: str = ""
    highlight_details: List[HighlightResult] = None

class FixedPDFHighlighter:
    def __init__(self, max_workers: int = 4, enable_fuzzy_matching: bool = True):
        """Initialize the fixed PDF highlighter with overlap prevention"""
        self.setup_logging()
        self.max_workers = max_workers
        self.enable_fuzzy_matching = enable_fuzzy_matching
        
        # Track existing highlights to prevent overlaps
        self.existing_highlights: Dict[int, List[fitz.Rect]] = {}  # page_num -> list of rects
        
        # Performance tracking
        self.start_time = time.time()
        
        # Enhanced direct variables with categories
        self.VARIABLE_CATEGORIES = {
            'location': {'Country', 'City', 'State_or_Region', 'Spatial_Unit_Name'},
            'temporal': {'Data_Collection_Period'},
            'crime': {'Crime_Type', 'Crime_Incidents', 'Crime_Type_Group'},
            'methodology': {'Model_Type', 'Data_Sources', 'Number_of_Data_Sources', 'Software_Used'},
            'spatial': {'Unit_Size', 'Total_Study_Area_Size', 'Number_of_Units', 'Average_Population_per_Unit'},
            'analytical': {'Independent_Variables', 'Model_Fit_Statistics', 'Coefficients'},
            'quality': {'Data_Limitations', 'MAUP_Discussion', 'Sensitivity_Analysis'},
            'future': {'Future_Suggestions'}
        }
        
        # Flatten for easy access
        self.DIRECT_VARS = set()
        for category_vars in self.VARIABLE_CATEGORIES.values():
            self.DIRECT_VARS.update(category_vars)
        
        # Enhanced color scheme with better contrast and spacing
        self.COLORS = {
            'location': [1.0, 0.85, 0.2],     # Bright yellow
            'temporal': [0.2, 0.8, 1.0],      # Cyan
            'crime': [1.0, 0.5, 0.5],         # Light red
            'methodology': [0.7, 0.85, 1.0],  # Light blue
            'spatial': [0.2, 1.0, 0.8],       # Mint green
            'analytical': [0.8, 1.0, 0.7],    # Light green
            'quality': [1.0, 0.7, 1.0],       # Pink
            'future': [0.9, 0.8, 1.0],        # Light purple
            'default': [0.8, 0.8, 0.8]        # Light gray
        }

    def setup_logging(self):
        """Setup enhanced logging"""
        log_filename = f"{datetime.now().strftime('%Y%m%d')}_fixed_pdf_highlighting.log"
        
        formatter = logging.Formatter(
            '%(asctime)s - %(levelname)s - [%(funcName)s:%(lineno)d] - %(message)s'
        )
        
        self.logger = logging.getLogger('FixedPDFHighlighter')
        self.logger.setLevel(logging.INFO)
        
        # Clear existing handlers
        for handler in self.logger.handlers[:]:
            self.logger.removeHandler(handler)
        
        # File handler
        fh = logging.FileHandler(log_filename, mode='w', encoding='utf-8')
        fh.setLevel(logging.INFO)
        fh.setFormatter(formatter)
        
        # Console handler
        ch = logging.StreamHandler()
        ch.setLevel(logging.INFO)
        ch.setFormatter(formatter)
        
        self.logger.addHandler(fh)
        self.logger.addHandler(ch)
        
        self.logger.info(f"Fixed PDF Highlighter initialized - Log file: {log_filename}")

    def get_variable_category_and_color(self, variable_name: str) -> Tuple[str, List[float]]:
        """Get the category and color for a variable"""
        for category, variables in self.VARIABLE_CATEGORIES.items():
            if variable_name in variables:
                return category, self.COLORS[category]
        return 'default', self.COLORS['default']

    def normalize_text_enhanced(self, text: str) -> str:
        """Enhanced text normalization"""
        if not text:
            return ""
        
        t = unicodedata.normalize('NFKC', str(text))
        t = t.replace('\u00A0', ' ').replace('\u00AD', '')
        t = t.replace('\u2009', ' ').replace('\u202F', ' ')
        t = t.replace('"', '"').replace('"', '"').replace(''', "'").replace(''', "'")
        t = t.replace('–', '-').replace('—', '-').replace('−', '-')
        t = re.sub(r'\s*\n\s*', ' ', t)
        t = re.sub(r'\s*-\s*\n\s*', '', t)
        t = re.sub(r'\s+', ' ', t)
        
        return t.strip()

    def clean_cell_value_enhanced(self, s: str) -> str:
        """Enhanced cell value cleaning"""
        if not s or pd.isna(s):
            return ""
        
        s = self.normalize_text_enhanced(str(s))
        s = re.sub(r'^[""\s]+|[""\s]+$', '', s)
        s = re.sub(r'[.。…]+$', '', s)
        s = re.sub(r'^\d+[\.\)]\s*', '', s)
        s = re.sub(r'^\s*[-•*]\s*', '', s)
        
        return s.strip()

    def extract_quotes_enhanced(self, text: str) -> List[str]:
        """Enhanced quote extraction"""
        if not text or pd.isna(text):
            return []
        
        text = self.normalize_text_enhanced(text)
        quotes = []
        
        # Method 1: Split by line breaks (most common in your data)
        if '\n' in text:
            lines = [line.strip() for line in text.split('\n') if line.strip()]
            quotes.extend([self.clean_cell_value_enhanced(line) for line in lines 
                          if len(line.strip()) > 10])
        
        # Method 2: Extract quoted text
        if not quotes:
            quote_patterns = [
                r'"([^"]{10,})"',
                r"'([^']{10,})'",
                r'\"([^\"]{10,})\"',
            ]
            
            for pattern in quote_patterns:
                matches = re.findall(pattern, text)
                quotes.extend([self.clean_cell_value_enhanced(match) for match in matches])
        
        # Method 3: Split by separators
        if not quotes:
            separators = [r'\s*[•*]\s*', r'\s*-\s*(?=[A-Z])', r'\s*\|\s*']
            
            parts = [text]
            for separator in separators:
                new_parts = []
                for part in parts:
                    new_parts.extend(re.split(separator, part))
                parts = new_parts
            
            quotes.extend([self.clean_cell_value_enhanced(part) for part in parts 
                          if len(part.strip()) > 10])
        
        # Method 4: Use entire text if still no quotes
        if not quotes and len(text.strip()) > 10:
            quotes.append(self.clean_cell_value_enhanced(text))
        
        # Deduplicate and filter
        final_quotes = []
        seen = set()
        
        for quote in quotes:
            if not quote or len(quote) < 10:
                continue
            
            key = quote.lower()[:50]  # Use first 50 chars as key
            if key not in seen:
                seen.add(key)
                final_quotes.append(quote)
        
        return final_quotes[:10]  # Limit to prevent too many highlights

    def find_complete_text_boundaries(self, page: fitz.Page, search_term: str) -> Optional[List[fitz.Rect]]:
        """
        Find complete text boundaries for multi-line text to ensure full highlighting
        This addresses the partial highlighting issue
        """
        try:
            # Get all text blocks (these represent logical text units)
            blocks = page.get_text("blocks")
            if not blocks:
                return None
            
            # Normalize search term for matching
            norm_search = self.normalize_text_enhanced(search_term.lower())
            
            # Find blocks that contain our search term
            matching_rects = []
            
            for block in blocks:
                if len(block) < 5:  # Invalid block
                    continue
                
                block_text = self.normalize_text_enhanced(block[4].lower())
                
                # Check if this block contains our search term
                if norm_search in block_text or self.partial_match(norm_search, block_text):
                    # Create rect for this entire block
                    rect = fitz.Rect(block[0], block[1], block[2], block[3])
                    matching_rects.append(rect)
            
            if matching_rects:
                return matching_rects
            
            # Fallback: Use word-level matching for better precision
            return self.find_word_level_boundaries(page, search_term)
            
        except Exception as e:
            self.logger.debug(f"Error in find_complete_text_boundaries: {e}")
            return None

    def partial_match(self, search_term: str, block_text: str, min_ratio: float = 0.6) -> bool:
        """Check if search term partially matches block text"""
        if not search_term or not block_text:
            return False
        
        # Split into words and check for significant word overlap
        search_words = set(search_term.split())
        block_words = set(block_text.split())
        
        if not search_words:
            return False
        
        overlap = len(search_words.intersection(block_words))
        ratio = overlap / len(search_words)
        
        return ratio >= min_ratio

    def find_word_level_boundaries(self, page: fitz.Page, search_term: str) -> Optional[List[fitz.Rect]]:
        """Find word-level boundaries for more precise highlighting"""
        try:
            # Get words with their positions
            words = page.get_text("words", flags=fitz.TEXT_DEHYPHENATE)
            if not words:
                return None
            
            # Normalize search term
            search_words = self.normalize_text_enhanced(search_term.lower()).split()
            if not search_words:
                return None
            
            # Find sequences of words that match our search term
            matching_rects = []
            
            for i in range(len(words) - len(search_words) + 1):
                # Check if we have a match starting at position i
                match_found = True
                word_rects = []
                
                for j, search_word in enumerate(search_words):
                    if i + j >= len(words):
                        match_found = False
                        break
                    
                    word_data = words[i + j]
                    if len(word_data) < 5:
                        match_found = False
                        break
                    
                    word_text = self.normalize_text_enhanced(word_data[4].lower())
                    
                    # Check for word match (exact or partial)
                    if search_word not in word_text and word_text not in search_word:
                        # Try fuzzy matching for this word
                        if not self.enable_fuzzy_matching or self.fuzzy_word_match(search_word, word_text) < 0.7:
                            match_found = False
                            break
                    
                    # Add word rectangle
                    word_rects.append(fitz.Rect(word_data[0], word_data[1], word_data[2], word_data[3]))
                
                if match_found and word_rects:
                    # Merge consecutive word rectangles into spans
                    merged_rects = self.merge_consecutive_rects(word_rects)
                    matching_rects.extend(merged_rects)
            
            return matching_rects if matching_rects else None
            
        except Exception as e:
            self.logger.debug(f"Error in find_word_level_boundaries: {e}")
            return None

    def fuzzy_word_match(self, word1: str, word2: str) -> float:
        """Calculate fuzzy match score for individual words"""
        if not word1 or not word2:
            return 0.0
        
        return difflib.SequenceMatcher(None, word1, word2).ratio()

    def merge_consecutive_rects(self, rects: List[fitz.Rect]) -> List[fitz.Rect]:
        """Merge consecutive rectangles that are on the same or nearby lines"""
        if not rects:
            return []
        
        if len(rects) == 1:
            return rects
        
        # Sort rects by position (top to bottom, left to right)
        sorted_rects = sorted(rects, key=lambda r: (round(r.y0, 1), r.x0))
        
        merged = []
        current_line_rects = [sorted_rects[0]]
        current_y = round(sorted_rects[0].y0, 1)
        
        for rect in sorted_rects[1:]:
            rect_y = round(rect.y0, 1)
            
            # If this rect is on the same line (within 2 points), add to current line
            if abs(rect_y - current_y) <= 2:
                current_line_rects.append(rect)
            else:
                # Process current line and start new line
                if current_line_rects:
                    merged.append(self.merge_line_rects(current_line_rects))
                current_line_rects = [rect]
                current_y = rect_y
        
        # Process final line
        if current_line_rects:
            merged.append(self.merge_line_rects(current_line_rects))
        
        return merged

    def merge_line_rects(self, line_rects: List[fitz.Rect]) -> fitz.Rect:
        """Merge rectangles on the same line into a single rectangle"""
        if not line_rects:
            return fitz.Rect()
        
        if len(line_rects) == 1:
            return line_rects[0]
        
        # Find bounding rectangle
        min_x0 = min(r.x0 for r in line_rects)
        min_y0 = min(r.y0 for r in line_rects)
        max_x1 = max(r.x1 for r in line_rects)
        max_y1 = max(r.y1 for r in line_rects)
        
        return fitz.Rect(min_x0, min_y0, max_x1, max_y1)

    def check_overlap_with_existing(self, page_num: int, new_rect: fitz.Rect, 
                                   min_gap: float = 5.0) -> bool:
        """
        Check if new rectangle overlaps with existing highlights
        This addresses the overlap issue
        """
        if page_num not in self.existing_highlights:
            return False
        
        for existing_rect in self.existing_highlights[page_num]:
            # Check for overlap or too close proximity
            if self.rects_too_close(existing_rect, new_rect, min_gap):
                return True
        
        return False

    def rects_too_close(self, rect1: fitz.Rect, rect2: fitz.Rect, min_gap: float) -> bool:
        """Check if two rectangles are too close or overlapping"""
        # Expand both rects by min_gap/2 to create a buffer zone
        buffer = min_gap / 2
        
        expanded_rect1 = fitz.Rect(
            rect1.x0 - buffer, rect1.y0 - buffer,
            rect1.x1 + buffer, rect1.y1 + buffer
        )
        
        return expanded_rect1.intersects(rect2)

    def register_highlight(self, page_num: int, rect: fitz.Rect):
        """Register a highlight to prevent future overlaps"""
        if page_num not in self.existing_highlights:
            self.existing_highlights[page_num] = []
        
        self.existing_highlights[page_num].append(rect)

    def search_text_in_page_fixed(self, page: fitz.Page, search_terms: List[str], 
                                 page_num: int, min_confidence: float = 0.6) -> Optional[Tuple[List[fitz.Rect], float, str]]:
        """
        Fixed text search that returns complete text boundaries and prevents overlaps
        """
        best_rects = None
        best_confidence = 0.0
        best_method = ""
        
        # Try each search term
        for term in search_terms[:5]:  # Limit to prevent excessive searching
            if len(term.strip()) < 3:
                continue
            
            # Method 1: Find complete text boundaries (addresses partial highlighting)
            rects = self.find_complete_text_boundaries(page, term)
            if rects:
                # Filter out rects that overlap with existing highlights
                non_overlapping_rects = []
                for rect in rects:
                    if not self.check_overlap_with_existing(page_num, rect):
                        non_overlapping_rects.append(rect)
                
                if non_overlapping_rects:
                    confidence = 1.0  # Complete boundary match
                    if confidence > best_confidence:
                        best_rects = non_overlapping_rects
                        best_confidence = confidence
                        best_method = "complete_boundaries"
                        
                        # Early exit for perfect matches
                        if confidence >= 0.95:
                            break
            
            # Method 2: Standard PyMuPDF search as fallback
            if best_confidence < 0.9:
                try:
                    for flags in [fitz.TEXT_DEHYPHENATE, 1, 0]:
                        found_rects = page.search_for(term, flags=flags)
                        if found_rects:
                            # Check for overlaps and filter
                            non_overlapping_rects = []
                            for rect in found_rects:
                                if not self.check_overlap_with_existing(page_num, rect):
                                    non_overlapping_rects.append(rect)
                            
                            if non_overlapping_rects:
                                confidence = 0.9  # Direct search match
                                if confidence > best_confidence:
                                    best_rects = non_overlapping_rects
                                    best_confidence = confidence
                                    best_method = "direct_search"
                                break
                except Exception as e:
                    self.logger.debug(f"Direct search failed for '{term}': {e}")
        
        if best_rects and best_confidence >= min_confidence:
            return best_rects, best_confidence, best_method
        
        return None

    def create_highlight_annotation_fixed(self, page: fitz.Page, rects: List[fitz.Rect], 
                                         page_num: int, color: List[float], 
                                         variable_name: str, quote_text: str = "") -> bool:
        """Create highlight annotation with overlap prevention"""
        try:
            if not rects:
                return False
            
            # Register all rects to prevent future overlaps
            for rect in rects:
                self.register_highlight(page_num, rect)
            
            if len(rects) == 1:
                # Single rectangle highlight
                annot = page.add_highlight_annot(rects[0])
            else:
                # Multiple rectangles - use quads for better multi-line highlighting
                try:
                    quads = [fitz.Quad(rect) for rect in rects]
                    annot = page.add_highlight_annot(quads)
                except Exception:
                    # Fallback: highlight each rect separately
                    for rect in rects:
                        annot = page.add_highlight_annot(rect)
                        annot.set_colors(stroke=color)
                        annot.set_opacity(0.5)  # Slightly more transparent to reduce visual noise
                        annot.update()
                    return True
            
            # Set annotation properties
            annot.set_colors(stroke=color)
            annot.set_opacity(0.5)  # Better transparency for overlapping areas
            
            # Set title and content
            try:
                annot.set_info(title=variable_name)
                content = quote_text[:150] + "..." if len(quote_text) > 150 else quote_text
                annot.set_contents(content)
            except AttributeError:
                try:
                    annot.set_info(title=variable_name)
                except Exception:
                    pass
            
            annot.update()
            return True
            
        except Exception as e:
            self.logger.error(f"Failed to create highlight annotation: {e}")
            return False

    def process_direct_variable_fixed(self, doc: fitz.Document, var_name: str, 
                                    value: str, search_limit: int) -> HighlightResult:
        """Process a direct variable with fixed highlighting"""
        start_time = time.time()
        
        # Generate search variants
        variants = self.generate_search_variants(value, var_name)
        
        # Search through pages
        for page_num in range(min(search_limit, len(doc))):
            try:
                page = doc[page_num]
                result = self.search_text_in_page_fixed(page, variants, page_num)
                
                if result:
                    rects, confidence, method = result
                    category, color = self.get_variable_category_and_color(var_name)
                    
                    if self.create_highlight_annotation_fixed(
                        page, rects, page_num, color, var_name, value
                    ):
                        processing_time = time.time() - start_time
                        return HighlightResult(
                            study_id="",
                            variable_name=var_name,
                            quote_text=value,
                            found=True,
                            page_number=page_num + 1,
                            confidence=confidence,
                            match_method=method,
                            processing_time=processing_time,
                            highlight_rect=str(rects[0]) if rects else ""
                        )
            except Exception as e:
                self.logger.debug(f"Error processing page {page_num} for {var_name}: {e}")
                continue
        
        processing_time = time.time() - start_time
        return HighlightResult(
            study_id="",
            variable_name=var_name,
            quote_text=value,
            found=False,
            processing_time=processing_time
        )

    def generate_search_variants(self, value: str, var_name: str) -> List[str]:
        """Generate search variants for better matching"""
        if not value:
            return []
        
        variants = []
        clean_value = self.clean_cell_value_enhanced(value)
        variants.append(clean_value)
        
        # Add some basic variants
        if len(clean_value.split()) > 8:
            # For long text, add shorter versions
            words = clean_value.split()
            for size in [12, 10, 8]:
                if len(words) >= size:
                    variants.append(" ".join(words[:size]))
                    break
        
        return list(set(variants))[:5]  # Limit variants

    def process_quote_fixed(self, doc: fitz.Document, var_name: str, 
                          quote: str, search_limit: int) -> HighlightResult:
        """Process a supporting quote with fixed highlighting"""
        start_time = time.time()
        
        variants = [quote]
        if len(quote.split()) > 8:
            words = quote.split()
            # Add shorter versions for better matching
            for size in [15, 12, 10, 8]:
                if len(words) >= size:
                    variants.append(" ".join(words[:size]))
                    if len(variants) >= 3:
                        break
        
        # Search with higher confidence for quotes
        min_confidence = 0.7
        
        # Limit search scope for quotes
        quote_search_limit = min(search_limit, len(doc) // 2 + 3)
        
        for page_num in range(quote_search_limit):
            try:
                page = doc[page_num]
                result = self.search_text_in_page_fixed(page, variants, page_num, min_confidence)
                
                if result:
                    rects, confidence, method = result
                    category, color = self.get_variable_category_and_color(var_name)
                    
                    if self.create_highlight_annotation_fixed(
                        page, rects, page_num, color, var_name, quote
                    ):
                        processing_time = time.time() - start_time
                        return HighlightResult(
                            study_id="",
                            variable_name=var_name,
                            quote_text=quote,
                            found=True,
                            page_number=page_num + 1,
                            confidence=confidence,
                            match_method=method,
                            processing_time=processing_time,
                            highlight_rect=str(rects[0]) if rects else ""
                        )
            except Exception as e:
                self.logger.debug(f"Error processing quote on page {page_num}: {e}")
                continue
        
        processing_time = time.time() - start_time
        return HighlightResult(
            study_id="",
            variable_name=var_name,
            quote_text=quote,
            found=False,
            processing_time=processing_time
        )

    def find_pdf_file(self, study_id: str, pdf_folder: str) -> Optional[Path]:
        """Find PDF file for a given study ID"""
        pdf_path = Path(pdf_folder)
        if not pdf_path.exists():
            return None
        
        patterns = [
            f"*{int(study_id):02d}*.pdf",
            f"*{study_id}*.pdf",
            f"*{study_id}_*.pdf",
            f"{study_id}_*.pdf",
        ]
        
        for pattern in patterns:
            matches = list(pdf_path.glob(pattern))
            if matches:
                return matches[0]
        
        return None

    def find_reference_section_start(self, doc: fitz.Document) -> Optional[int]:
        """Find the start page of the reference section"""
        reference_patterns = [
            r'^\s*references\s*$',
            r'^\s*bibliography\s*$',
            r'^\s*works\s+cited\s*$',
        ]
        
        start_page = max(0, len(doc) - int(len(doc) * 0.3))
        
        for page_num in range(start_page, len(doc)):
            try:
                page = doc[page_num]
                text = page.get_text()
                lines = text.split('\n')[:20]
                
                for line in lines:
                    line_clean = line.strip().lower()
                    if any(re.match(pattern, line_clean) for pattern in reference_patterns):
                        return page_num
            except Exception:
                continue
        
        return None

    def process_single_study_fixed(self, study_id: str, df: pd.DataFrame, 
                                 pdf_folder: str, output_folder: str) -> StudyProcessingResult:
        """Process a single study with fixed highlighting"""
        start_time = time.time()
        
        # Reset highlight tracking for this study
        self.existing_highlights.clear()
        
        try:
            self.logger.info(f"Starting fixed processing for Study {study_id}")
            
            # Find study in dataframe
            study_rows = df[df['Study_ID'].astype(str) == str(study_id)]
            if study_rows.empty:
                raise ValueError(f"Study {study_id} not found in dataset")
            
            study_row = study_rows.iloc[0]
            
            # Find PDF file
            pdf_file = self.find_pdf_file(study_id, pdf_folder)
            if not pdf_file:
                raise FileNotFoundError(f"PDF file not found for Study {study_id}")
            
            # Open PDF document
            doc = fitz.open(pdf_file)
            total_pages = len(doc)
            self.logger.info(f"Opened PDF: {pdf_file.name} ({total_pages} pages)")
            
            # Find reference section to limit search scope
            ref_start = self.find_reference_section_start(doc)
            search_limit = ref_start if ref_start else total_pages
            
            # Track results
            highlight_results = []
            highlights_created = 0
            quotes_processed = 0
            
            # Process direct variables first (higher priority)
            self.logger.info(f"Processing direct variables for Study {study_id}")
            for var_name in self.DIRECT_VARS:
                if var_name not in study_row or pd.isna(study_row[var_name]):
                    continue
                
                value = str(study_row[var_name]).strip()
                if not value or value.lower() in ['n/a', 'na', '-', '--']:
                    continue
                
                result = self.process_direct_variable_fixed(
                    doc, var_name, value, search_limit
                )
                highlight_results.append(result)
                
                if result.found:
                    highlights_created += 1
                    self.logger.info(f"  ✓ Highlighted {var_name}: {value[:50]}...")
            
            # Process supporting quotes
            self.logger.info(f"Processing supporting quotes for Study {study_id}")
            quote_columns = [col for col in study_row.index 
                           if col.startswith('Supporting_quotes_for__')]
            
            for quote_col in quote_columns:
                if pd.isna(study_row[quote_col]) or not study_row[quote_col]:
                    continue
                
                var_name = quote_col.replace('Supporting_quotes_for__', '').strip('_')
                quotes = self.extract_quotes_enhanced(study_row[quote_col])
                quotes_processed += len(quotes)
                
                for quote in quotes:
                    result = self.process_quote_fixed(
                        doc, var_name, quote, search_limit
                    )
                    highlight_results.append(result)
                    
                    if result.found:
                        highlights_created += 1
                        self.logger.debug(f"  ✓ Highlighted quote for {var_name}: {quote[:30]}...")
            
            # Save highlighted PDF
            output_path = Path(output_folder)
            output_path.mkdir(parents=True, exist_ok=True)
            
            output_file = output_path / f"Study_{study_id}_Fixed_Highlighted.pdf"
            doc.save(str(output_file))
            doc.close()
            
            processing_time = time.time() - start_time
            
            self.logger.info(f"✅ Successfully processed Study {study_id} (FIXED VERSION)")
            self.logger.info(f"   Highlights created: {highlights_created}")
            self.logger.info(f"   Quotes processed: {quotes_processed}")
            self.logger.info(f"   Processing time: {processing_time:.2f}s")
            
            return StudyProcessingResult(
                study_id=study_id,
                success=True,
                highlights_created=highlights_created,
                quotes_found=quotes_processed,
                processing_time=processing_time,
                output_file=str(output_file),
                highlight_details=highlight_results
            )
            
        except Exception as e:
            processing_time = time.time() - start_time
            error_msg = f"Error processing Study {study_id}: {str(e)}"
            self.logger.error(error_msg)
            
            return StudyProcessingResult(
                study_id=study_id,
                success=False,
                processing_time=processing_time,
                error_message=error_msg
            )

def main():
    """Main function for command-line usage"""
    if len(sys.argv) < 2:
        print("Usage: python fixed_pdf_highlighter.py <study_id> [options]")
        print("Options:")
        print("  --csv <path>          Path to combined dataset CSV")
        print("  --pdf-folder <path>   Path to PDF folder")
        print("  --output <path>       Output folder for highlighted PDFs")
        print("  --no-fuzzy            Disable fuzzy matching")
        print("")
        print("Example:")
        print("  python fixed_pdf_highlighter.py 1 --csv data.csv")
        sys.exit(1)
    
    study_id = sys.argv[1]
    csv_file = "20250827_Analysis & Results/20250827_combined_dataset_no_reasoning.csv"
    pdf_folder = "Review_articles"
    output_folder = f"{datetime.now().strftime('%Y%m%d')}_Fixed_Highlighted_PDFs"
    enable_fuzzy = True
    
    # Parse additional arguments
    i = 2
    while i < len(sys.argv):
        if sys.argv[i] == '--csv' and i + 1 < len(sys.argv):
            csv_file = sys.argv[i + 1]
            i += 2
        elif sys.argv[i] == '--pdf-folder' and i + 1 < len(sys.argv):
            pdf_folder = sys.argv[i + 1]
            i += 2
        elif sys.argv[i] == '--output' and i + 1 < len(sys.argv):
            output_folder = sys.argv[i + 1]
            i += 2
        elif sys.argv[i] == '--no-fuzzy':
            enable_fuzzy = False
            i += 1
        else:
            i += 1
    
    # Load dataset
    try:
        df = pd.read_csv(csv_file, encoding='utf-8')
    except UnicodeDecodeError:
        try:
            df = pd.read_csv(csv_file, encoding='latin-1')
        except UnicodeDecodeError:
            df = pd.read_csv(csv_file, encoding='cp1252')
    
    # Initialize highlighter
    highlighter = FixedPDFHighlighter(enable_fuzzy_matching=enable_fuzzy)
    
    print(f"Fixed PDF Highlighter - Addresses partial highlighting & overlaps")
    print(f"Study to process: {study_id}")
    print(f"CSV file: {csv_file}")
    print(f"PDF folder: {pdf_folder}")
    print(f"Output folder: {output_folder}")
    print("="*70)
    
    # Process study
    result = highlighter.process_single_study_fixed(study_id, df, pdf_folder, output_folder)
    
    # Print results
    if result.success:
        print(f"\n🎉 SUCCESS! Fixed PDF highlighting completed!")
        print(f"Study: {result.study_id}")
        print(f"Highlights created: {result.highlights_created}")
        print(f"Quotes processed: {result.quotes_found}")
        print(f"Processing time: {result.processing_time:.2f}s")
        print(f"Output file: {result.output_file}")
        print(f"\n🔧 Key fixes applied:")
        print(f"  ✓ Full text boundary detection (no more partial highlighting)")
        print(f"  ✓ Overlap prevention system")
        print(f"  ✓ Multi-line text support")
        print(f"  ✓ Better transparency settings")
    else:
        print(f"❌ Processing failed: {result.error_message}")

if __name__ == "__main__":
    main()
