import sys
from pathlib import Path
import fitz

pdf_path = Path(r"c:\Users\kukumar\OneDrive - UGent\My Projects\Grafitti Project\Urban_Foraging_Archive\Articles_drafts\2024 Snatching\20250825_ModeBased_Highlighting\Study_43_ENHANCED_HYBRID_HIGHLIGHTED.pdf")
if not pdf_path.exists():
    print(f"PDF not found: {pdf_path}")
    sys.exit(1)

with fitz.open(pdf_path) as doc:
    total = 0
    empty_contents = 0
    titles = {}
    for i, page in enumerate(doc):
        ann = page.first_annot
        while ann:
            total += 1
            info = ann.info or {}
            title = info.get("title") or info.get("subject") or ""
            content = info.get("content") or ""
            if not content.strip():
                empty_contents += 1
            titles[title] = titles.get(title, 0) + 1
            ann = ann.next
    print({
        "total_annots": total,
        "empty_contents": empty_contents,
        "unique_titles": len(titles),
        "top_titles": sorted(titles.items(), key=lambda x: -x[1])[:10],
    })
