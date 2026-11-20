from pathlib import Path
from docx import Document
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.enum.table import WD_TABLE_ALIGNMENT, WD_CELL_VERTICAL_ALIGNMENT
from docx.enum.section import WD_SECTION
from docx.shared import Inches, Pt, RGBColor
from docx.oxml import OxmlElement
from docx.oxml.ns import qn

root = Path(r"C:\Users\revel\OneDrive\Desktop\R_cosmic")
src = root / "Dokumentace_nova_analyza_graf.docx"
out = root / "Dokumentace_nova_analyza_graf_podle_vzoru.docx"
doc = Document(src)

# Reference-derived page setup: A4, portrait, generous academic margins.
for section in doc.sections:
    section.page_width = Inches(8.27)
    section.page_height = Inches(11.69)
    section.top_margin = Inches(1.18125)
    section.bottom_margin = Inches(0.9847)
    section.left_margin = Inches(0.9847)
    section.right_margin = Inches(0.9847)
    section.header_distance = Inches(0.49236)
    section.footer_distance = Inches(0.49236)
    section.different_first_page_header_footer = True

def set_style_font(style, name, size, bold=None, italic=None):
    style.font.name = name
    style._element.rPr.rFonts.set(qn("w:eastAsia"), name)
    style.font.size = Pt(size)
    style.font.color.rgb = RGBColor(0, 0, 0)
    if bold is not None:
        style.font.bold = bold
    if italic is not None:
        style.font.italic = italic

normal = doc.styles["Normal"]
set_style_font(normal, "Times New Roman", 12)
normal.paragraph_format.space_after = Pt(6)
normal.paragraph_format.line_spacing = 1.15

h1 = doc.styles["Heading 1"]
set_style_font(h1, "Arial", 14, True)
h1.paragraph_format.space_before = Pt(24)
h1.paragraph_format.space_after = Pt(8)
h1.paragraph_format.keep_with_next = True
h1.paragraph_format.keep_together = True
h2 = doc.styles["Heading 2"]
set_style_font(h2, "Arial", 13, True)
h2.paragraph_format.space_before = Pt(10)
h2.paragraph_format.space_after = Pt(5)
h2.paragraph_format.keep_with_next = True
h2.paragraph_format.keep_together = True
cap = doc.styles["Caption"]
set_style_font(cap, "Times New Roman", 10, True, False)
cap.paragraph_format.space_before = Pt(4)
cap.paragraph_format.space_after = Pt(8)
cap.paragraph_format.keep_with_next = True

# Convert the existing first-page information into the same centered hierarchy
# as the reference while retaining the author's original facts.
paras = doc.paragraphs
univ = next(p for p in paras[:12] if p.text.strip() == "Univerzita Pardubice")
title = next(p for p in paras[:12] if p.text.strip() == "Statistická analýza kosmických startů")
cover_order = [
    univ,
    next(p for p in doc.paragraphs[:12] if p.text.strip() == "Semestrální práce v R"),
    title,
    next(p for p in doc.paragraphs[:12] if p.text.strip() == "Ondřej Kodat"),
    next(p for p in doc.paragraphs[:12] if p.text.strip() == "Obor Informační technologie"),
    next(p for p in doc.paragraphs[:12] if p.text.strip() == "Navazující magisterské studium, 4. ročník"),
    next(p for p in doc.paragraphs[:12] if p.text.strip() == "Předmět NMAST"),
    next(p for p in doc.paragraphs[:12] if p.text.strip() == "2026"),
]
contents_heading = next(p for p in doc.paragraphs if p.text.strip() == "Obsah")
for p in list(doc.paragraphs[:12]):
    if p._p is not contents_heading._p:
        p._p.getparent().remove(p._p)
for p in cover_order:
    contents_heading._p.addprevious(p._p)
contents_heading.paragraph_format.page_break_before = True
cover = cover_order
for p in cover:
    p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    p.paragraph_format.left_indent = Inches(0)
    p.paragraph_format.first_line_indent = Inches(0)
    p.paragraph_format.space_after = Pt(9)
    for run in p.runs:
        run.font.name = "Arial"
        run._element.get_or_add_rPr().rFonts.set(qn("w:eastAsia"), "Arial")
        run.font.color.rgb = RGBColor(0, 0, 0)
        run.font.bold = False
        run.font.italic = False
        run.font.size = Pt(12)

univ.style = doc.styles["Normal"]
title.style = doc.styles["Normal"]
univ.runs[0].font.size = Pt(22)
univ.runs[0].font.bold = True
for run in univ.runs:
    run.font.size = Pt(22)
    run.font.bold = True
univ.paragraph_format.space_after = Pt(95)
for run in title.runs:
    run.font.size = Pt(18)
    run.font.bold = True
title.paragraph_format.space_after = Pt(40)
for border in title._p.pPr.findall(qn("w:pBdr")) if title._p.pPr is not None else []:
    title._p.pPr.remove(border)
title.paragraph_format.space_after = Pt(14)
# Make the work title the stronger line in the two-line title block.
for r in title.runs:
    if "Statistická analýza" in r.text:
        r.font.size = Pt(18)
        r.font.bold = True
    elif r.text.strip():
        r.font.size = Pt(14)
        r.font.italic = True
subtitle = cover_order[1]
for run in subtitle.runs:
    run.font.size = Pt(14)
    run.font.italic = True
year = cover_order[-1]
for run in year.runs:
    run.font.size = Pt(14)

# Number the actual chapter and subsection headings in the reference's academic
# outline style; leave the contents heading unnumbered.
chapter = 0
subcounts = {}
for p in doc.paragraphs:
    if p.style.name == "Heading 1":
        if p.text.strip() == "Obsah":
            continue
        chapter += 1
        subcounts[chapter] = 0
        if not p.text[:1].isdigit():
            p.text = f"{chapter} {p.text}"
    elif p.style.name == "Heading 2":
        subcounts[chapter] = subcounts.get(chapter, 0) + 1
        if not p.text[:1].isdigit():
            p.text = f"{chapter}.{subcounts[chapter]} {p.text}"

# Remove direct color/font choices from the report body so the reference-derived
# styles apply consistently, while preserving existing bold/italic emphasis.
for p in doc.paragraphs:
    if p in cover:
        continue
    if p.style.name in ("Heading 1", "Heading 2", "Caption"):
        continue
    for run in p.runs:
        run.font.name = "Times New Roman"
        run._element.get_or_add_rPr().rFonts.set(qn("w:eastAsia"), "Times New Roman")
        run.font.size = Pt(12)
        run.font.color.rgb = RGBColor(0, 0, 0)

# Images fit within the new A4 text width. Center them like the sample.
for shape in doc.inline_shapes:
    max_w = Inches(6.1)
    if shape.width > max_w:
        ratio = max_w / shape.width
        shape.width = max_w
        shape.height = int(shape.height * ratio)
for p in doc.paragraphs:
    if any(el.tag == qn("w:drawing") for el in p._p.iter()):
        p.alignment = WD_ALIGN_PARAGRAPH.CENTER

# Table captions remain above/below their tables as authored. Give data tables
# restrained borders and repeated bold header rows, consistent with the sample.
for table in doc.tables:
    table.style = "Table Grid"
    table.alignment = WD_TABLE_ALIGNMENT.CENTER
    table.autofit = True
    for row_i, row in enumerate(table.rows):
        if row_i == 0:
            trpr = row._tr.get_or_add_trPr()
            if trpr.find(qn("w:tblHeader")) is None:
                trpr.append(OxmlElement("w:tblHeader"))
        for cell in row.cells:
            cell.vertical_alignment = WD_CELL_VERTICAL_ALIGNMENT.CENTER
            for p in cell.paragraphs:
                p.paragraph_format.space_after = Pt(2)
                for run in p.runs:
                    run.font.name = "Times New Roman"
                    run._element.get_or_add_rPr().rFonts.set(qn("w:eastAsia"), "Times New Roman")
                    run.font.size = Pt(10)
                    run.font.color.rgb = RGBColor(0, 0, 0)
                    if row_i == 0:
                        run.font.bold = True

# Add a centered PAGE field to the regular footer. The first-page footer stays
# blank, matching the reference's distinct cover page.
footer = doc.sections[0].footer
fp = footer.paragraphs[0]
fp.alignment = WD_ALIGN_PARAGRAPH.CENTER
fp.clear()
field_run = fp.add_run()
begin = OxmlElement("w:fldChar"); begin.set(qn("w:fldCharType"), "begin")
instr = OxmlElement("w:instrText"); instr.set(qn("xml:space"), "preserve"); instr.text = " PAGE "
separate = OxmlElement("w:fldChar"); separate.set(qn("w:fldCharType"), "separate")
text = OxmlElement("w:t"); text.text = "2"
end = OxmlElement("w:fldChar"); end.set(qn("w:fldCharType"), "end")
for el in (begin, instr, separate, text, end):
    field_run._r.append(el)
field_run.font.name = "Times New Roman"
field_run.font.size = Pt(10)

doc.core_properties.title = "Statistická analýza kosmických startů"
doc.core_properties.subject = "Semestrální práce pro NMAST"
doc.save(out)
print(out)
