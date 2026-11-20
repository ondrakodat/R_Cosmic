from csv import DictReader
from pathlib import Path

from docx import Document
from docx.enum.table import WD_CELL_VERTICAL_ALIGNMENT
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.shared import Inches
from docx.text.paragraph import Paragraph


ROOT = Path(r"C:\Users\revel\OneDrive\Desktop\R_cosmic")
DOCX = ROOT / "Dokumentace_nova_analyza_graf.docx"
GRAPH = ROOT / "nova_analyza" / "neorbitalni_pocty_po_roku.png"
AGENCIES = ROOT / "nova_analyza" / "neorbitalni_top10_agency.csv"


def insert_paragraph_after(anchor, text="", style="Normal"):
    element = OxmlElement("w:p")
    anchor._p.addnext(element)
    paragraph = Paragraph(element, anchor._parent)
    paragraph.style = style
    if text:
        paragraph.add_run(text)
    return paragraph


def insert_paragraph_after_element(element, parent, text="", style="Normal"):
    paragraph_element = OxmlElement("w:p")
    element.addnext(paragraph_element)
    paragraph = Paragraph(paragraph_element, parent)
    paragraph.style = style
    if text:
        paragraph.add_run(text)
    return paragraph


def insert_picture_after(anchor, image_path, width):
    paragraph = insert_paragraph_after(anchor)
    paragraph.alignment = WD_ALIGN_PARAGRAPH.CENTER
    paragraph.add_run().add_picture(str(image_path), width=Inches(width))
    return paragraph


def insert_table_after(anchor, rows):
    table = doc.add_table(rows=1, cols=2)
    table.style = "Light Shading Accent 1"
    table.rows[0].cells[0].text = "Kód organizace"
    table.rows[0].cells[1].text = "Počet záznamů"
    for row in rows:
        cells = table.add_row().cells
        cells[0].text = row["Agency"]
        cells[1].text = f'{int(row["Pocet"]):,}'.replace(",", " ")
        cells[1].paragraphs[0].alignment = WD_ALIGN_PARAGRAPH.RIGHT
    for cell in table.rows[0].cells:
        for paragraph in cell.paragraphs:
            for run in paragraph.runs:
                run.bold = True
        cell.vertical_alignment = WD_CELL_VERTICAL_ALIGNMENT.CENTER
    table.rows[0]._tr.get_or_add_trPr().append(OxmlElement("w:tblHeader"))
    anchor._p.addnext(table._tbl)
    return table


doc = Document(DOCX)
paras = doc.paragraphs

# The existing contents is a manual bullet list. Add the new subsection there.
toc_anchor = next(p for p in paras if p.text == "Vymezení orbitálních a neorbitálních záznamů")
insert_paragraph_after(toc_anchor, "Doplňkový přehled neorbitálních záznamů", "List Bullet")

# Add results directly after the existing category summary and before the numeric audit.
anchor = next(p for p in doc.paragraphs if p.text.startswith("Kategorie D (hlubokovesmírné mise"))
heading = insert_paragraph_after(anchor, "Vývoj neorbitálních záznamů podle roku a organizace", "Heading 2")
annual = insert_paragraph_after(
    heading,
    "Roční přehled zahrnuje 85 let od roku 1942 do roku 2026 a jeho součet odpovídá 68 551 jasně neorbitálním záznamům. Nejvyšší počet připadá na rok 1970 (3 286 záznamů); další maxima jsou v letech 1976 (3 160) a 1969 (3 137). Jde o počty v katalogu, jehož pokrytí neorbitálních letů je méně úplné.",
)
graph = insert_picture_after(annual, GRAPH, 6.7)
caption = insert_paragraph_after(
    graph,
    "Obrázek 7. Roční počty jasně neorbitálních záznamů v katalogu GCAT, 1942–2026.",
    "Caption",
)
agency_note = insert_paragraph_after(
    caption,
    "Mezi záznamy s vyplněným kódem Agency mají nejvyšší četnost MRN (23 478), AN (8 156) a RVSN (3 396). Kódy označují evidované organizace a jejich četnosti nelze bez ověření slučovat napříč historickými obdobími.",
)
with AGENCIES.open(encoding="utf-8-sig", newline="") as f:
    agency_rows = list(DictReader(f))
table = insert_table_after(agency_note, agency_rows)
table_caption = insert_paragraph_after_element(
    table._tbl,
    doc,
    "Tabulka 3. Deset nejčetnějších kódů Agency u jasně neorbitálních záznamů.",
    "Caption",
)
insert_paragraph_after(
    table_caption,
    "Roční řadu ani přehled organizací neslučujeme s orbitálními počty. Doplňkový výsledek slouží k popisu katalogu a jeho omezené úplnosti.",
)

# Keep the written method and conclusion aligned with the new outputs.
for p in doc.paragraphs:
    if p.text.startswith("7. Doplňková neorbitální část"):
        p.text = (
            "7. Doplňková neorbitální část  Jasně neorbitální kategorie jsme shrnuli podle roku, "
            "kategorie a kódů organizací. Hlubokovesmírné mise, starty z jiných těles, "
            "near-orbitální záznamy a záznamy, které nejsou startem, jsme vykázali zvlášť."
        )
    if p.text.startswith("Neorbitální záznamy jsme proto vyhodnotili odděleně."):
        p.text = (
            "Neorbitální záznamy jsme proto vyhodnotili odděleně. Jejich roční počet "
            "dosáhl maxima 3 286 v roce 1970; nejčetnější evidované kódy Agency byly MRN, "
            "AN a RVSN. Analýza popisuje souvislosti v katalogu, ne příčinné vztahy ani "
            "spolehlivou předpověď budoucích startů."
        )

doc.save(DOCX)
print(f"Updated {DOCX}")
