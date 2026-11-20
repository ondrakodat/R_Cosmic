from pathlib import Path
import re

from docx import Document
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.text.paragraph import Paragraph
from docx.shared import Pt, RGBColor

ROOT = Path(r"C:\Users\revel\OneDrive\Desktop\R_cosmic")
SOURCE = ROOT / "Dokumentace_nova_analyza_graf_podle_vzoru.docx"
OUTPUT = ROOT / "Dokumentace_nova_analyza_graf_podle_osnovy_pocasi.docx"

doc = Document(SOURCE)
body = doc._element.body

def paragraph_for(element):
    return Paragraph(element, doc._body)

def clean_heading(text):
    return re.sub(r"^\s*\d+(?:\.\d+)*\s+", "", text).strip()

def heading_info(element):
    if element.tag != qn("w:p"):
        return None
    p = paragraph_for(element)
    if p.style.name not in ("Heading 1", "Heading 2"):
        return None
    return p.style.name, clean_heading(p.text)

# Split the existing report into intact content blocks at every existing heading.
elements = list(body)
first_content_idx = next(
    i for i, el in enumerate(elements)
    if (info := heading_info(el)) and info == ("Heading 1", "Cíl práce")
)
prefix = elements[:first_content_idx]
content = elements[first_content_idx:-1]  # Keep the section properties as the last body child.
units = {}
active = []
for el in content:
    info = heading_info(el)
    if info and active:
        head_info = heading_info(active[0])
        units[head_info[1]] = active
        active = []
    active.append(el)
if active:
    head_info = heading_info(active[0])
    units[head_info[1]] = active

expected = {
    "Cíl práce", "Datové soubory a proměnné", "Ověřený výsledek první kontroly",
    "Ověřený výsledek druhého kroku", "Časový graf ročních počtů",
    "Vývoj startů podle pěti nejčastějších organizací", "Kódy výsledků orbitálních startů",
    "Vypočtená úspěšnost orbitálních startů", "Graf vážené úspěšnosti pěti organizací",
    "Hmotnost nákladu dopraveného na oběžnou dráhu", "Rozdělení hmotnosti nákladu",
    "Náklad podle typu rakety", "Test rozdílu průměrného OrbPay mezi obdobími",
    "Welchova ANOVA podle typu rakety", "Korelace roku s ročním počtem orbitálních startů",
    "Lineární a kvadratická regrese ročních počtů", "Doplňkový přehled neorbitálních startů",
    "Vývoj neorbitálních záznamů podle roku a organizace", "Audit dalších číselných proměnných",
    "Popisné statistiky dalších číselných proměnných", "Pokrytí údajů podle desetiletí",
    "Metodika zpracované analýzy", "Výzkumná otázka a omezení", "Testované otázky",
    "Analýza rozptylu dalších číselných proměnných", "Původ dat a omezení", "Zdroje", "Závěr",
}
missing = expected - set(units)
if missing:
    raise RuntimeError(f"Could not identify existing sections: {sorted(missing)}")

def unit(key, title):
    els = units[key]
    p = paragraph_for(els[0])
    p.style = doc.styles["Heading 2"]
    p.text = title
    return els

def section(title, entries):
    # entries is a list of intact paragraph/table/image blocks, each headed with a Heading 2.
    return (title, entries)

# Move the combined regression output into separate linear and nonlinear chapters.
regression = units["Lineární a kvadratická regrese ročních počtů"]
regression_elements = regression[1:]
linear_elements, nonlinear_elements = [], []
nonlinear_started = False
for el in regression_elements:
    if el.tag == qn("w:p"):
        p = paragraph_for(el)
        text = p.text.strip()
        if text.startswith("Kvadratický model") or text.startswith("Obrázek 6."):
            nonlinear_started = True
        if any(child.tag == qn("w:drawing") for child in el.iter()):
            nonlinear_started = True
        if nonlinear_started:
            nonlinear_elements.append(el)
        else:
            linear_elements.append(el)
    elif nonlinear_started:
        nonlinear_elements.append(el)
    else:
        linear_elements.append(el)

linear_heading = paragraph_for(regression[0])
linear_heading.style = doc.styles["Heading 2"]
linear_heading.text = "Lineární model ročních počtů"
linear_unit = [regression[0], *linear_elements]
nonlinear_unit = [make_heading for make_heading in []]  # initialized below

def new_paragraph(text, style_name="Normal", alignment=None):
    p = Paragraph(OxmlElement("w:p"), doc._body)
    p.style = doc.styles[style_name]
    if alignment is not None:
        p.alignment = alignment
    if text:
        p.add_run(text)
    return p

nonlinear_heading = new_paragraph("Kvadratický model ročních počtů", "Heading 2")
nonlinear_unit = [nonlinear_heading._p, *nonlinear_elements]

# Move the stand-alone question into the introduction, while source provenance and
# methodological caveats stay with the data chapter.
research = units["Výzkumná otázka a omezení"]
research_paras = [paragraph_for(el) for el in research[1:] if el.tag == qn("w:p") and paragraph_for(el).text.strip()]
if research_paras:
    research_heading = paragraph_for(research[0])
    research_heading.style = doc.styles["Heading 2"]
    research_heading.text = "Výzkumná otázka"
    # Keep the research question in this block; move the catalogue caveat to Data.
    moved_limitations = []
    for el in research[1:]:
        if el.tag == qn("w:p") and paragraph_for(el).text.strip().startswith("Katalog je průběžně doplňovaný"):
            moved_limitations.append(el)
    research_unit = [research[0], *[el for el in research[1:] if el not in moved_limitations]]
    limitations_heading = new_paragraph("Omezení katalogu", "Heading 2")
    limitations_unit = [limitations_heading._p, *moved_limitations]
else:
    research_unit = unit("Výzkumná otázka a omezení", "Výzkumná otázka")
    limitations_unit = [new_paragraph("Omezení katalogu", "Heading 2")._p]

# Put the definition-source line beside the reference list, not among conclusions.
tested = units["Testované otázky"]
source_definition = []
tested_body = []
for el in tested[1:]:
    if el.tag == qn("w:p") and paragraph_for(el).text.strip().startswith("Zdroj definic proměnných:"):
        source_definition.append(el)
    else:
        tested_body.append(el)
tested_unit = [tested[0], *tested_body]
paragraph_for(tested[0]).style = doc.styles["Heading 2"]
paragraph_for(tested[0]).text = "Souhrn hlavních výsledků"

source_unit = units["Zdroje"]
source_unit.extend(source_definition)

# Build the target outline in the same order as the weather paper.
chapters = [
    section("Úvod", [
        unit("Cíl práce", "Cíl práce"),
        research_unit,
    ]),
    section("Původ dat a metoda měření", [
        unit("Datové soubory a proměnné", "Zdroj dat a proměnné"),
        unit("Ověřený výsledek první kontroly", "Kontrola dat a výběr orbitálních záznamů"),
        unit("Ověřený výsledek druhého kroku", "Počty podle roku, organizace a typu rakety"),
        unit("Metodika zpracované analýzy", "Postup zpracované analýzy"),
        unit("Původ dat a omezení", "Původ dat a omezení katalogu"),
        unit("Zdroje", "Zdroje"),
        limitations_unit,
    ]),
    section("Popisná statistika", [
        unit("Kódy výsledků orbitálních startů", "Kategorie LaunchCode"),
        unit("Vypočtená úspěšnost orbitálních startů", "Úspěšnost orbitálních startů"),
        unit("Hmotnost nákladu dopraveného na oběžnou dráhu", "Popisná statistika nákladu OrbPay"),
        unit("Doplňkový přehled neorbitálních startů", "Doplňkový souhrn neorbitálních kategorií"),
        unit("Audit dalších číselných proměnných", "Další číselné proměnné"),
        unit("Popisné statistiky dalších číselných proměnných", "Popisné statistiky dalších proměnných"),
        unit("Pokrytí údajů podle desetiletí", "Pokrytí údajů podle desetiletí"),
        (2, [new_paragraph("Grafy", "Heading 2")._p]),
        (3, unit("Časový graf ročních počtů", "Roční počty orbitálních startů")),
        (3, unit("Vývoj startů podle pěti nejčastějších organizací", "Starty podle nejčastějších organizací")),
        (3, unit("Graf vážené úspěšnosti pěti organizací", "Vážená úspěšnost organizací")),
        (3, unit("Rozdělení hmotnosti nákladu", "Rozdělení hmotnosti nákladu")),
        (3, unit("Náklad podle typu rakety", "Náklad podle typu rakety")),
        (3, unit("Vývoj neorbitálních záznamů podle roku a organizace", "Neorbitální záznamy podle roku a organizace")),
    ]),
    section("Testování hypotéz", [
        unit("Test rozdílu průměrného OrbPay mezi obdobími", "Welchův dvouvýběrový t-test OrbPay"),
    ]),
    section("ANOVA a vícenásobné porovnání", [
        unit("Welchova ANOVA podle typu rakety", "Welchova ANOVA podle typu rakety"),
        unit("Analýza rozptylu dalších číselných proměnných", "ANOVA dalších proměnných podle desetiletí"),
    ]),
    section("Korelační analýza", [
        unit("Korelace roku s ročním počtem orbitálních startů", "Rok a roční počet orbitálních startů"),
    ]),
    section("Lineární regrese", [linear_unit]),
    section("Nelineární regrese", [nonlinear_unit]),
    section("Závěr a komentář k výsledkům", [
        tested_unit,
        unit("Závěr", "Závěr"),
    ]),
]

# Update the existing manual contents list and append the run-guide entry.
contents_heading = next(
    paragraph_for(el) for el in prefix
    if (info := heading_info(el)) and info == ("Heading 1", "Obsah")
)
contents_paras = []
first_body_heading = paragraph_for(content[0])
found_contents = False
for el in prefix:
    if el is contents_heading._p:
        found_contents = True
        continue
    if found_contents and el.tag == qn("w:p"):
        p = paragraph_for(el)
        if p.style.name == "List Bullet":
            contents_paras.append(p)
        elif p.text.strip() and p.style.name != "Normal":
            break
chapter_names = [title for title, _ in chapters] + ["Návod na spuštění"]
for i, title in enumerate(chapter_names):
    if i < len(contents_paras):
        contents_paras[i].text = title
    else:
        bullet = new_paragraph(title, "List Bullet")
        # Insert the additional list item immediately before the first chapter.
        first_body_heading._p.addprevious(bullet._p)
if len(contents_paras) > len(chapter_names):
    for p in contents_paras[len(chapter_names):]:
        p._p.getparent().remove(p._p)

# Remove the old main-body XML and rebuild it in the new order, leaving the cover,
# contents, and final section properties in place.
for el in content:
    body.remove(el)
for el in prefix:
    # A new contents bullet may already have been inserted before the first body heading.
    pass

append_point = body.sectPr

def insert_before_sect(element):
    append_point.addprevious(element)

chapter_index = 0
for chapter_title, entries in chapters:
    chapter_index += 1
    h = new_paragraph(f"{chapter_index} {chapter_title}", "Heading 1")
    if chapter_index == 1:
        h.paragraph_format.page_break_before = True
    insert_before_sect(h._p)
    sub_index = 0
    nested_counts = {}
    for entry_spec in entries:
        level, entry = entry_spec if isinstance(entry_spec, tuple) and isinstance(entry_spec[0], int) else (2, entry_spec)
        if not entry:
            continue
        if level == 2:
            sub_index += 1
            nested_counts[sub_index] = 0
        else:
            nested_counts[sub_index] = nested_counts.get(sub_index, 0) + 1
        head_el = entry[0]
        head = paragraph_for(head_el)
        sub_title = clean_heading(head.text)
        if head._p.pPr is not None:
            old_page_break = head._p.pPr.find(qn("w:pageBreakBefore"))
            if old_page_break is not None:
                head._p.pPr.remove(old_page_break)
        head.style = doc.styles[f"Heading {level}"]
        if level == 2:
            head.text = f"{chapter_index}.{sub_index} {sub_title}"
        else:
            head.text = f"{chapter_index}.{sub_index}.{nested_counts[sub_index]} {sub_title}"
        for el in entry:
            insert_before_sect(el)

# Add the run guide last, matching the reference's final practical section.
chapter_index += 1
guide_heading = new_paragraph(f"{chapter_index} Návod na spuštění", "Heading 1")
insert_before_sect(guide_heading._p)
guide_paras = [
    "Analýzu lze znovu spustit v programu RGui pomocí skriptu postup_novy.R. Ve složce projektu musí být také zdrojový soubor launch_data_original.csv. Skript načte data a vytvoří výstupní grafy a tabulky v podadresáři nova_analyza.",
    "V RGui otevřete postup_novy.R a zvolte Source pro spuštění celého skriptu. Pokud projekt přesunete do jiné složky, upravte na začátku skriptu příkaz setwd() na adresář, který obsahuje soubor launch_data_original.csv.",
]
for text in guide_paras:
    p = new_paragraph(text, "Normal")
    insert_before_sect(p._p)

# Captions are numbered in their new reading order.
figure_no = table_no = 0
for p in doc.paragraphs:
    if p.style.name != "Caption":
        continue
    if re.match(r"^Obrázek\s+\d+[.:]", p.text):
        figure_no += 1
        p.text = re.sub(r"^Obrázek\s+\d+[.:]", f"Obrázek {figure_no}.", p.text)
    elif re.match(r"^Tabulka\s+\d+[.:]", p.text):
        table_no += 1
        p.text = re.sub(r"^Tabulka\s+\d+[.:]", f"Tabulka {table_no}.", p.text)

# Let ordinary prose flow across pages. Keep only chapter-level starts forced;
# keep a picture paragraph with its following caption, and keep table rows intact.
for p in doc.paragraphs:
    if p.style.name != "Normal" or p._p.pPr is None:
        continue
    ppr = p._p.pPr
    page_break = ppr.find(qn("w:pageBreakBefore"))
    if page_break is not None:
        ppr.remove(page_break)
    has_drawing = any(child.tag == qn("w:drawing") for child in p._p.iter())
    if not has_drawing:
        for tag in ("w:keepNext", "w:keepLines"):
            prop = ppr.find(qn(tag))
            if prop is not None:
                ppr.remove(prop)

doc.core_properties.title = "Statistická analýza kosmických startů"
doc.core_properties.subject = "Semestrální práce pro NMAST"
h3_style = doc.styles["Heading 3"]
h3_style.font.name = "Arial"
h3_style._element.rPr.rFonts.set(qn("w:eastAsia"), "Arial")
h3_style.font.size = Pt(12)
h3_style.font.bold = True
h3_style.font.color.rgb = RGBColor(0, 0, 0)
h3_style.paragraph_format.space_before = Pt(8)
h3_style.paragraph_format.space_after = Pt(4)
h3_style.paragraph_format.keep_with_next = True
doc.save(OUTPUT)
print(OUTPUT)
