import sys
sys.path.append('..')
from preprocessor import view_df_as_html, frequency_by_columns
from nltk.corpus import stopwords
import pandas as pd
from database_wrapper import *
import re

custom_stopwords = stopwords.words('german') + ['bzw', 'sowie', 'ca', 'ggf', 'ab',
                                                'incl', 'ggfs', 'z.b', 'je', 'inkl', 'u.a', 'o.g', 'zt', 'z.zt', 'usw', 'etwa', 'd.h']

REGEX_BUND = [
    r"\bbundes(?:beauftragt|rat|(?:verwaltungs|patent)gericht|bau|ministerium|tag|forschungsinstitut|institut|bank|archiv|netzagentur|nachrichtendienst|eisenbahnvermögen|post)+(?:e|er|es|s)?\b",
    r"\bbundes(?:verwaltung|kriminal|kartell|aufsicht|sorten|zentral|präsedial|kanzler)?s?(?:amt){1}\b",
    r"\b(?:umwelt|eisenbahn|fernstraßen|(?:luft|kraft)fahrt)+bundesamt\b",
    r"\bbundes?(?:anstalt|agentur)?\b",
    r"\brentenversicherung\b",
    r"\bthüneninstitut\b",
    r"\bkühninstitut\b",
    r"\bkochinstitut\b",
    r"\bfriedrichloefflerinstitut\b",
    r"\bgeneralzolldirektion\b",
    r"\bbund\b",
    r"\bhilfswerk\b",  # <- critical to identify by one word
]

REGEX_LAND = [
    r"\blandes(?:kriminal|verwaltung|polizei|schul|untersuchung)?s?(?:amt){1}\b",
    r"\bland(?:es)?(?:rechnungshof|(?:aufnahme|straßenbau)?behörde|sportverband|baudirektion|hauptkasse|wasserversorgung|stelle|forstwirtschaft|(?:kredit)?bank|forsten|(?:sozial)?gericht|beauftragt|(?:talsperren|tags)+(?:verwaltung)?|betrieb|anstalt)+(?:e|er|es|s)?\b",
    r"\bstraßennrw\b",
    r"\bministerpräsident\b",
    r"\bstaats(?:ministerium|kanzlei|archiv|bibliothek)+\b",
    r"\bsenatsverwaltung\b",
    r"\b(?:sachsen|thüringen)+forst\b",
    r"\bkultusministerkonferenz\b",
    r"\bkultusministerium\b",
    r"\bakdb\b",
    r"\bbitbw\b",
]

REGEX_KREIS_BEZIRK = [
    r"\bkreis(?:jugend|bau|sozial|forst)?(?:amt|verwaltung)?\b",
    r"\b(?:landrat)s?(?:amt)?\b",
    r"\b(?:bezirk)s?(?:amt|leitung|regierung)?\b",
    r"\b(?:burgen)?land(?:es)?(?:kreis(?:es)?|hauptstadt)\b",
    r"\bkreis(?:stadt|ausschuss)\b",
    r"\b.*kreis\b",
]

REGEX_STADT_GEMEINDE = [
    r"\b(?:stadt)(?:amt|verwaltung|reinigung)?\b",
    r"\bbezirksverband\b",
    r"\b(?:verbands?|samt)?gemeinden?(?:werke|verwaltung?)?\b",
    r"\bkommunal(?:unternehmen|betriebe?)\b",
    r"\borts?verwaltung\b",
    r"\bst(?:a|ä)+dt(?:betrieb|etag|ischer)?\b",
    r"\b(?:hanse|bundes|klingen|universitäts|luther)?stadt\b",
    r"\b(?:ober)?bürgermeister(?:in)?\b",
    r"\bgemeindeverwaltungsverband\b",
    r"\bkommunen\b",
    r"\bkommunalverband\b",
    r"\bstädteregion\b",
    r"\bstädtisch(?:e|es)\b",
]

REGEX_UNDEFINED = [
    r"\b(?:finanz|bau|forst|kultur|polizeiverwaltung|beschaffung|veterinäruntersuchung|marken|verwaltung|gesundheit|haupt|wasserstraßenneubau|rechnungsprüfung|information|ausländer|organisation|bürger|verbraucherschutz|bauaufsicht|straßenbau|bauordnung|oberberg|gartenbau|biosphärenreservat|gewerbeaufsicht|umweltschutz|schul)?s?amt\b",
    r"\b(?:abfallzweck|entsorgung|verwaltung)?s?(?:zweck)?verband\b",
    r"\bminister(?:iums?|ial)+(?:beauftragt(?:en|er|e)+)?\b",
    r"\bbezügestellen?\b",
    r"\bstraßenbetriebe?\b",
    r"\bwirtschaftsförderung\b",
    r"\bgemeinschaftskasse\b",
    r"\bentwicklungszusammenarbeit\b",
    r"\b(?:haupt|zentral)+verwaltung\b",
    r"\bpolizei(?:inspektion|präsidium)?\b",
    r"\b(?:kontroll|ausländer)?behörde\b",
    r"\bdatenverarbeitungszentrale\b",
    r"\bverwaltungsdienststelle\b",
    r"\bgeobasisinformation\b",
    r"\bbürgerdienste\b",
    r"\bregionaldirektion\b",
    r"\bstaatliche(?:n|s)?\b",
    r"\binformationstechnikzentrum\b",
    r"\bregierungs?(?:präsidium)?\b",
    r"\bverfassungsschutz\b",
    r"\bfinanzministerium\b",
    r"\bjobcenter\b",
    r"\bstaatsbetrieb\b",
    r"\bbereitschaftspolizei\b",
    r"\bitdienstleistungszentrum\b",
    r"\bverwaltungsgemeinschaft\b",
    r"\bpolizeiakademie\b",
    r"\bgeneraldirektion\b",
    r"\brechnungshof\b",
    r"\bforstverwaltung\b",
    r"\bverwaltung\b",  # <- critical
    r"\bverwaltungssteuerung\b",
    r"\b(?:aufbau|förder)+bank\b",
    r"wsa",
]

REGEX_JUSTIZ = [
    r"\b(?:amt|sozial|verwaltung|oberlandes|oberverwaltung|verfassung)+s?gerichts?(?:bezirk|hof)?\b",
    r"\b(?:ober)?staatsanw(?:a|ä)+lt(?:in|schaft)?\b",
]


def check_regex_list(regex_list, text):
    for r in regex_list:
        match = re.search(r, text)
        if match is not None:
            return True
    return False


def public_tagger(text):
    '''
    This regex-based tagger is designed to minimize the alpha error (false-positives).
    It matches with 73.88% employers in the Interamt dataset.
    '''
    tags = []
    # Bund
    if check_regex_list(REGEX_BUND, text):
        tags.append('BUND')
    # Land
    if check_regex_list(REGEX_LAND, text):
        tags.append('LAND')
    # Kreis/Bezirk
    if check_regex_list(REGEX_KREIS_BEZIRK, text):
        tags.append('KREIS_BEZIRK')
    # Stadt/Gemeinde
    if check_regex_list(REGEX_STADT_GEMEINDE, text):
        tags.append('STADT_GEMEINDE')
    # Justiz
    if check_regex_list(REGEX_JUSTIZ, text):
        tags.append('JUSTIZ')
    # Undefined
    if check_regex_list(REGEX_UNDEFINED, text):
        tags.append('UNDEFINED_PUBLIC')
    return tags


def dropNA_for_lists(list):
    return [item for item in list if item not in ['', None]]


def flatten(a):
    # https://discuss.python.org/t/why-cant-iterable-unpacking-be-used-in-comprehension/15622/9
    return [c for b in a for c in flatten(b)] if isinstance(a, list) else [a]


def find_tags(employer_name, method_to_run):
    # Cleansing
    employer_name_sep = employer_name.lower().split(' ')
    employer_name_sep = [re.sub(r"[^a-zA-ZäÄüÜöÖß]", '', item)
                         for item in employer_name_sep]
    employer_name_sep = [
        item for item in employer_name_sep if item not in custom_stopwords]
    employer_name_sep = dropNA_for_lists(employer_name_sep)
    tags = [method_to_run(item) for item in employer_name_sep]

    return flatten(tags)


def test_match_method(interamt_col):
    employers = get_one_column(interamt_col, 'Behörde')

    df = pd.DataFrame(employers)
    df['Tags'] = df.apply(lambda row: ' '.join(
        find_tags(str(row['Behörde']), public_tagger)), axis=1)
    df = frequency_by_columns(df, 'Behörde', 'Tags')
    sum_employers = len(df.index)
    df = df.replace(r'^\s*$', pd.NA, regex=True)
    df = df.dropna()
    sum_found = len(df.index)

    print(f"{sum_found} of {sum_employers} ({round(sum_found / sum_employers * 100,2)}%) could be tagged as public.")


if __name__ == '__main__':
    try:
        db = mongo_authenticate('../')
        cols = db.list_collection_names()
        print('Connection working:', cols)
    except Exception as e:
        print('Connection not working.')
        print(e)
        exit(1)

    col = db['jobads']

    test_match_method(col)
