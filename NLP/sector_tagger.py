# ---------------------------------------------------------------------------- #
#               Sector tagger for public and non-profit employers              #
#                                                                              #
#                     (c) Nico Gießmann, MA thesis, 2023-24                    #
# ---------------------------------------------------------------------------- #

import sys
sys.path.append('..')
from helper import view_df_as_html, frequency_by_columns, frequency_by_one_column
from nltk.corpus import stopwords
import pandas as pd
from database_wrapper import *
import re

custom_stopwords = stopwords.words('german') + ['bzw', 'sowie', 'ca', 'ggf', 'ab',
                                                'incl', 'ggfs', 'z.b', 'je', 'inkl', 'u.a', 'o.g', 'zt', 'z.zt', 'usw', 'etwa', 'd.h']

REGEX_BUND = [
    r"\bbundes(?:beauftragt|rat|(?:verwaltungs|patent)gericht|bau|ministerium|tag|forschungsinstitut|institut|bank|archiv|(?:netz)?agentur|nachrichtendienst|eisenbahnvermögen|post|polizei(?:akademie)?|zentrale|finanzhof|polizeipräsidium)+(?:e|er|es|s)?\b",
    r"\bbundes(?:verwaltung|verwaltugs|kriminal|kartell|aufsicht|sorten|zentral|präsedial|präsendial|präsidial|kanzler)?s?(?:amt){1}\b",
    r"\b(?:umwelt|eisenbahn|fernstraßen|(?:luft|kraft)fahrt)+bundesamt\b",
    r"\bbundes?(?:anstalt|agentur)?\b",
    r"\brentenversicherung\b",
    r"\bthüneninstitut\b",
    r"\bkühninstitut\b",
    r"\bkochinstitut\b",
    r"\bfriedrichloefflerinstitut\b",
    r"\bgeneralzolldirektion\b",
    r"\bbund\b",
    r"\bhilfswerk\b", # <- critical to identify by one word
    r"\bwetterdienst\b", # DWD
    r"\brubnerinstitut\b",
    r"\bthüneninstitut\b",
    r"\bkochinstitut\b",
    r"\bbundespolizeiaus\b",
    r"\bgdws\b",
]

REGEX_LAND = [
    r"\blandes(?:kriminal|verwaltung|polizei|schul|untersuchung)?s?(?:amt){1}\b",
    r"\bland(?:es)?(?:rechnungshof|(?:aufnahme|straßenbau)?behörde|baudirektion|hauptkasse|wasserversorgung|stelle|forstwirtschaft|(?:kredit)?bank|forsten|(?:sozial)?gericht|beauftragt|talsperren?|tags?(?:verwaltung)?|betrieb|anstalt|direktion|rechenzentrum|labor|feuerwehr(?:schule)?|archiv|institut|talsperrenverwaltung|forstanstalt|förderzentrum|forschungsanstalt|zentrale)+(?:e|er|es|s)?\b",
    r"\bstraßennrw\b",
    r"\bministerpräsident\b",
    r"\bstaats(?:ministerium|kanzlei|archiv|bibliothek|bad)+\b",
    r"\bsenatsverwaltung\b",
    r"\b(?:sachsen|thüringen)+forst\b",
    r"\bkultusministerkonferenz\b",
    r"\bkultusministerium\b",
    r"\bakdb\b",
    r"\bbitbw\b",
    r"\bdienstleistungsdirektion\b",
    r"\bdatenverarbeitung\b",
    r"\bversorgungskammer\b",
    r"\bstaatsgemäldesammlung(?:en)?",
    r"\bdienstleistungszentrum\b",
    r"\blandesämter\b",
    # r"\bland\b",
    r"\blandesvermessung\b",
    r"\bsenator(?:in)?\b",
    r"\btierseuchenkasse\b",
    r"\blehrkräfteakademie\b",
    r"\bgymnasium\b",
    r"\blda\b", # Landesdatenschutz
    r"\brzvk\b", # Ruhegehalts- und Zusatzversorgungskasse des Saarlandes
    r"\bglücksspielbehörde\b",
    r"\bsenatsverw\b",
    r"\bsenjustv\b",
]

REGEX_KREIS_BEZIRK = [
    r"\bkreis(?:jugend|bau|sozial|forst)?(?:amt|verwaltung)?\b",
    r"\b(?:landrat)s?(?:amt)?\b",
    r"\b(?:bezirk)s?(?:amt|leitung|regierung)?\b",
    r"\b(?:burgen)?land(?:es)?(?:kreis(?:es)?|hauptstadt)\b",
    r"\bkreis(?:stadt|ausschuss)+\b",
    # r"\b.*kreis\b",
    r"\blandes(?:hauptstadt|hauptstaddt)+\b", # typo
    r"\bkreispolizeibehörde\b",
    r"\blandkreistag\b",
    r"\bba\b",
    r"\bsaarpfalzkreis\b",
]

REGEX_STADT_GEMEINDE = [
    r"\b(?:stadt)(?:amt|verwaltung|reinigung|werke)?\b",
    r"\bbezirksverband\b",
    r"\b(?:verbands?|samt)?gemeinden?(?:werke|verwaltung?)?\b",
    r"\bkommunal(?:unternehmen|betriebe?)\b",
    r"\borts?verwaltung\b",
    r"\bst(?:a|ä)+dt(?:betrieb|etag|ischer)?\b",
    r"\b(?:hanse|bundes|klingen|universitäts|luther|wallfahrts|oranien|kolping|barlach|roland|schöffer|insel|fontane|mähdrescher|widukind|viertore|welterbe|kupfer|mittel|schliemann|wissenschafts)?stadt\b",
    r"\b(?:ober)?bürgermeister(?:in)?\b",
    r"\bgemeindeverwaltungsverband\b",
    r"\bkommunen\b",
    r"\bkommunalverband\b",
    r"\bkommunal(?:e|er|en)?\b",
    r"\bstädteregion\b",
    r"\bstädtisch(?:e|es)?\b",
    r"\bkommunales\b",
    r"\b(?:orts|markt|burg)+gemeinde\b",
    # r"\bmarkt\b",
    r"\bgemeindeverband\b",
    r"\bstadtentwässerung\b",
    r"\bgemeindebund\b",
    r"\babwasserzweckverband\b",
    r"\bwasserverband\b",
    r"\bfernwasserversorgung\b",
    r"\bgrundschule\b",
    r"\bschulverband\b",
    r"\bförderschulen?\b",
    r"\bganztagesschule\b",
    r"\babwasserverband\b",
    r"\bkommunalservice\b",
    r"\bstudieninstitut\b",
    r"\bstadtwirtschaftliche\b",
    r"\bwirtschaftsbetrieb\b",
    r"\beigenbetrieb\b",
    r"\blandschaftsverband\b",
    r"\bmagistrat\b",
    r"\bverwaltungsschulverband\b",
    r"\bgemeindeunfallversicherungsverband\b",
    r"\bsis\b",
    r"\bgemeindeverwaltungsverband\b",
]

REGEX_PUBLIC_UNDEFINED = [
    r"\b(?:finanz|bau|forst|kultur|polizeiverwaltung|beschaffung|veterinäruntersuchung|marken|verwaltung|gesundheit|haupt|wasserstraßenneubau|rechnungsprüfung|information|ausländer|organisation|bürger|verbraucherschutz|bauaufsicht|straßenbau|bauordnung|oberberg|gartenbau|biosphärenreservat|gewerbeaufsicht|umweltschutz|schul|wasserwirtschaft|nationalpark|schifffahrts)?s?amt\b",
    r"\b(?:abfallzweck|entsorgung|verwaltung)?s?(?:zweck)?verband\b",
    r"\bminister(?:iums?|ial)+(?:beauftragt(?:en|er|e)+)?\b",
    r"\bbezügestellen?\b",
    r"\bstraßenbetriebe?\b",
    r"\bwirtschaftsförderung\b",
    r"\bgemeinschaftskasse\b",
    r"\bentwicklungszusammenarbeit\b",
    # r"\b(?:haupt|zentral)+verwaltung\b",
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
    r"\bpolizei(?:akademie|direktion|behörde)?\b",
    r"\bgeneraldirektion\b",
    r"\brechnungshof\b",
    r"\bforstverwaltung\b",
    # r"\bverwaltung\b",  # <- critical
    r"\bverwaltungssteuerung\b",
    r"\b(?:aufbau|förder)+bank\b",
    r"\bwsa\b",
    r"\banstalt\b" # Anstalt öffentlichen Rechts
    # r"\brechts\b",
    # r"öffentliche?", # no boundary
    r"\bkdör\b",
    r"\baör\b",
    r"\btrinkwasserversorgung\b",
    r"\bsicherheitsbereich\b",
    r"\bitdienstleistungszentrum\b",
    r"\boberfinanzdirektion\b",
    r"\bbeauftragt(?:e|er)+\b",
    r"\bdatenverarbeitungszentrum\b",
    r"^wsa.*",
    r"\berftverband\b",
    # r"\barbeit\b", # BA
    r"\bdienstleistungszentrum\b",
    r"\binnenministerium\b",
    # r"\bregion\b",
    # r"\bforst\b",
    r"\brechenzentrum\b",
    r"\bhavariekommando\b",
    r"\bitdienstleister\b",
    r"\bverkehrsverbund\b",
    r"\bitstelle\b",
    r"\bsprind\b",
]

REGEX_JUSTIZ = [
    r"\b(?:amt|sozial|verwaltung|oberlandes|oberverwaltung|verfassung)+s?gerichts?(?:bezirk|hof)?\b",
    r"\b(?:ober)?staatsanw(?:a|ä)+lt(?:in|schaft)?\b",
    r"\bjva\b",
    r"\bjustizvollzugsanstalt\b",
    r"\b(?:bundes)?sozialgericht\b",
    r"\bgeneralbundesanwalt\b",
    r"\bmaßregelvollzugszentrum\b",
    r"\bjustizvollzugsschule",
    r"\bjustizvollzugskrankenhaus\b",
]

REGEX_NONPROFIT = [
    r"\bverkehrsbetriebe\b",
    r"\b(?:handwerk|landwirtschaft)+s?kammer\b",
    r"\bstiftung\b",
    r"\b(?:berufs)?genossenschaft\b",
    r"\bsozialversicherung\b",
    r"\bverbraucherzentrale\b",
    r".*stiftung$",
    r"\bkrankenversicherung\b", # PKV kritisch
    r"\bkassenärztliche?\b",
    r"\b(?:studierenden|studenten)+werk\b",
    r"\bLandwirtschaftskammer\b",
    r"\b(?:landes)?wohlfahrtsverband\b",
    r"\bregionalverband\b",
    r"\bunfallkasse\b",
    r"\bkreishandwerkerschaft\b",
    r"\b(?:sozial|versorgung)+s?verband\b",
    r"\bstaatstheater\b",
    r".*bibliothek$",
    r"\bggmbh\b",
    r"\bversorgungsausgleichskasse\b",
    r"\bhandelskammer\b",
    r".*museum$",
    r"\b(?:bundes|landes)?ärztekammer\b",
    r"\bkindertagesstätten?\b",
    r"\bnerufsakademien?\b",
    r"\bcharité\b",
    r"\bkrankenhaus\b", # --> ambiguous
    r"\bkrankenhäuser\b",
    r"\bnotarkammer\b",
    r"\bfachschule\b",
    r"\bpflegekammer\b",
    r"\bberufsakademie\b",
    r"\bakkreditierungsstelle\b",
    r"\blandes(?:sport)?verband\b",
    r"\barbeitskammer\b",
    r"\bkreisjugendring\b",
    r"\bnationalpark\b",
    r"\btouristikverband\b",
    r"\bkunstakademie\b",
    r"\b(?:kindergarten|schul)+zweckverband\b",
    r"\bkassenzahnärztliche?\b",
    r"\bbildungsstätte\b",
    r".*sammlung$",
    r".*verein$",
    r"\bpsychotherapeutenkammer\b",
    r".*verband$",
    r"\bbürgerhilfe\b",
    r"\bgiz\b",
    r"\bgesundheitskasse\b",
    r"\bschulen?\b",
    r"\bkita\b",
    r"\bgemeinnützige?\b",
    r"\bjugendhilfe\b",
]

REGEX_CHURCH = [
    r"\b(?:landes|nord)?kirche\b",
    r"\bevangelisch(?:e|er)?\b",
    r"\bkathologisch(?:e|er)?\b",
    r"\berzdiözese\b",
    r"\b(?:ober)?kirchenrat\b",
    r"\bgeneralvikariat\b",
    r"\b(?:erz)?bischöflich(?:e|es)?\b",
    r"\bkirchenverwaltung\b",
    r"\b(?:landes|kreis|regional)?kirchenamt\b",
    r"\bkirchenkreisamt\b",
    r"\bdiakonisches\b",
    r"\bdiakonie\b",
    r"\bcaritas\b",
    r"\bev\b",
    r"\bnordkirchner\b",
    r"\bbistum\b",
    r"\bkirchlich(?:en|e)?\b",
    r"\bekd\b",
    r"\bneukirchener\b",
    r"\b(?:gesamt)?kirchengemeinde\b",
    r"\bkirchenkreis\b",
    r"\bevluth\b",
    r"\bkath\b",
]

REGEX_SCIENCE = [
    r".*universität$",
    r"\b(?:fach)*hochschule$\b",
    r"\bfh\b",
    r"\buniversitätsklinikum\b",
    r"\buniversität\b"
    r"\buniversity\b",
    r".*hochschule$",
    r"\bhaw\b",
    # r"\bgesellschaft\b",
    r".*helmholtzzentrum$",
    r"\bforschungsgemeinschaft\b",
    r"\bforschungszentrum\b",
    r"\bhelmholtz\b",
    r"\bfraunhofer\b",
    r"\bfraunhoferinstitut\b",
    r"\bhelmholtzinstitut\b",
    r"\bleibnizinstitut\b",
    r"\bth\b",
    r"\bmaxplanckgesellschaft\b",
    r"\bwissenschaft(?:en)?\b",
    r"\bmaxplanckinstitut\b",
    r"\bfraunhoferinstitutszentrum\b",
    r"\binstitut\b",
    r"\belektronensynchrotron\b",
    r"\bplanck\b",
    r"\bforschungsverbund\b",
    r"\bakademie\b",
    r"\bwissenschaftszentrum\b",
    r"\bipk\b",
    r"\blmu\b|\btu\b|\btum\b|\brwth\b|\bkit\b",
]


def check_regex_list(regex_list, text):
    for r in regex_list:
        match = re.search(r, text)
        if match is not None:
            return True
    return False


def nonprofit_tagger(text):
    '''
    This regex-based tagger is designed to tag nonprofit employers while minimizing the alpha error (false-positives).
    It matches with 27.58% employers in the Interamt dataset.
    It matches with 5.36% employers in the BA dataset.
    '''
    tags = []
    # Kirche
    if check_regex_list(REGEX_CHURCH, text):
        tags.append('CHURCH')
    # Wissenschaft
    if check_regex_list(REGEX_SCIENCE, text):
        tags.append('SCIENCE')
    # Undefined
    if check_regex_list(REGEX_NONPROFIT, text):
        tags.append('UNDEFINED_NONPROFIT')
    # return tags
    return tags

def public_tagger(text):
    '''
    This regex-based tagger is designed to tag public employers while minimizing the alpha error (false-positives).
    It matches with 78.99% employers in the Interamt dataset.
    It matches with 2.42% employers in the BA dataset.
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
    if check_regex_list(REGEX_PUBLIC_UNDEFINED, text):
        tags.append('UNDEFINED_PUBLIC')
    # return tags
    return tags

def dropNA_for_lists(list):
    return [item for item in list if item not in ['', None]]

def flatten(a):
    # https://discuss.python.org/t/why-cant-iterable-unpacking-be-used-in-comprehension/15622/9
    return [c for b in a for c in flatten(b)] if isinstance(a, list) else [a]

def find_tags(employer_name, method_to_run):
    # Cleansing
    employer_name = re.sub(r"[\(,\)]", ' ', employer_name)
    employer_name = re.sub(r"[^a-zA-ZäÄüÜöÖß ]", '', employer_name)
    employer_name_sep = employer_name.lower().split(' ')
    employer_name_sep = [
        item for item in employer_name_sep if item not in custom_stopwords]
    employer_name_sep = dropNA_for_lists(employer_name_sep)
    tags = [method_to_run(item) for item in employer_name_sep]

    return flatten(tags)

def find_tags_two_methods(employer_name, method1, method2):
    # Cleansing
    employer_name = re.sub(r"[\(,\)]", ' ', employer_name)
    employer_name = re.sub(r"[^a-zA-ZäÄüÜöÖß ]", '', employer_name)
    employer_name_sep = employer_name.lower().split(' ')
    employer_name_sep = [
        item for item in employer_name_sep if item not in custom_stopwords]
    employer_name_sep = dropNA_for_lists(employer_name_sep)
    tags_first = flatten([method1(item) for item in employer_name_sep])
    tags_second = flatten([method2(item) for item in employer_name_sep])
    tags_first.extend(tags_second)
    return tags_first

def _test_method(df, column, tagger):
    df = frequency_by_one_column(df, column)
    df['Tags'] = df.apply(lambda row: ' '.join(
        find_tags(str(row[column]), tagger)), axis=1)
    sum_employers = len(df.index)
    df = df.replace(r'^\s*$', pd.NA, regex=True)
    df = df.dropna()
    sum_found = len(df.index)

    print(f"{sum_found} of {sum_employers} ({round(sum_found / sum_employers * 100,2)}%) could be tagged.")
    return sum_found

def _get_non_matchers(df, column, tagger):
    df = frequency_by_one_column(df, column)
    df['Tags'] = df.apply(lambda row: ' '.join(
        find_tags(str(row[column]), tagger)), axis=1)
    df = df.replace(r'^\s*$', pd.NA, regex=True)
    df = df[df['Tags'].isna()]
    return df

def _get_matchers(df, column, tagger):
    df = frequency_by_one_column(df, column)
    df['Tags'] = df.apply(lambda row: ' '.join(
        find_tags(str(row[column]), tagger)), axis=1)
    df = df.replace(r'^\s*$', pd.NA, regex=True)
    df = df.dropna()
    return df

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

    # Tagging der Datasets
    employers = get_one_column(col, 'Behörde', 100)
    df = pd.DataFrame(employers)
    view_df_as_html(_get_non_matchers(df, 'Behörde', public_tagger))

    '''
    # Branchenspezifisches Tagging testen, um weiter zu verfeinern
    employers = get_one_column_filter(col, 'arbeitgeber', 'branchengruppe', 'Wissenschaft, Forschung, Entwicklung')
    df = pd.DataFrame(employers)
    _test_method(df, 'arbeitgeber', nonprofit_tagger)
    _get_non_matchers(df, 'arbeitgeber', nonprofit_tagger)
    '''