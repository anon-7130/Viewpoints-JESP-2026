import pandas as pd
import os
import re
import json
import openpyxl

ATTENTION_CHECKS = {'AC-FL': 1, 'AC-C': 3, 'AC-FR': 5} # The title states the required answer
LITERACY_ANSWERS = {'veto': '2/3', 'members': 'The Republican Party', 'conservative': 'The Republican Party'}
VP_CORRECT_RE = re.compile(r'\bvice[\s.\-]*pres|\bv\.?\s*p\.?\b', re.IGNORECASE)
ITEM_TITLE_RE = re.compile(r'^(?P<family>[A-Z]+)(?:-(?P<number>\d+))?-(?P<variant>[A-Z]+)$')
MAX_LITERACY_FAILS = 1 # Any attention check failure excludes, but one literacy slip is ordinary

CLEAN_COLUMNS = [
    'session_id', 'participant_id',
    'group_id', 'group_title', 'group_order',
    'component_id', 'component_title', 'component_order',
    'item_code', 'item_family', 'item_number', 'item_variant',
    'target_category', 'stimulus_version', 'topic',
    'response_type', 'measure', 'question_text',
    'response_value', 'response_label', 'response_text', 'response_raw',
    'scale_n_points', 'scale_labels', 'choice_options',
    'is_attention_check', 'attention_check_expected', 'attention_check_passed',
    'is_duplicate_response', 'time_taken_ms', 'submitted_at', 'image_url',
    'screening_passed', 'n_screening_failed', 'screening_failed',
]


def parse_item_title(title: str) -> dict:
    m = ITEM_TITLE_RE.match(title.strip())
    if not m:
        return {'family': '', 'number': pd.NA, 'variant': '', 'code': ''}

    family, number, variant = m.group('family'), m.group('number'), m.group('variant')

    return {
        'family': family,
        'number': int(number) if number else pd.NA,
        'variant': variant,
        'code': f'{family}-{number}' if number else family
    }


def build_component_map(config: dict) -> pd.DataFrame:
    rows = []

    for group in config['groups']:
        for comp in group['components']:
            cfg = comp.get('responseConfig') or {}
            labels = [lab['text'] for lab in cfg.get('labels', [])]
            questions = [q['text'] for q in cfg.get('questions', [])]
            options = [opt['text'] for opt in cfg.get('options', [])]
            parsed = parse_item_title(comp['title'])

            rows.append({
                'component_id': comp['id'],
                'component_title': comp['title'],
                'component_order': comp['componentOrder'],
                'group_id': group['id'],
                'group_title': group['title'],
                'group_order': group['groupOrder'],
                'response_type': comp['responseType'],
                'measure': comp['measure'],
                'instruction': comp.get('instruction') or '',
                'image_url': comp.get('imageUrl') or '',
                'requires_response': bool(cfg.get('requireResponse', False)),
                'scale_n_points': cfg.get('numberOfPoints'),
                'scale_labels': '|'.join(labels),
                'question_text': questions[0] if len(questions) == 1 else '|'.join(questions),
                'choice_options': '|'.join(options),
                'item_family': parsed['family'],
                'item_number': parsed['number'],
                'item_variant': parsed['variant'],
                'item_code': parsed['code'],
                'is_attention_check': comp['title'] in ATTENTION_CHECKS,
                'attention_check_expected': ATTENTION_CHECKS.get(comp['title'])
            })

    return pd.DataFrame(rows)


def add_stimulus_metadata(component_map: pd.DataFrame, stimuli_path: str, sheet: str) -> pd.DataFrame:
    workbook = openpyxl.load_workbook(stimuli_path, read_only = True, data_only = True) # Not pd.read_excel, which needs a newer openpyxl
    rows = list(workbook[sheet].iter_rows(values_only = True))
    workbook.close()

    stimuli = pd.DataFrame(rows[1:], columns = rows[0]).dropna(how = 'all')
    stimuli = stimuli[['Stimulus ID', 'Target category', 'Version', 'Topic']]
    stimuli.columns = ['component_title', 'target_category', 'stimulus_version', 'topic']
    stimuli['component_title'] = stimuli['component_title'].astype(str).str.strip()
    stimuli = stimuli.drop_duplicates('component_title')

    # Component titles repeat across groups (e.g., "Eye-tracking setup"), but stimulus titles are unique, so no component gets two categories
    component_map = component_map.merge(stimuli, on = 'component_title', how = 'left', validate = 'many_to_one')

    unused = sorted(set(stimuli['component_title']) - set(component_map['component_title']))
    print(f'Stimuli in {os.path.basename(stimuli_path)}: {len(stimuli)} ({component_map["target_category"].notna().sum()} used in this study)')
    print(f'Sheet stimuli not used in this study: {", ".join(unused)}')

    return component_map


def decode_response(raw: str, response_type: str) -> tuple:
    if pd.isna(raw):
        return pd.NA, pd.NA

    try:
        parsed = json.loads(raw)
    except (json.JSONDecodeError, TypeError):
        return pd.NA, str(raw)

    if response_type == 'LIKERT_GRID':
        return next(iter(parsed.values())), pd.NA # Every grid in this study holds exactly one question

    return pd.NA, parsed if isinstance(parsed, str) else json.dumps(parsed)


def load_responses(path: str, component_map: pd.DataFrame) -> pd.DataFrame:
    df = pd.read_csv(path, dtype = str)
    df = df.rename(columns = {
        'Session ID': 'session_id',
        'Participant ID': 'participant_id',
        'Component ID': 'component_id',
        'Response': 'response_raw',
        'Time Taken (ms)': 'time_taken_ms',
        'Submitted At': 'submitted_at'
    })
    df = df.merge(component_map, on = 'component_id', how = 'left', validate = 'many_to_one')

    decoded = [decode_response(r, t) for r, t in zip(df['response_raw'], df['response_type'])]
    df['response_value'] = pd.array([d[0] for d in decoded], dtype = 'Int64')
    df['response_text'] = [d[1] for d in decoded]

    # Attach the scale label (e.g., "Far left") to each Likert answer
    likert_scales = component_map.loc[component_map['response_type'] == 'LIKERT_GRID', 'scale_labels'].unique()
    scale_map = pd.DataFrame([
        {'scale_labels': labels, 'response_value': i, 'response_label': text}
        for labels in likert_scales if labels
        for i, text in enumerate(labels.split('|'), start = 1)
    ])
    df = df.merge(scale_map, on = ['scale_labels', 'response_value'], how = 'left')

    df['time_taken_ms'] = pd.to_numeric(df['time_taken_ms'], errors = 'coerce').astype('Int64')
    df['submitted_at'] = pd.to_datetime(df['submitted_at'], format = 'ISO8601', utc = True)

    print(f'Responses: {len(df)} from {df["participant_id"].nunique()} participants')

    return df.sort_values(['participant_id', 'submitted_at']).reset_index(drop = True)


def screening(df: pd.DataFrame) -> pd.DataFrame:
    participants = pd.Index(sorted(df['participant_id'].unique()), name = 'participant_id')
    titles = df['component_title'].str.lower()
    screen = pd.DataFrame(index = participants)

    def answers(title: str, column: str) -> pd.Series:
        rows = df[titles == title.lower()]
        return rows.drop_duplicates('participant_id', keep = 'last').set_index('participant_id')[column].reindex(participants)

    # A missing answer stays NA, so it counts as a failure below but is labelled separately
    for title, expected in ATTENTION_CHECKS.items():
        got = answers(title, 'response_value')
        screen[title] = got.eq(expected).where(got.notna())

    got = answers('vp', 'response_text')
    screen['vp'] = got.astype('string').str.contains(VP_CORRECT_RE).where(got.notna())

    for title, expected in LITERACY_ANSWERS.items():
        got = answers(title, 'response_text')
        screen[title] = got.astype('string').str.strip().eq(expected).where(got.notna())

    ac_titles = list(ATTENTION_CHECKS)
    literacy_titles = ['vp'] + list(LITERACY_ANSWERS)
    failed = screen.eq(False) | screen.isna()

    out = screen.add_prefix('screen_')
    out['n_ac_failed'] = failed[ac_titles].sum(axis = 1)
    out['n_literacy_failed'] = failed[literacy_titles].sum(axis = 1)
    out['screening_failed'] = [
        ';'.join(f'{title}:missing' if pd.isna(value) else f'{title}:wrong' for title, value in row.items() if pd.isna(value) or not value)
        for _, row in screen.iterrows()
    ]
    out['n_screening_failed'] = out['n_ac_failed'] + out['n_literacy_failed']
    out['screening_passed'] = (out['n_ac_failed'] == 0) & (out['n_literacy_failed'] <= MAX_LITERACY_FAILS)

    # Only participants who reached the Likert block; the many consent-only dropouts would otherwise fill every count with "missing"
    reached = out[out.index.isin(df.loc[df['response_type'] == 'LIKERT_GRID', 'participant_id'])]
    print(f'Screening among the {len(reached)} participants who reached the Likert block:')
    print(pd.DataFrame({
        'pass': reached[[f'screen_{t}' for t in ac_titles + literacy_titles]].eq(True).sum(),
        'fail': reached[[f'screen_{t}' for t in ac_titles + literacy_titles]].eq(False).sum(),
        'not answered': reached[[f'screen_{t}' for t in ac_titles + literacy_titles]].isna().sum()
    }))
    print('Participants removed due to a failed attention check:')
    print((reached['n_ac_failed'] > 0).sum())
    print(f'Participants removed due to failing more than {MAX_LITERACY_FAILS} literacy item(s):')
    print(((reached['n_ac_failed'] == 0) & (reached['n_literacy_failed'] > MAX_LITERACY_FAILS)).sum())

    return out.reset_index()


def exclude_participants(df: pd.DataFrame, screen: pd.DataFrame) -> pd.DataFrame:
    passed = screen.loc[screen['screening_passed'], 'participant_id']
    df = df[df['participant_id'].isin(passed)].copy()

    # Some participants answered the same component twice; the last answer is kept
    key = ['participant_id', 'component_id']
    df['is_duplicate_response'] = df.duplicated(key, keep = False)
    n_before = len(df)
    df = df.drop_duplicates(key, keep = 'last')

    print('Repeated responses removed:')
    print(n_before - len(df))

    df = df.merge(screen[['participant_id', 'screening_passed', 'n_screening_failed', 'screening_failed']], on = 'participant_id', how = 'left')
    df['attention_check_passed'] = pd.NA
    is_check = df['is_attention_check'].fillna(False)
    df.loc[is_check, 'attention_check_passed'] = df.loc[is_check, 'response_value'] == df.loc[is_check, 'attention_check_expected']

    return df.reset_index(drop = True).reindex(columns = CLEAN_COLUMNS)


def process_validation_data(data_dir: str = './data/raw', stimuli_path: str = './stimuli_updated.xlsx', stimuli_sheet: str = 'Selected stimuli 88') -> None:
    with open(os.path.join(data_dir, 'study_configuration.json')) as f:
        config = json.load(f)

    component_map = build_component_map(config)
    component_map = add_stimulus_metadata(component_map, stimuli_path, stimuli_sheet)

    data = load_responses(os.path.join(data_dir, 'discrete_responses.csv'), component_map)
    screen = screening(data)
    clean = exclude_participants(data, screen)

    likert = clean[clean['response_type'] == 'LIKERT_GRID']
    print(f'Retained: {likert["participant_id"].nunique()} participants, {len(likert)} Likert responses across {likert["component_title"].nunique()} items')

    if not os.path.exists('./data/processed'):
        os.makedirs('./data/processed')

    clean.to_csv('./data/processed/clean_data.csv', index = False)


if __name__ == "__main__":
    process_validation_data() # Run from the 2_stimuli_validation folder
