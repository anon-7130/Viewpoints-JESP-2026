import pandas as pd
import os
import textwrap

import matplotlib
matplotlib.use('Agg') # Write files without opening a window
import matplotlib.pyplot as plt

COLORS = {
    'surface': '#fcfcfb', 'page': '#f9f9f7',
    'ink': '#0b0b0b', 'ink_secondary': '#52514e', 'muted': '#898781',
    'grid': '#e1e0d9',
    'ramp': ['#1a5fb4', '#7fb2e2', '#87867f', '#eb9583', '#c2352f']
}

# Short x-axis ticks; the full labels go in the figure caption
SHORT_LABELS = {
    'Far left': 'FL', 'Left of centre': 'L', 'Centre': 'C',
    'Right of centre': 'R', 'Far right': 'FR'
}

# No "Centre" stimuli exist, so 3 is never a target
TARGET_TO_SCALE_POINT = {'Extreme left': 1, 'Left': 2, 'Right': 4, 'Extreme right': 5}

FAMILY_NAMES = {
    'AC': 'Attention checks',
    'CSV': 'CSV', 'WB': 'WB', 'GEN': 'Gender', 'GUN': 'Guns',
    'IMM': 'Immigration', 'RACE': 'Race', 'TRN': 'Transgender'
}

PANELS_PER_FIGURE = 12
N_COLUMNS = 4


def load_likert(path: str) -> tuple[pd.DataFrame, list[str]]:
    df = pd.read_csv(path)
    df = df[(df['response_type'] == 'LIKERT_GRID') & df['response_value'].notna()].copy()
    df['response_value'] = df['response_value'].astype(int)
    labels = df['scale_labels'].dropna().unique()[0].split('|')

    return df, labels


def draw_panel(ax: plt.Axes, values: pd.Series, title: str, labels: list[str], ymax: float, target: int = None) -> None:
    x = range(1, len(labels) + 1)
    counts = values.value_counts().reindex(x, fill_value = 0)

    ax.bar(x, counts, color = COLORS['ramp'], width = 0.74, zorder = 2, linewidth = 0)
    for xi, n in zip(x, counts):
        if n:
            ax.annotate(f'{n}', (xi, n), textcoords = 'offset points', xytext = (0, 2), ha = 'center', fontsize = 7, color = COLORS['ink_secondary'])

    if target is not None:
        ax.plot([target], [0], marker = '^', markersize = 7, color = COLORS['ink_secondary'], clip_on = False, zorder = 5) # Caret under the scale point the statement was written to occupy

    ax.set_xticks(x)
    ax.set_xticklabels([SHORT_LABELS.get(lab, lab[:2]) for lab in labels])
    ax.set_ylim(0, ymax)
    ax.set_title(f'{title}   n={counts.sum()}', fontsize = 9, color = COLORS['ink'], loc = 'left', pad = 6)

    ax.set_facecolor(COLORS['surface'])
    for side in ('top', 'right'):
        ax.spines[side].set_visible(False)
    for side in ('left', 'bottom'):
        ax.spines[side].set_color(COLORS['grid'])
    ax.tick_params(colors = COLORS['muted'], labelsize = 8, length = 0)
    ax.grid(axis = 'y', color = COLORS['grid'], linewidth = 0.6, zorder = 0)
    ax.set_axisbelow(True)


def make_figure(panels: list, suptitle: str, labels: list[str], ymax: float, ncols: int, caption_extra: str = '') -> plt.Figure:
    nrows = -(-len(panels) // ncols) # Ceiling division
    fig, axes = plt.subplots(nrows, ncols, figsize = (3.1 * ncols, 2.5 * nrows + 1.1), squeeze = False)
    fig.patch.set_facecolor(COLORS['page'])

    for ax, panel in zip(axes.flat, panels):
        draw_panel(ax, panel[1], panel[0], labels, ymax, panel[2] if len(panel) > 2 else None)
    for ax in list(axes.flat)[len(panels):]:
        ax.set_visible(False)

    caption = '  '.join(f'{SHORT_LABELS.get(l, l[:2])} = {l}' for l in labels) + caption_extra

    fig.suptitle(suptitle, fontsize = 13, color = COLORS['ink'], x = 0.012, ha = 'left', y = 0.985)
    fig.text(0.012, 0.008, textwrap.fill(caption, 140), fontsize = 8, color = COLORS['muted'], ha = 'left')
    fig.supylabel('responses', fontsize = 9, color = COLORS['ink_secondary'], x = 0.002)
    fig.tight_layout(rect = [0.012, 0.035, 1, 0.955])

    return fig


def save_figure(fig: plt.Figure, name: str, output_dir: str) -> None:
    fig.savefig(os.path.join(output_dir, name), dpi = 150, facecolor = COLORS['page'], bbox_inches = 'tight')
    plt.close(fig)


def plot_responses(data_path: str = './data/processed/clean_data.csv', output_dir: str = './output/figures') -> None:
    df, labels = load_likert(data_path)

    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    ymax = df.groupby(['component_title', 'response_value']).size().max() * 1.18 # Shared across panels, so they are comparable by eye

    # Stimuli take their sheet's target category; attention checks take the answer their title demands
    items = df.drop_duplicates('component_title').set_index('component_title')
    targets = {title: TARGET_TO_SCALE_POINT[cat] for title, cat in items['target_category'].dropna().items() if cat in TARGET_TO_SCALE_POINT}
    targets |= {title: int(v) for title, v in items['attention_check_expected'].dropna().items()}
    caption = '   ^ = the scale point the item was designed to elicit'

    # One figure per item family, split into pages of PANELS_PER_FIGURE
    for family, fam_df in df.groupby('item_family', sort = True):
        family_items = sorted(fam_df['component_title'].unique())
        pages = [family_items[i:i + PANELS_PER_FIGURE] for i in range(0, len(family_items), PANELS_PER_FIGURE)]

        for page_no, page in enumerate(pages, start = 1):
            panels = [(item, fam_df.loc[fam_df['component_title'] == item, 'response_value'], targets.get(item)) for item in page]
            suffix = f'  ({page_no}/{len(pages)})' if len(pages) > 1 else ''
            fig = make_figure(panels, f'{FAMILY_NAMES.get(family, family)} — response distribution per item{suffix}', labels, ymax, N_COLUMNS, caption)
            name = f'responses_{family}' + (f'_p{page_no}' if len(pages) > 1 else '') + '.png'
            save_figure(fig, name, output_dir)

    # Attention checks are left out of the overview; their fixed answers would swamp the pooled distribution
    statements = df[~df['is_attention_check'].fillna(False)]
    overview = [('All items pooled', statements['response_value'])]
    overview += [(f'Variant {v}', g['response_value']) for v, g in statements.groupby('item_variant') if v]
    overview_max = max(s.value_counts().max() for _, s in overview) * 1.18

    fig = make_figure(overview, 'Overview — pooled distribution and by item variant', labels, overview_max, min(N_COLUMNS, len(overview)))
    save_figure(fig, 'responses_overview.png', output_dir)

    print(f'Figures written to {output_dir}')


if __name__ == "__main__":
    plot_responses() # Run from the 2_stimuli_validation folder
