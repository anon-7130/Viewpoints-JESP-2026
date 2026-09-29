Does online sharing drive attention to extreme viewpoints?
=======
This repo contains data, analysis scripts, and supplementary material for the Registered Report 'Does online sharing drive attention to extreme viewpoints?' submitted to _Journal of Experimental Social Psychology_.

## Repo structure

```
├── 1_developmental_studies/    Developmental Studies 1 & 2 (lab-based eye tracking)
├── 2_stimuli_validation/       Stimulus validation pretest
│   ├── data/raw/
│   ├── data/processed/         Cleaned ratings written by clean_data.py
│   ├── scripts/
│   ├── output/figures/
│   └── stimuli_updated.xlsx    Stimulus lookup used by the pilot study
└── 3_pilot_study/              Pilot study (online webcam eye tracking)
    ├── analysis.R
    ├── data/raw/
    ├── data/derived/           Cache written by analysis.R (not tracked)
    └── output/
        ├── tables/
        └── figures/
```

### 1. Developmental studies
Materials, data processing, and analyses for Developmental Studies 1 and 2, which informed the design, dependent variable, and sample size planning of the main study.

| Folder | Contents |
|---|---|
| `Website/` | The social media feed shown to participants (Flask server) and installation instructions |
| `Eye tracking script/` | Records the screen, mouse, keyboard, and EyeLink gaze data during a session |
| `Fixation check/` | Plots corrected fixations on the stimulus images to check the gaze mapping |
| `Study data & analysis/` | Preprocessing (EyeLink conversion, eyeScrollR mapping), the analysis (`data_analysis.Rmd`), and the power analysis for the main study (`power analysis/`) |

### 2. Stimuli validation
The pretest in which Prolific participants rated the political position of each candidate statement. The selected statements and their target categories are in `stimuli_updated.xlsx` (sheet `Selected stimuli 88`), which the pilot study reads directly.

`clean_data.py` applies the exclusions (any failed attention check, or more than one failed political literacy item) and writes `data/processed/clean_data.csv`; `plot_responses.py` plots the rating distribution per statement to `output/figures/`. Run them in order with `2_stimuli_validation/` as the working directory:

```
cd 2_stimuli_validation
python scripts/clean_data.py
python scripts/plot_responses.py
```

They need Python 3 with `pandas`, `matplotlib`, and `openpyxl`.

### 3. Pilot study
`analysis.R` runs the full pilot analysis: data loading, fixation detection, exclusions, the dwell-time models, and the registered-report model comparison (M0–M6), outcome-neutral checks, and secondary analyses. Tables are written to `output/tables/` and figures to `output/figures/`.

Statement positions (−2 to +2) are the median placement by the stimulus-validation raters, read from `2_stimuli_validation/data/processed/clean_data.csv`, so run `clean_data.py` first if that file is missing. Run `analysis.R` with `3_pilot_study/` as the working directory:

```
cd 3_pilot_study
Rscript analysis.R
```

It needs R with `tidyverse`, `lme4`, `lmerTest`, `readxl`, and `data.table`. The package versions of the last run are in `output/session_info.txt`. The first run parses the raw eye-tracking file and caches the result in `data/derived/`, and later runs reuse the cache as long as the inputs and fixation parameters are unchanged.

## Large data files
The pilot's raw eye-tracking file exceeds GitHub's 100 MB file limit, so it is stored as a zip archive. Unzip it in place before running `analysis.R`. From the repo root:

```
unzip "3_pilot_study/data/raw/political_spectrum_3_pilot_(v5)_September+23+2026_06.39_eye_tracking.csv.zip" -d 3_pilot_study/data/raw
```
