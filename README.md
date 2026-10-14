Does online sharing drive attention to extreme viewpoints?
=======
This repo contains data, analysis scripts, and supplementary material for the Registered Report 'Does online sharing drive attention to extreme viewpoints?' submitted to _Journal of Experimental Social Psychology_.

## Repo structure

```
├── 0_online_supplementary/     Online Supplementary Materials
├── 1_developmental_studies/    Developmental Studies 1 & 2 (lab-based eye tracking)
├── 2_stimuli_validation/       Stimulus validation pretest
│   ├── data/raw/
│   ├── data/processed/         Cleaned ratings written by clean_data.py
│   ├── scripts/
│   ├── stimuli/                The 88 post images shown to participants
│   ├── output/figures/
│   └── stimuli.xlsx            The 88 statements, also read by the pilot study
├── 3_pilot_study/              Pilot study (online webcam eye tracking)
│   ├── analysis.R
│   ├── data/raw/
│   ├── data/derived/           Cache written by analysis.R (not tracked)
│   └── output/
│       ├── tables/
│       └── figures/
├── 4_x_length_benchmark/       Length of X posts, the benchmark for statement length
│   ├── x_length_benchmark.py
│   ├── x_length_sample_manifest.csv
│   └── x_length_benchmark.csv
└── 5_sensitivity_analysis/     Simulation-based sensitivity power analysis
    ├── sensitivity_power_simulation.R
    └── sensitivity_power_*.csv
```

### 0. Online supplementary materials
`SI.docx` holds the Online Supplementary Materials.

### 1. Developmental studies
Materials, data processing, and analyses for Developmental Studies 1 and 2, which informed the design, dependent variable, and sample size planning of the main study.

| Folder | Contents |
|---|---|
| `Website/` | The social media feed shown to participants (Flask server) and installation instructions |
| `Eye tracking script/` | Records the screen, mouse, keyboard, and EyeLink gaze data during a session |
| `Fixation check/` | Plots corrected fixations on the stimulus images to check the gaze mapping |
| `Study data & analysis/` | Preprocessing (EyeLink conversion, eyeScrollR mapping), the analysis (`data_analysis.Rmd`), and the power analysis for the main study (`power analysis/`) |

The full data for both studies are on OSF (links in the Online Supplementary Materials); `data_analysis.Rmd` expects them in `Study data & analysis/final_data/`.

### 2. Stimuli validation
The pretest in which Prolific participants rated the political position of each statement. The 88 statements, with their topic, target category, word count, and source, are in `stimuli.xlsx` (sheet `Selected stimuli 88`), which the pilot study reads directly. The same 88 statements were shown in the pretest and the pilot. The post images are in `stimuli/`, named by the image ID in `data/raw/study_configuration.json`.

`clean_data.py` applies the exclusions (any failed attention check, or more than one failed political literacy item), which leaves 59 raters, and writes `data/processed/clean_data.csv`; `plot_responses.py` plots the rating distribution per statement to `output/figures/`. Run them in order with `2_stimuli_validation/` as the working directory:

```
cd 2_stimuli_validation
python scripts/clean_data.py
python scripts/plot_responses.py
```

They need Python 3 with `pandas`, `matplotlib`, and `openpyxl`.

### 3. Pilot study
`analysis.R` runs the full pilot analysis: data loading, fixation detection, exclusions, the dwell-time models, and the registered-report model comparison (M0–M6), outcome-neutral checks, and secondary analyses. Tables are written to `output/tables/` and figures to `output/figures/`.

Statement positions (−2 to +2) are the median placement by the stimulus-validation raters, read from `2_stimuli_validation/data/processed/clean_data.csv`, so run `clean_data.py` first if that file is missing. A participant's ideal point is the mean of position × centred agreement, divided by a constant computed from the statement positions (1.977 for the current set) so that it lies on the same −2 to +2 metric. Run `analysis.R` with `3_pilot_study/` as the working directory:

```
cd 3_pilot_study
Rscript analysis.R
```

It needs R with `tidyverse`, `lme4`, `lmerTest`, `readxl`, and `data.table`. The package versions of the last run are in `output/session_info.txt`. The first run parses the raw eye-tracking file and caches the result in `data/derived/`, and later runs reuse the cache as long as the inputs and fixation parameters are unchanged.

### 4. X length benchmark
`x_length_benchmark.py` computes the length of the X posts that the statements are compared with. It uses a fixed 10% sample (44 files, listed with checksums in `x_length_sample_manifest.csv`) of the 22-million-post release accompanying Balasubramanian et al. (2024), counts whitespace-separated words, and writes five specifications to `x_length_benchmark.csv`. The primary one, English-language posts that are not replies with URLs and mentions removed, gives *Mdn* = 20 words (IQR = 12–34). Run it from the repo root:

```
python 4_x_length_benchmark/x_length_benchmark.py --download
```

`--download` fetches the sampled files (about 830 MB) into `4_x_length_benchmark/data/`, which is not tracked; `--data-dir` stores them elsewhere. It needs Python 3 only.

### 5. Sensitivity analysis
`sensitivity_power_simulation.R` finds, for each hypothesis, the smallest effect detected with 90% (and 80%) power in the minimum analysed sample (*N* = 500; 250 per condition), using the hypothesis's own mixed-effects model and the variance components of the pilot. Statement positions and the ideal-point rescaling are the same as in `3_pilot_study/analysis.R`. The agreement ratings are the pilot's own: each simulated participant is a pilot participant drawn with replacement, half from those with an ideal point left of centre and half from those right of it, matching the stratified recruitment. The script reads them from `3_pilot_study/data/derived/analysis_trials.rds`, so run `analysis.R` first. Residuals are drawn from a skewed (gamma) distribution, and an effect counts as detected when every focal coefficient has *p* < .05 (Wald *z*) with the predicted sign. Run it with `5_sensitivity_analysis/` as the working directory:

```
cd 5_sensitivity_analysis
N_CORES=3 Rscript sensitivity_power_simulation.R
```

The summary, the power curves, and the per-dataset estimates are written to `sensitivity_power_summary.csv`, `sensitivity_power_curves.csv`, and `sensitivity_power_sims.csv`. It needs R with `lme4` and takes about an hour on three cores; without `N_CORES` it uses all cores but one.

## Large data files
The pilot's raw eye-tracking file exceeds GitHub's 100 MB file limit, so it is stored as a zip archive. Unzip it in place before running `analysis.R`. From the repo root:

```
unzip "3_pilot_study/data/raw/political_spectrum_3_pilot_(v5)_September+23+2026_06.39_eye_tracking.csv.zip" -d 3_pilot_study/data/raw
```
