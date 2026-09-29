##### Init #####
suppressWarnings(suppressPackageStartupMessages({ # Silences "package built under R x.y.z" noise
    library(tidyverse)
    library(lme4)
    library(lmerTest)
    library(readxl)
}))

options(width = 160) # Keep printed tables on one line
if (!l10n_info()$`UTF-8`) invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8")) # Figure labels use an en dash and a multiplication sign; Rscript can start in the C locale

##### Paths #####
export_path <- "data/raw/political_spectrum_3_pilot_(v5)_September+23+2026_06.39"
resp_csv <- paste0(export_path, ".csv") # One wide row per participant
rt_csv <- paste0(export_path, "_reaction_times.csv") # Same layout, "_RT_ms" columns
et_csv <- paste0(export_path, "_eye_tracking.csv") # One row per gaze sample
stimuli_xlsx <- "../2_stimuli_validation/stimuli_updated.xlsx"
stimuli_sheet <- "Selected stimuli 88" # Holds more rows than the 88 fielded posts, so it is filtered to the posts in the data
pretest_csv <- "../2_stimuli_validation/data/processed/clean_data.csv" # Pretest placements written by clean_data.py
derived_dir <- "data/derived"
output_dir <- "./output"

##### Study design #####
post_id_re <- "[A-Z]+-[0-9]+-[AS]" # e.g., "CSV-139-S". Neither pattern may contain capture groups
post_variant_re <- "-[AS]$"

# Item columns carry a block suffix: none = block 1, " (k)" = block k. Stage-1 blocks are randomizer's arms, so the block a participant answered gives their condition. First condition is reference level
condition_blocks <- c(following = 1L, sharing = 2L)
stage2_block <- 3L # Agreement ratings, shown to everyone
conditions <- names(condition_blocks)

q_personal <- "Rate how much you agree/disagree with the statement"
q_contacts <- "Rate how much people in your social circle would agree/disagree with the statement"

mc_option_col <- "Manipulation Check 2"
mc_text_col <- "Something else"
mc_component <- "Manipulation Check 2" # The eye-tracking export lists every option as an AOI of this component

chosen_key <- "ArrowLeft" # Follow/share; ArrowRight = not follow/not share
stage1_keys <- c("ArrowLeft", "ArrowRight")

position_codes <- c("Extreme left" = -2, "Left" = -1, "Right" = 1, "Extreme right" = 2) # Researcher-coded "Target category"; checks the design balance only
statements_per_topic_position <- 2
pretest_scale_centre <- 3 # Pretest placements run 1 (far left) to 5 (far right); subtracting 3 gives the -2..+2 statement positions

##### Parameters #####
# Fixation detection (I-DT)
fix_dispersion_px <- 60 # Max (xmax - xmin) + (ymax - ymin) within one fixation
fix_min_duration <- 100 # ms
gaze_smooth_k <- 3 # Running-median width applied before I-DT; 1 = off

# A saccade is horizontal if its deviation from horizontal axis is at most this quantile of all completers' deviations
horiz_tol_quantile <- 0.5
min_sacc_amp_px <- 20
min_reading_run_saccades <- 2

within_trial_gap_ms <- 250 # Longer gaps between samples are tracking dropouts, not sampling intervals
max_plausible_trial_s <- 300 # Longer trials are idle rather than reading

# Exclusion gates
fast_trial_ms <- 1000
max_pct_fast_trials <- 0.15
mc_excluded_answers <- c("Reading the words without thinking much about them",
                         "Passing the time or being entertained")
gaze_qc_max_pct_below_line <- 0.3
min_pct_trials_with_pattern <- 50

# Agreement params
scale_midpoint <- 4
ci_level <- 0.95
iso_tol_grid <- c(5, 10, 15, 20, 30, 40, 50, 60)

# Registered-report model set
ideal_point_divisor <- 3 # Raw ideal point runs from -6 to +6; dividing by 3 puts it on the -2..+2 metric of statement positions (provisional)
bf_threshold <- 20
bic_threshold <- 2 * log(bf_threshold)
max_as_correlation <- 0.95 # Above this, the H5 and H6 comparisons are uninformative
min_acrophily_coverage <- 10 # Median statements beyond the ideal point

##### Helper functions #####
cond_labels <- c(following = "Following", sharing = "Sharing")
cond_cols <- set_names(c("black", "grey55")[seq_along(conditions)], conditions) # Greyscale with redundant linetype and shape, so figures print in black and white
cond_lty <- set_names(c("solid", "dashed")[seq_along(conditions)], conditions)
cond_shapes <- set_names(c(16, 17)[seq_along(conditions)], conditions)

fig_font <- "Arial"
theme_set(
    theme_classic(base_size = 11, base_family = fig_font) +
        theme(
            axis.text               = element_text(colour = "black"),
            axis.line               = element_line(linewidth = .4),
            axis.ticks              = element_line(colour = "black", linewidth = .4),
            legend.position         = "top",
            legend.justification    = "left",
            legend.key.width        = unit(1.6, "lines"),
            strip.background        = element_blank(),
            strip.text              = element_text(size = 11, hjust = 0),
            plot.margin             = margin(6, 10, 6, 6)
        )
)

saveFigure <- function(plot, name, width, height, output_dir = "./output") {
    fig_dir <- file.path(output_dir, "figures")
    if (!dir.exists(fig_dir)) {
        dir.create(fig_dir, recursive = TRUE)
    }

    path <- file.path(fig_dir, name)
    ggsave(paste0(path, ".pdf"), plot, width = width, height = height, device = cairo_pdf, bg = "white") # Vector PDF (fonts embedded) for the manuscript
    ggsave(paste0(path, ".png"), plot, width = width, height = height, dpi = 600, bg = "white") # 600 dpi PNG for Word
}

saveTable <- function(x, name, output_dir = "./output") {
    tab_dir <- file.path(output_dir, "tables")
    if (!dir.exists(tab_dir)) {
        dir.create(tab_dir, recursive = TRUE)
    }

    write.csv(x, file.path(tab_dir, name), row.names = FALSE)
}

cached <- function(name, build, key, derived_dir = "data/derived") {
    if (!dir.exists(derived_dir)) {
        dir.create(derived_dir, recursive = TRUE)
    }

    path <- file.path(derived_dir, name)
    if (file.exists(path)) {
        x <- readRDS(path)
        if (identical(x$key, key)) return(x$value) # Rebuild whenever the key differs from the one stored with the cache
    }

    x <- build()
    saveRDS(list(key = key, value = x), path)

    return(x)
}

horizDeviation <- function(dx, dy) {
    a <- abs(atan2(dy, dx) * 180 / pi)
    pmin(a, 180 - a) # 0-90 deg; leftward and rightward movements are folded together
}

describeConditions <- function(condition) {
    paste(map_chr(conditions, ~ sprintf("%d %s", sum(condition == .x), .x)), collapse = ", ")
}

coefs <- function(m) { # Works for lmerTest (t, df, p), lme4 lmer (t only), glmer (z, p) and lm fits
    co <- summary(m)$coefficients
    has_p <- startsWith(colnames(co)[ncol(co)], "Pr")

    tibble(
        term        = rownames(co),
        estimate    = co[, 1],
        se          = co[, 2],
        df          = if ("df" %in% colnames(co)) co[, "df"] else NA_real_,
        statistic   = co[, ncol(co) - has_p],
        p_value     = if (has_p) co[, ncol(co)] else NA_real_
    )
}

##### Responses #####
loadResponses <- function(resp_csv, rt_csv, stimuli_xlsx, stimuli_sheet, pretest_csv) {
    readWideExport <- function(path) {
        d <- read.csv(path, check.names = FALSE, colClasses = "character")
        d[!is.na(d$`Participant ID`) & nzchar(d$`Participant ID`), , drop = FALSE] # Row 1 below the header is the question-text row, not a participant
    }

    parseItemCols <- function(nms) { # One row per item column: post, block, and (Stage 2) which grid question
        m <- str_match(nms, paste0("^(", post_id_re, ")( \\(([0-9]+)\\))?(_(.+))?$"))
        keep <- !is.na(m[, 1])

        tibble(
            column          = nms[keep],
            componentTitle  = m[keep, 2],
            componentId     = paste0(componentTitle, coalesce(m[keep, 3], "")), # The eye-tracking file's label
            block           = coalesce(as.integer(m[keep, 4]), 1L),
            question        = m[keep, 6]
        )
    }

    resp_wide <- readWideExport(resp_csv)
    rt_wide <- readWideExport(rt_csv)

    resp_items <- parseItemCols(names(resp_wide))
    rt_items <- parseItemCols(str_remove(names(rt_wide), "_RT_ms$")) %>%
        mutate(column = paste0(column, "_RT_ms"))

    stopifnot(all(c(q_personal, q_contacts) %in% resp_items$question),
              all(c(condition_blocks, stage2_block) %in% resp_items$block))

    rt_long <- rt_wide %>%
        select(`Participant ID`, all_of(rt_items$column)) %>%
        pivot_longer(-`Participant ID`, names_to = "column", values_to = "timeTakenMs") %>%
        filter(nzchar(timeTakenMs)) %>%
        left_join(select(rt_items, column, componentId), by = "column") %>%
        transmute(participantId = `Participant ID`, componentId, timeTakenMs = as.numeric(timeTakenMs))

    # One row per participant x component; Stage-2 rows carry both agreement ratings side by side, Stage-1 rows the keypress in `response`
    resp <- resp_wide %>%
        select(`Participant ID`, `Session ID`, all_of(resp_items$column)) %>%
        pivot_longer(all_of(resp_items$column), names_to = "column", values_to = "response") %>%
        filter(nzchar(response)) %>%
        left_join(resp_items, by = "column") %>%
        transmute(
            participantId   = `Participant ID`,
            sessionId       = `Session ID`,
            componentId, componentTitle, block,
            field           = case_when(question == q_personal ~ "agreement_personal",
                                        question == q_contacts ~ "agreement_contacts",
                                        TRUE ~ "response"),
            response
        ) %>%
        pivot_wider(names_from = field, values_from = response) %>%
        mutate(across(c(agreement_personal, agreement_contacts), as.numeric)) %>%
        left_join(rt_long, by = c("participantId", "componentId"))

    pretest_positions <- read.csv(pretest_csv) %>% # Thurstone scale value: the median placement by the independent pretest raters
        filter(response_type == "LIKERT_GRID", !is.na(target_category), !is.na(response_value)) %>%
        group_by(componentTitle = component_title) %>%
        summarise(position = median(response_value) - pretest_scale_centre, .groups = "drop")

    stimuli <- read_excel(stimuli_xlsx, sheet = stimuli_sheet) %>%
        filter(!is.na(`Stimulus ID`)) %>%
        transmute(componentTitle = `Stimulus ID`, topic = Topic,
                  target_position = unname(position_codes[`Target category`])) %>%
        left_join(pretest_positions, by = "componentTitle")
    stopifnot(!anyDuplicated(stimuli$componentTitle), !is.na(stimuli$target_position))

    components <- resp_items %>%
        distinct(componentId, componentTitle, block) %>%
        mutate(variant = str_remove_all(str_extract(componentTitle, post_variant_re), "[^[:alnum:]]")) %>%
        left_join(stimuli, by = "componentTitle")
    stopifnot(!anyDuplicated(components$componentId), !is.na(components$target_position), !is.na(components$position))

    viewing_ids <- components$componentId[components$block %in% condition_blocks] # Stage 1
    rating_ids <- components$componentId[components$block == stage2_block] # Stage 2

    stimuli <- filter(stimuli, componentTitle %in% components$componentTitle)
    stopifnot(all(count(stimuli, topic, target_position)$n == statements_per_topic_position), # Direction and extremity must vary independently of topic
              n_distinct(stimuli$target_position) == length(position_codes))
    message(sprintf("  pretest positions match the target category for %d of %d posts",
                    sum(stimuli$position == stimuli$target_position), nrow(stimuli)))
    message(sprintf("  %d posts: %d topics x %d positions x %d statements",
                    nrow(stimuli), n_distinct(stimuli$topic), length(position_codes), statements_per_topic_position))

    responses <- list(
        resp_wide   = resp_wide,
        resp        = resp,
        stimuli     = stimuli,
        components  = components,
        viewing_ids = viewing_ids,
        rating_ids  = rating_ids,
        n_posts     = n_distinct(components$componentTitle[components$componentId %in% viewing_ids])
    )

    return(responses)
}

##### Condition and completion #####
participantCompletion <- function(responses) {
    resp <- responses$resp

    # A participant who answered neither Stage-1 block (dropped out early) or both (restarted session) has no identifiable condition and cannot be complete
    participant_condition <- resp %>%
        filter(block %in% condition_blocks) %>%
        distinct(participantId, block) %>%
        group_by(participantId) %>%
        summarise(condition = if (n() == 1) conditions[match(block, condition_blocks)] else NA_character_,
                  .groups = "drop")

    completion <- resp %>%
        group_by(participantId) %>%
        summarise(n_image_answered = n_distinct(componentTitle[componentId %in% responses$viewing_ids]),
                  .groups = "drop") %>%
        full_join(responses$resp_wide %>%
                      transmute(participantId = `Participant ID`,
                                status = Status,
                                prolificId = na_if(str_trim(`prolific ID`), ""),
                                duration_s = suppressWarnings(as.numeric(`Duration (s)`))),
                  by = "participantId") %>%
        left_join(participant_condition, by = "participantId") %>%
        mutate(
            n_image_answered    = coalesce(n_image_answered, 0L),
            n_image_expected    = responses$n_posts,
            complete            = status == "Completed" & !is.na(condition) & n_image_answered == responses$n_posts # Status is authoritative; the other two catch a truncated upload
        ) %>%
        group_by(prolificId) %>%
        mutate(attempts_by_this_person = if_else(is.na(prolificId), 1L, n())) %>%
        ungroup() %>%
        relocate(prolificId, .after = participantId)

    participants <- completion %>%
        filter(complete) %>%
        transmute(participantId, Condition = factor(condition, levels = conditions))

    message(sprintf("  %d participants, %d completed all %d Stage-1 trials (%s)",
                    nrow(completion), nrow(participants), responses$n_posts,
                    describeConditions(participants$Condition)))

    status_mismatch <- filter(completion, (status == "Completed") != complete)
    if (nrow(status_mismatch) > 0) {
        message("  Status 'Completed' but failing the condition/trial-count check:")
        print(as.data.frame(select(status_mismatch, participantId, prolificId, status, condition, n_image_answered)),
              row.names = FALSE)
    }

    repeats <- filter(completion, attempts_by_this_person > 1)
    if (nrow(repeats) > 0) {
        message("  Prolific IDs with more than one attempt:")
        print(as.data.frame(repeats %>%
                                arrange(prolificId) %>%
                                select(prolificId, participantId, n_image_answered, complete)),
              row.names = FALSE)
    }

    participant_results <- list(
        completion      = completion,
        participants    = participants,
        completers      = participants$participantId
    )

    return(participant_results)
}

##### Eye tracking: gaze samples -> fixations (cached) #####
detectFixations <- function(t, x, y, dispersion = fix_dispersion_px, min_dur = fix_min_duration) {
    # I-DT: grow a window while gaze stays within the dispersion limit; emit its centroid once it spans at least min_dur
    n <- length(t)
    out <- vector("list", n)
    k <- 0L
    i <- 1L

    while (i <= n) {
        j <- i
        while (j < n) {
            xs <- x[i:(j + 1)]; ys <- y[i:(j + 1)]
            if ((max(xs) - min(xs)) + (max(ys) - min(ys)) > dispersion) break
            j <- j + 1L
        }

        if (t[j] - t[i] >= min_dur) {
            k <- k + 1L
            out[[k]] <- tibble(fix_start = t[i], fix_end = t[j], duration = t[j] - t[i],
                               x = mean(x[i:j]), y = mean(y[i:j]), n_samples = j - i + 1L)
            i <- j + 1L
        } else {
            i <- i + 1L
        }
    }

    bind_rows(out[seq_len(k)])
}

smoothGaze <- function(v, k = gaze_smooth_k) { # Webcam jitter can exceed a word-to-word reading step; a short running median lets I-DT separate fixations without shredding them
    if (k <= 1 || length(v) < k) v else as.numeric(runmed(v, k, endrule = "median"))
}

loadEyeTracking <- function(et_csv, responses, completers, derived_dir = "data/derived") {
    buildEyeTracking <- function() { # Restricted to completers' Stage-1 viewing trials; every other screen is dropped on load
        message("Loading eye tracking (large file) ...")
        et <- as_tibble(data.table::fread(
            et_csv,
            select = c("Participant ID", "Session ID", "Component", "Measure", "Time (ms)", "X", "Y", "AOI"),
            colClasses = list(character = c("Participant ID", "Session ID"),
                              numeric = c("Time (ms)", "X", "Y")),
            showProgress = FALSE
        ))

        mc_offered <- et %>% # Every option offered, including ones nobody chose
            filter(Component == mc_component, str_starts(AOI, "Choice: ")) %>%
            distinct(AOI) %>%
            pull(AOI) %>%
            str_remove("^Choice: ")

        gaze <- et %>%
            filter(Measure == "Gaze", !is.na(X), !is.na(Y),
                   Component %in% responses$viewing_ids, `Participant ID` %in% completers) %>%
            transmute(participantId = `Participant ID`, sessionId = `Session ID`,
                      componentId = Component, t_ms = `Time (ms)`, x = X, y = Y) %>%
            left_join(select(responses$components, componentId, componentTitle), by = "componentId") %>%
            arrange(participantId, t_ms) # The file is not in time order
        rm(et)

        message("Detecting fixations ...")
        fixations <- gaze %>%
            group_by(participantId, sessionId, componentId, componentTitle) %>%
            group_modify(~ detectFixations(.x$t_ms, smoothGaze(.x$x), smoothGaze(.x$y))) %>%
            ungroup()

        trial_gaze <- gaze %>% # Within-trial intervals vs dropout gaps, the inputs to the gaze-rate QC
            group_by(participantId, componentId) %>%
            summarise(
                n_gaze_rows = n(),
                gap_ms      = sum(diff(t_ms)[diff(t_ms) > within_trial_gap_ms]),
                within_ms   = sum(diff(t_ms)[diff(t_ms) <= within_trial_gap_ms]),
                n_within    = sum(diff(t_ms) <= within_trial_gap_ms),
                .groups     = "drop"
            )

        stage1_bounds <- gaze %>%
            group_by(participantId) %>%
            summarise(stage1_start = min(t_ms), stage1_end = max(t_ms), .groups = "drop")

        list(fixations = fixations, trial_gaze = trial_gaze, stage1_bounds = stage1_bounds, mc_offered = mc_offered)
    }

    eye <- cached("eye_tracking.rds", buildEyeTracking,
                  key = list(sort(responses$viewing_ids), sort(completers), file.size(et_csv),
                             fix_dispersion_px, fix_min_duration, gaze_smooth_k),
                  derived_dir = derived_dir)
    eye$fixations <- arrange(eye$fixations, participantId, componentId, fix_start) # Everything downstream relies on this order: saccades are consecutive fixations within a trial

    return(eye)
}

##### Saccades and the horizontal tolerance #####
saccadeTolerance <- function(fixations) {
    saccades <- fixations %>%
        group_by(participantId, componentId) %>%
        reframe(dev = horizDeviation(diff(x), diff(y)), amp = sqrt(diff(x)^2 + diff(y)^2)) %>%
        filter(amp >= min_sacc_amp_px)

    horiz_tol_deg <- unname(quantile(saccades$dev, horiz_tol_quantile))
    message(sprintf("  horizontal tolerance: %.2f deg (quantile %.2f of %d saccades, all completers)",
                    horiz_tol_deg, horiz_tol_quantile, nrow(saccades)))

    return(list(saccades = saccades, horiz_tol_deg = horiz_tol_deg))
}

##### Eye-tracking QC #####
gazeRateQC <- function(responses, eye, participant_results, output_dir = "./output") {
    # Numerator and denominator cover the same Stage-1 trials:
    #   rate_stage1_hz   gaze rows / viewing time
    #   hz_within        rate over within-trial intervals only (camera speed)
    #   coverage_stage1  share of viewing time not lost to dropout gaps
    # rate_stage1_hz ~= hz_within * coverage_stage1, so the two diagnostics say why a participant fails
    completers <- participant_results$completers

    trial_coverage <- responses$resp %>%
        filter(participantId %in% completers, componentId %in% responses$viewing_ids,
               !is.na(timeTakenMs), timeTakenMs <= max_plausible_trial_s * 1000) %>%
        select(participantId, componentId, timeTakenMs) %>%
        left_join(eye$trial_gaze, by = c("participantId", "componentId")) %>%
        mutate(across(c(n_gaze_rows, gap_ms, within_ms, n_within), ~ coalesce(.x, 0))) # A trial with no gaze at all still counts towards viewing time

    rt_vs_gaze <- trial_coverage %>%
        group_by(participantId) %>%
        summarise(
            n_trials        = n(),
            RT_s            = sum(timeTakenMs) / 1000,
            n_gaze_rows     = sum(n_gaze_rows),
            hz_within       = if (sum(within_ms) > 0) 1000 * sum(n_within) / sum(within_ms) else NA_real_,
            coverage_stage1 = 1 - sum(gap_ms) / sum(timeTakenMs),
            .groups         = "drop"
        ) %>%
        filter(RT_s > 0) %>%
        mutate(rate_stage1_hz = n_gaze_rows / RT_s) %>%
        left_join(participant_results$participants, by = "participantId")

    median_hz <- median(rt_vs_gaze$rate_stage1_hz)
    message(sprintf("  gaze rate: median %.1f Hz (within-trial %.1f Hz x coverage %.2f)",
                    median_hz, median(rt_vs_gaze$hz_within, na.rm = TRUE), median(rt_vs_gaze$coverage_stage1)))

    p_gaze <- ggplot(rt_vs_gaze, aes(RT_s, n_gaze_rows, colour = Condition, shape = Condition)) +
        geom_point(size = 2, alpha = .8) +
        geom_text(aes(label = sprintf("%.0f Hz", hz_within)), vjust = -0.8, size = 2.3, family = fig_font, show.legend = FALSE) + # Labels: within-trial rate
        geom_abline(slope = median_hz, intercept = 0, colour = "black", linetype = "dashed", linewidth = .4) + # Dashed line: median rate
        scale_colour_manual(values = cond_cols, labels = cond_labels) +
        scale_shape_manual(values = cond_shapes, labels = cond_labels) +
        labs(x = "Stage 1 viewing time (s)", y = "Stage 1 gaze samples")
    saveFigure(p_gaze, "gaze_samples_vs_viewing_time", 6.5, 5, output_dir)

    gaze_qc <- list(
        trial_coverage          = trial_coverage,
        rt_vs_gaze              = rt_vs_gaze,
        median_hz               = median_hz
    )

    return(gaze_qc)
}

manipulationCheck <- function(resp_wide, completion, mc_offered, output_dir = "./output") {
    manipulation_check <- resp_wide %>%
        transmute(participantId = `Participant ID`,
                  mc_option = str_trim(.data[[mc_option_col]]),
                  mc_text = str_trim(.data[[mc_text_col]])) %>%
        filter(nzchar(mc_option)) %>%
        left_join(select(completion, participantId, prolificId, status, condition, complete), by = "participantId") %>%
        relocate(mc_option, mc_text, .after = complete) %>%
        arrange(condition, mc_option, participantId)

    mc_plot_dat <- manipulation_check %>% # Completers only, by condition
        filter(complete) %>%
        mutate(mc_option = factor(mc_option, levels = sort(union(mc_offered, mc_option)))) %>%
        count(condition, mc_option, .drop = FALSE) %>%
        group_by(condition) %>%
        mutate(pct = 100 * n / sum(n)) %>%
        ungroup()

    p_mc <- ggplot(mc_plot_dat, aes(n, fct_rev(mc_option), fill = condition)) +
        geom_col(position = position_dodge(width = .75), width = .68, colour = "black", linewidth = .3) +
        geom_text(aes(label = ifelse(n == 0, "0", sprintf("%d (%.0f%%)", n, pct))),
                  position = position_dodge(width = .75), hjust = -0.12, size = 3, family = fig_font) +
        scale_y_discrete(labels = function(x) str_wrap(x, 34)) +
        scale_x_continuous(expand = expansion(mult = c(0, .18))) +
        scale_fill_manual(values = c(following = "grey25", sharing = "grey80"), labels = cond_labels, name = "Condition") +
        labs(x = "Participants", y = NULL)
    saveFigure(p_mc, "manipulation_check", 6.5, 4.5, output_dir)

    return(manipulation_check)
}

##### Reading runs per Stage-1 trial #####
readingRuns <- function(fixations, responses, participant_results, horiz_tol_deg) {
    trialMetrics <- function(x, y, duration) {
        dx <- diff(x); dy <- diff(y)
        qualified <- sqrt(dx^2 + dy^2) >= min_sacc_amp_px # A sub-threshold displacement is noise, so it cannot be horizontal and breaks a run
        horizontal <- qualified & horizDeviation(dx, dy) <= horiz_tol_deg
        runs <- rle(horizontal)
        is_run <- runs$values & runs$lengths >= min_reading_run_saccades
        reading <- rep(is_run, runs$lengths)

        tibble(
            n_fix               = length(x),
            n_sacc              = sum(qualified),
            n_horizontal_sacc   = sum(horizontal),
            n_reading_sacc      = sum(reading),
            n_reading_runs      = sum(is_run),
            reading_dwell_ms    = sum(duration[c(reading, FALSE) | c(FALSE, reading)]), # A fixation counts if it starts or ends a reading-run saccade
            fixation_time_ms    = sum(duration)
        )
    }

    stage1_trials <- responses$resp %>% # Every Stage-1 post responded to, so a trial the tracker lost entirely still exists as a zero-pattern trial
        filter(participantId %in% participant_results$completers, componentId %in% responses$viewing_ids) %>%
        distinct(participantId, componentId, componentTitle)

    message("Detecting reading runs ...")
    trial_reading <- fixations %>%
        group_by(participantId, componentId, componentTitle) %>%
        summarise(trialMetrics(x, y, duration), .groups = "drop") %>%
        right_join(stage1_trials, by = c("participantId", "componentId", "componentTitle")) %>%
        left_join(select(responses$components, componentId, topic), by = "componentId") %>%
        mutate(
            across(c(n_fix, n_sacc, n_horizontal_sacc, n_reading_sacc, n_reading_runs), ~ coalesce(.x, 0L)),
            across(c(reading_dwell_ms, fixation_time_ms), ~ coalesce(.x, 0)),
            has_reading_pattern = reading_dwell_ms > 0
        )
    stopifnot(nrow(trial_reading) == nrow(stage1_trials),
              all((trial_reading$n_reading_runs > 0) == trial_reading$has_reading_pattern))

    message(sprintf("  %d trials; %d (%.1f%%) produced no fixation and count as zero-pattern trials",
                    nrow(trial_reading), sum(trial_reading$n_fix == 0), 100 * mean(trial_reading$n_fix == 0)))

    prevalence <- trial_reading %>% # Gate-4 input. Denominator is every trial presented, so lost tracking counts against the participant
        group_by(participantId) %>%
        summarise(
            n_trials                = n(),
            n_trials_with_fixations = sum(n_fix > 0),
            pct_trials_tracked      = 100 * mean(n_fix > 0),
            n_with_pattern          = sum(has_reading_pattern),
            pct_trials_with_pattern = 100 * mean(has_reading_pattern),
            pct_pattern_of_tracked  = if (any(n_fix > 0)) 100 * sum(has_reading_pattern) / sum(n_fix > 0) else NA_real_, # For comparison only
            total_reading_runs      = sum(n_reading_runs),
            runs_per_trial          = mean(n_reading_runs),
            median_dwell_ms         = median(reading_dwell_ms),
            .groups                 = "drop"
        ) %>%
        left_join(participant_results$participants, by = "participantId") %>%
        mutate(below_threshold = pct_trials_with_pattern < min_pct_trials_with_pattern) %>%
        arrange(pct_trials_with_pattern)

    return(list(trial_reading = trial_reading, prevalence = prevalence))
}

##### Exclusion gates #####
exclusionGates <- function(responses, participant_results, manipulation_check, gaze_qc, prevalence, mc_offered, output_dir = "./output") {
    message("Applying exclusion gates ...")
    stopifnot(all(mc_excluded_answers %in% mc_offered)) # A reworded option would disarm gate 2

    completers <- participant_results$completers
    participants <- participant_results$participants

    gates <- list(
        key_mashing         = responses$resp %>%
                                  filter(participantId %in% completers, componentId %in% responses$viewing_ids, !is.na(timeTakenMs)) %>%
                                  group_by(participantId) %>%
                                  summarise(pct_fast = mean(timeTakenMs < fast_trial_ms), .groups = "drop") %>%
                                  filter(pct_fast > max_pct_fast_trials) %>%
                                  pull(participantId),
        manipulation_check  = manipulation_check %>%
                                  filter(complete, mc_option %in% mc_excluded_answers) %>%
                                  pull(participantId),
        gaze_rate           = gaze_qc$rt_vs_gaze %>%
                                  filter(1 - rate_stage1_hz / gaze_qc$median_hz > gaze_qc_max_pct_below_line) %>%
                                  pull(participantId),
        reading_yield       = prevalence %>%
                                  filter(below_threshold) %>%
                                  pull(participantId)
    )

    gate_labels <- c(
        key_mashing         = sprintf("key-mashing (>%.0f%% of trials < %d ms)", 100 * max_pct_fast_trials, fast_trial_ms),
        manipulation_check  = "manipulation check (not engaging with content)",
        gaze_rate           = sprintf("gaze rate (>%.0f%% below median)", 100 * gaze_qc_max_pct_below_line),
        reading_yield       = sprintf("reading pattern on < %d%% of trials", min_pct_trials_with_pattern)
    )

    # n_failing_gate: fails this gate, whatever the earlier gates did. n_removed: removed here, i.e., not already removed by an earlier gate
    removed_after <- c(list(character(0)), accumulate(gates, union))
    remaining <- map(removed_after, ~ filter(participants, !participantId %in% .x))

    funnel <- tibble(
        step            = c("completers", sprintf("gate %d: %s", seq_along(gates), gate_labels[names(gates)])),
        n_failing_gate  = c(NA, lengths(gates)),
        n_removed       = c(NA, diff(lengths(removed_after))),
        n_remaining     = map_int(remaining, nrow)
    ) %>%
        bind_cols(map_dfr(remaining, ~ as_tibble(as.list(
            set_names(map_int(conditions, function(cond) sum(.x$Condition == cond)), paste0("n_", conditions))))))
    saveTable(funnel, "exclusion_funnel.csv", output_dir)
    print(as.data.frame(funnel), row.names = FALSE)
    message("NOTE: gate 4 conditions on the dependent variable")

    analysis_sample <- last(remaining)

    exclusion_results <- list(
        gates           = gates,
        funnel          = funnel,
        analysis_sample = analysis_sample,
        analysis_ids    = analysis_sample$participantId
    )

    return(exclusion_results)
}

##### Descriptives of the reading-pattern DV #####
dvDescriptives <- function(trial_reading, prevalence, analysis_sample, components, saccades, horiz_tol_deg, output_dir = "./output") {
    analysis_ids <- analysis_sample$participantId

    trials_by_condition <- left_join(trial_reading, analysis_sample, by = "participantId")
    dv_summary <- bind_rows(trials_by_condition, mutate(trials_by_condition, Condition = "all")) %>% # "all" row: the zero-trial share quoted in the text
        mutate(Condition = fct_inorder(as.character(Condition))) %>%
        group_by(Condition) %>%
        summarise(
            n_participants      = n_distinct(participantId),
            n_trials            = n(),
            pct_trials_zero     = 100 * mean(!has_reading_pattern),
            mean_runs_per_trial = mean(n_reading_runs),
            mean_dwell_ms       = mean(reading_dwell_ms),
            .groups             = "drop"
        )
    saveTable(dv_summary, "dv_descriptives.csv", output_dir)
    print(as.data.frame(mutate(dv_summary, across(where(is.double), ~ round(.x, 2)))), row.names = FALSE)

    prevalence <- mutate(prevalence, in_analysis = participantId %in% analysis_ids)

    p_prev <- ggplot(prevalence, aes(pct_trials_with_pattern, fill = in_analysis)) +
        geom_histogram(binwidth = 5, boundary = 0, colour = "black", linewidth = .3) +
        geom_vline(xintercept = min_pct_trials_with_pattern, linetype = "dashed", linewidth = .5) + # Gate-4 cut-off
        scale_fill_manual(values = c(`TRUE` = "grey30", `FALSE` = "white"),
                          labels = c(`TRUE` = "Analysed", `FALSE` = "Excluded"), name = NULL) +
        scale_y_continuous(expand = expansion(mult = c(0, .05))) +
        labs(x = "Trials with a reading pattern (%)", y = "Participants")
    saveFigure(p_prev, "reading_pattern_prevalence", 6.5, 4, output_dir)

    per_post <- trial_reading %>%
        group_by(componentTitle) %>%
        summarise(
            n_participants                  = n_distinct(participantId),
            n_with_pattern                  = sum(has_reading_pattern),
            pct_participants_with_pattern   = 100 * mean(has_reading_pattern),
            total_reading_runs              = sum(n_reading_runs),
            mean_runs_per_participant       = mean(n_reading_runs),
            mean_dwell_ms                   = mean(reading_dwell_ms),
            .groups                         = "drop"
        ) %>%
        left_join(distinct(components, componentTitle, topic, variant), by = "componentTitle") %>%
        arrange(pct_participants_with_pattern) %>%
        mutate(rank = row_number())

    post_long <- per_post %>%
        select(rank, variant,
               `Participants with a reading pattern (%)` = pct_participants_with_pattern,
               `Total reading runs` = total_reading_runs) %>%
        pivot_longer(-c(rank, variant), names_to = "metric") %>%
        mutate(metric = fct_inorder(metric))
    post_med <- post_long %>% group_by(metric) %>% summarise(med = median(value), .groups = "drop")
    variants <- sort(unique(per_post$variant))

    p_post <- ggplot(post_long, aes(rank, value)) +
        geom_segment(aes(xend = rank, yend = 0), colour = "grey80", linewidth = .4) +
        geom_point(aes(colour = variant, shape = variant), size = 1.9) +
        geom_hline(data = post_med, aes(yintercept = med), linetype = "dashed", linewidth = .5) +
        geom_text(data = post_med, aes(x = 1, y = med, label = sprintf("Mdn = %.0f", med)),
                  hjust = 0, vjust = -0.6, size = 3.2, family = fig_font) +
        facet_wrap(~ metric, ncol = 1, scales = "free_y") +
        scale_colour_manual(values = set_names(c("black", "grey55", "grey75")[seq_along(variants)], variants),
                            na.value = "grey50", name = "Variant") +
        scale_shape_manual(values = set_names(c(16, 17, 15)[seq_along(variants)], variants), name = "Variant") +
        labs(x = "Post (ranked)", y = NULL)
    saveFigure(p_post, "reading_patterns_per_post", 6.5, 6, output_dir)

    # Isotropic benchmark. With no directional preference, deviation from horizontal would be uniform on [0, 90] deg
    # and a +/-T window would pass T/90 of saccades by chance
    sacc_analysis <- filter(saccades, participantId %in% analysis_ids)

    iso <- tibble(tol_deg = sort(unique(c(iso_tol_grid, horiz_tol_deg)))) %>%
        mutate(
            observed_pass       = map_dbl(tol_deg, ~ mean(sacc_analysis$dev <= .x)),
            isotropic_null      = tol_deg / 90,
            enrichment          = observed_pass / isotropic_null,
            is_operating_point  = tol_deg == horiz_tol_deg
        )
    message(sprintf("  %d saccades in the analysis sample; enrichment at %.2f deg = %.2fx",
                    nrow(sacc_analysis), horiz_tol_deg, iso$enrichment[iso$is_operating_point]))

    p_iso <- ggplot(iso, aes(tol_deg)) +
        geom_line(aes(y = isotropic_null, linetype = "Isotropic null (T/90)"), colour = "grey45", linewidth = .6) +
        geom_line(aes(y = observed_pass, linetype = "Observed"), linewidth = .7) +
        geom_point(aes(y = observed_pass), size = 1.8) +
        geom_vline(xintercept = horiz_tol_deg, colour = "grey40", linetype = "dotted") + # Operating tolerance
        geom_text(data = filter(iso, is_operating_point),
                  aes(y = observed_pass, label = sprintf("%.2f×", enrichment)),
                  vjust = -0.6, hjust = 1.15, size = 3.5, family = fig_font) +
        scale_linetype_manual(values = c("Isotropic null (T/90)" = "dashed", "Observed" = "solid"), name = NULL) +
        labs(x = "Horizontal tolerance T (degrees)", y = "Proportion of saccades counted horizontal")
    saveFigure(p_iso, "isotropic_benchmark", 6.5, 4.5, output_dir)

    return(list(prevalence = prevalence, per_post = per_post, iso = iso))
}

##### Analysis trials #####
buildAnalysisTrials <- function(trial_reading, responses, analysis_sample, derived_dir = "data/derived") {
    ratings <- responses$resp %>%
        filter(participantId %in% analysis_sample$participantId, componentId %in% responses$rating_ids) %>%
        distinct(participantId, componentTitle, .keep_all = TRUE) %>%
        select(participantId, componentTitle, agreement_personal, agreement_contacts)

    analysis_trials <- trial_reading %>%
        left_join(ratings, by = c("participantId", "componentTitle")) %>%
        left_join(analysis_sample, by = "participantId")

    analysis_trials %>% # Input of power_analysis.R
        select(participantId, Condition, componentTitle, topic, agreement_personal, agreement_contacts,
               n_reading_runs, reading_dwell_ms) %>%
        saveRDS(file.path(derived_dir, "analysis_trials.rds"))

    return(list(ratings = ratings, analysis_trials = analysis_trials))
}

##### Pilot model #####
pilotModels <- function(analysis_trials, output_dir = "./output") {
    # Quadratic in agreement, standardised across all analysed trials, and its interaction with condition. All p values are two-sided
    # Random effects: (1|componentTitle) + (1|participantId) is the primary; adding (1|topic) is the secondary specification
    fixed_rhs <- "agreement_z + I(agreement_z^2) + Condition + agreement_z:Condition + I(agreement_z^2):Condition"
    re_specs <- c(
        re_stimulus = "(1 | componentTitle) + (1 | participantId)",
        re_topic    = "(1 | topic) + (1 | componentTitle) + (1 | participantId)"
    )
    quad_term <- "I(agreement_z^2)"
    quad_cond_term <- paste0(quad_term, ":Condition", conditions[2])

    measures <- list(
        personal = list(col = "agreement_personal", label = "Personal Agreement",
                        axis = "Personal agreement"),
        contacts = list(col = "agreement_contacts", label = "Perceived Social Media Contacts Agreement",
                        axis = "Perceived agreement of social media contacts")
    )

    fitModel <- function(d, re = "re_stimulus") {
        suppressMessages(lmerTest::lmer(
            as.formula(paste("reading_dwell_ms ~", fixed_rhs, "+", re_specs[[re]])),
            data = d, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))))
    }

    predictAt <- function(m, grid) { # grid holds agreement_z and Condition
        z <- qnorm(1 - (1 - ci_level) / 2)
        X <- model.matrix(as.formula(paste("~", fixed_rhs)), grid)
        se <- sqrt(rowSums((X %*% as.matrix(vcov(m))) * X))

        mutate(grid, pred = as.numeric(X %*% fixef(m)), lo = pred - z * se, hi = pred + z * se)
    }

    predictCurve <- function(m, d, n = 100) { # Back-transformed to the rating scale for plotting
        grid <- expand_grid(agreement_z = seq(min(d$agreement_z), max(d$agreement_z), length.out = n),
                            Condition = factor(conditions, levels = conditions))
        predictAt(m, grid) %>% mutate(agreement = agreement_z * sd(d$agreement) + mean(d$agreement))
    }

    message("Fitting pilot models ...")
    fits <- map(measures, function(ms) {
        d <- analysis_trials %>%
            filter(!is.na(.data[[ms$col]])) %>%
            mutate(agreement = .data[[ms$col]], agreement_z = as.numeric(scale(agreement)))
        list(data = d, models = map(set_names(names(re_specs)), ~ fitModel(d, .x)))
    })

    fixed_effects <- map_dfr(names(re_specs), function(re) map_dfr(names(measures), function(mkey) {
        d <- fits[[mkey]]$data
        coefs(fits[[mkey]]$models[[re]]) %>%
            mutate(measure = measures[[mkey]]$label, re_spec = re,
                   n_participants = n_distinct(d$participantId), n_obs = nrow(d),
                   agreement_mean = mean(d$agreement), agreement_sd = sd(d$agreement), .before = 1)
    }))
    saveTable(fixed_effects, "fixed_effects.csv", output_dir)

    random_effects <- map_dfr(names(re_specs), function(re) map_dfr(names(measures), function(mkey) {
        as.data.frame(VarCorr(fits[[mkey]]$models[[re]])) %>%
            transmute(measure = measures[[mkey]]$label, re_spec = re, group = grp, sd = sdcor)
    }))
    saveTable(random_effects, "random_effects.csv", output_dir)

    # Model predictions quoted in the text: dwell time at mean agreement, the peak and trough of each condition's curve, and the two scale extremes
    curve_summary <- imap_dfr(measures, function(ms, mkey) {
        d <- fits[[mkey]]$data
        m <- fits[[mkey]]$models$re_stimulus
        b <- fixef(m)

        predictCurve(m, d, n = 6001) %>%
            group_by(Condition) %>%
            summarise(
                quadratic           = b[[quad_term]] + (first(Condition) == conditions[2]) * b[[quad_cond_term]],
                at_mean_ms          = predictAt(m, tibble(agreement_z = 0, Condition = first(Condition)))$pred,
                max_ms              = max(pred),
                max_at_z            = agreement_z[which.max(pred)],
                max_at_rating       = agreement[which.max(pred)],
                min_ms              = min(pred),
                min_at_z            = agreement_z[which.min(pred)],
                min_at_rating       = agreement[which.min(pred)],
                at_disagreement_ms  = pred[which.min(agreement_z)],
                at_agreement_ms     = pred[which.max(agreement_z)],
                .groups             = "drop"
            ) %>%
            mutate(measure = ms$label, .before = 1)
    })
    saveTable(curve_summary, "curve_summary.csv", output_dir)

    message("\nPilot model predictions (primary random-effects specification):")
    print(as.data.frame(mutate(curve_summary, across(where(is.numeric), ~ round(.x, 2)))), row.names = FALSE)

    # Dwell-by-agreement figures. Lines: model predictions with ci_level bands; points: observed means
    iwalk(measures, function(ms, mkey) {
        d <- fits[[mkey]]$data
        curve <- predictCurve(fits[[mkey]]$models$re_stimulus, d)
        means <- d %>%
            group_by(Condition, agreement) %>%
            summarise(reading_dwell_ms = mean(reading_dwell_ms), .groups = "drop")
        scale_points <- sort(unique(d$agreement))

        p <- ggplot(curve, aes(agreement, pred, colour = Condition, fill = Condition, linetype = Condition, shape = Condition)) +
            geom_ribbon(aes(ymin = lo, ymax = hi), alpha = .12, colour = NA, show.legend = FALSE) +
            geom_line(linewidth = .7) +
            geom_point(data = means, aes(agreement, reading_dwell_ms), size = 1.8) +
            scale_x_continuous(breaks = scale_points) +
            scale_y_continuous(labels = scales::label_comma()) +
            scale_colour_manual(values = cond_cols, labels = cond_labels) +
            scale_fill_manual(values = cond_cols, labels = cond_labels) +
            scale_linetype_manual(values = cond_lty, labels = cond_labels) +
            scale_shape_manual(values = cond_shapes, labels = cond_labels) +
            labs(x = sprintf("%s (%g–%g)", ms$axis, min(scale_points), max(scale_points)),
                 y = "Reading-pattern dwell time (ms)")
        saveFigure(p, sprintf("dwell_by_%s_agreement", mkey), 6.5, 4.5, output_dir)
    })

    pilot_results <- list(
        fits            = fits,
        fixed_effects   = fixed_effects,
        random_effects  = random_effects,
        curve_summary   = curve_summary
    )

    return(pilot_results)
}

##### Registered-report analyses #####
# Candidate model set, decision rule, outcome-neutral checks and secondary analyses of the Stage 1 report
rrDerivedVariables <- function(analysis_trials, ratings, stimuli) {
    ideal_points <- ratings %>% # Thurstone-style ideal point. Pretest statement position x centred personal rating, averaged within each topic, then across topics
        filter(!is.na(agreement_personal)) %>%
        inner_join(stimuli, by = "componentTitle") %>%
        group_by(participantId, topic) %>%
        summarise(topic_score = mean(position * (agreement_personal - scale_midpoint)), .groups = "drop_last") %>%
        summarise(ideal_point = mean(topic_score) / ideal_point_divisor, .groups = "drop")

    rr_trials <- analysis_trials %>% # Every candidate model is fitted to these same rows, so their BICs are comparable
        filter(!is.na(agreement_personal), !is.na(agreement_contacts)) %>%
        left_join(select(stimuli, componentTitle, position), by = "componentTitle") %>%
        left_join(ideal_points, by = "participantId") %>%
        mutate(
            TDT     = reading_dwell_ms,
            A       = agreement_personal,
            S       = agreement_contacts,
            C       = as.integer(Condition == conditions[2]),
            R       = 1L - C,
            A_lo    = pmin(A - scale_midpoint, 0),
            A_hi    = pmax(A - scale_midpoint, 0),
            S_lo    = pmin(S - scale_midpoint, 0),
            S_hi    = pmax(S - scale_midpoint, 0),
            A_lo_R  = A_lo * R,
            A_hi_R  = A_hi * R,
            S_lo_C  = S_lo * C,
            S_hi_C  = S_hi * C,
            D       = abs(position - ideal_point),
            Z       = as.integer(sign(position) == sign(ideal_point) & abs(position) > abs(ideal_point)) # sign(0) matches no statement, so an ideal point of exactly 0 has no side
        )
    stopifnot(!is.na(rr_trials$position), !is.na(rr_trials$ideal_point))

    return(rr_trials)
}

rrModelComparison <- function(rr_trials, output_dir = "./output") {
    rr_re <- "(1 | participantId) + (1 + C | componentTitle)"
    rr_models <- tribble(
        ~model, ~hypothesis,    ~rhs,                                       ~invariant,
        "M0",   "H0",           "C",                                        FALSE,
        "M1",   "H1/H2",        "A + C",                                    TRUE,
        "M2",   "H3",           "A_lo + A_hi + C",                          TRUE,
        "M3",   "H5",           "C + S_lo_C + S_hi_C",                      FALSE,
        "M4",   "baseline",     "D + C",                                    TRUE,
        "M5",   "H4",           "D + Z + C",                                TRUE,
        "M6",   "H6",           "C + A_lo_R + A_hi_R + S_lo_C + S_hi_C",    FALSE
    )

    rr_signs <- list( # Signs each hypothesis requires; M1 is handled separately because either sign supports a hypothesis (H1 or H2)
        M2 = c(A_lo = 1, A_hi = -1),
        M3 = c(S_lo_C = -1, S_hi_C = 1),
        M5 = c(Z = 1),
        M6 = c(A_lo_R = 1, A_hi_R = -1, S_lo_C = -1, S_hi_C = 1)
    )

    fitRR <- function(rhs) {
        suppressMessages(lme4::lmer(as.formula(paste("TDT ~", rhs, "+", rr_re)), data = rr_trials, REML = FALSE,
                                    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))))
    }

    augmentWithCondition <- function(rhs) { # The moderation check's alternative: every non-condition term also interacts with C
        terms <- setdiff(str_split_1(rhs, fixed(" + ")), "C")
        paste(rhs, paste0(terms, ":C", collapse = " + "), sep = " + ")
    }

    signCheck <- function(model, b) {
        if (model == "M1") return(if (b[["A"]] > 0) "H1 (A > 0)" else "H2 (A < 0)")
        want <- rr_signs[[model]]
        if (is.null(want)) return(NA_character_)
        if (all(sign(b[names(want)]) == want)) "match" else "mismatch"
    }

    fitWarnings <- function(m) paste(c(m@optinfo$conv$lme4$messages, unlist(m@optinfo$warnings)), collapse = "; ")

    message("Fitting the registered-report model set ...")
    rr_fits <- map(set_names(rr_models$rhs, rr_models$model), fitRR)

    bic_table <- rr_models %>%
        mutate(
            n_par               = map_int(rr_fits, ~ as.integer(attr(logLik(.x), "df"))),
            logLik              = map_dbl(rr_fits, ~ as.numeric(logLik(.x))),
            BIC                 = map_dbl(rr_fits, BIC),
            delta_BIC           = BIC - min(BIC),
            BF_leader_vs_model  = exp(delta_BIC / 2),
            BIC_weight          = exp(-delta_BIC / 2) / sum(exp(-delta_BIC / 2)),
            sign_check          = map2_chr(model, rr_fits, ~ signCheck(.x, fixef(.y))),
            singular            = map_lgl(rr_fits, isSingular),
            warnings            = map_chr(rr_fits, fitWarnings)
        ) %>%
        arrange(BIC)
    saveTable(bic_table, "rr_bic_table.csv", output_dir)
    saveTable(imap_dfr(rr_fits, ~ mutate(coefs(.x), model = .y, .before = 1)), "rr_coefficients.csv", output_dir)

    # Decision rule
    top <- bic_table[1, ]
    gap_to_runner_up <- bic_table$delta_BIC[2]
    selected <- gap_to_runner_up >= bic_threshold

    moderation <- if (top$invariant) { # Only accounts that predict the same effect in both conditions get this check
        m_aug <- fitRR(augmentWithCondition(top$rhs))
        delta <- BIC(m_aug) - top$BIC

        tibble(
            model                               = top$model,
            rhs_augmented                       = augmentWithCondition(top$rhs),
            BIC_invariant                       = top$BIC,
            BIC_augmented                       = BIC(m_aug),
            delta_BIC_augmented_minus_invariant = delta,
            verdict                             = case_when(delta >= bic_threshold ~ "invariance supported",
                                                            delta <= -bic_threshold ~ "invariance contradicted",
                                                            TRUE ~ "unresolved"),
            singular                            = isSingular(m_aug),
            warnings                            = fitWarnings(m_aug)
        )
    }
    if (!is.null(moderation)) saveTable(moderation, "rr_moderation_check.csv", output_dir)

    n_retained <- n_distinct(rr_trials$participantId)
    stop_met <- selected && (is.null(moderation) || moderation$verdict != "unresolved")

    rr_decision <- tibble(
        n_participants          = n_retained,
        !!!set_names(map(conditions, ~ n_distinct(rr_trials$participantId[rr_trials$Condition == .x])), paste0("n_", conditions)),
        n_obs                   = nrow(rr_trials),
        leader                  = top$model,
        leader_hypothesis       = top$hypothesis,
        runner_up               = bic_table$model[2],
        delta_BIC_runner_up     = gap_to_runner_up,
        BF_leader_vs_runner_up  = exp(gap_to_runner_up / 2),
        selected                = selected,
        sign_check              = top$sign_check,
        moderation              = if (is.null(moderation)) "not required" else moderation$verdict,
        stopping_criterion_met  = stop_met
    )
    saveTable(rr_decision, "rr_decision.csv", output_dir)

    message(sprintf("\nRegistered-report model set (n = %d, %d trials; BF >= %d means delta BIC >= %.2f):",
                    n_retained, nrow(rr_trials), bf_threshold, bic_threshold))
    print(as.data.frame(bic_table %>%
        transmute(model, hypothesis, n_par, BIC = round(BIC, 1), delta_BIC = round(delta_BIC, 2),
                  BIC_weight = signif(BIC_weight, 3), sign_check, singular)), row.names = FALSE)
    message(sprintf("  leader %s, %s; moderation: %s; stopping criterion met: %s",
                    top$model, if (selected) "selected" else "not selected (inconclusive)",
                    rr_decision$moderation, stop_met))

    rr_results <- list(
        fits        = rr_fits,
        bic_table   = bic_table,
        moderation  = moderation,
        decision    = rr_decision
    )

    return(rr_results)
}

rrOutcomeNeutralChecks <- function(rr_trials, output_dir = "./output") {
    per_participant <- rr_trials %>%
        group_by(participantId, Condition) %>%
        summarise(
            ideal_point             = first(ideal_point),
            sd_A_minus_S            = sd(A - S),
            n_beyond_ideal_point    = sum(Z),
            .groups                 = "drop"
        )

    r_AS <- cor(rr_trials$A, rr_trials$S)
    median_coverage <- median(per_participant$n_beyond_ideal_point)

    outcome_neutral <- tibble(
        check                   = c("cor(A, S) across trials", "median within-participant SD of A - S",
                                    "median statements beyond the ideal point"),
        value                   = c(r_AS, median(per_participant$sd_A_minus_S), median_coverage),
        criterion               = c(sprintf("<= %.2f", max_as_correlation), NA, sprintf(">= %d", min_acrophily_coverage)),
        consequence_if_failed   = c("H5/H6 comparisons uninformative", NA, "acrophily comparison underpowered"),
        passed                  = c(r_AS <= max_as_correlation, NA, median_coverage >= min_acrophily_coverage)
    )
    saveTable(outcome_neutral, "rr_outcome_neutral_checks.csv", output_dir)
    print(as.data.frame(mutate(outcome_neutral, value = round(value, 3))), row.names = FALSE)

    return(list(per_participant = per_participant, outcome_neutral = outcome_neutral))
}

rrSecondaryAnalyses <- function(trial_reading, responses, analysis_sample, output_dir = "./output") {
    choices <- responses$resp %>%
        filter(participantId %in% analysis_sample$participantId, componentId %in% responses$viewing_ids) %>%
        select(participantId, componentId, response)
    stopifnot(all(choices$response %in% stage1_keys))

    choice_trials <- trial_reading %>%
        inner_join(choices, by = c("participantId", "componentId")) %>%
        left_join(analysis_sample, by = "participantId") %>%
        mutate(
            chosen  = as.integer(response == chosen_key),
            C       = as.integer(Condition == conditions[2]),
            TDT_s   = reading_dwell_ms / 1000 # Seconds keep the logit coefficients on a readable scale
        )

    # Individual level: does a participant look longer at the posts they go on to choose?
    m_choice <- suppressMessages(glmer(chosen ~ TDT_s * C + (1 | componentTitle) + (1 | participantId),
                                       data = choice_trials, family = binomial,
                                       control = glmerControl(optimizer = "bobyqa")))
    saveTable(coefs(m_choice) %>% mutate(n_obs = nrow(choice_trials), singular = isSingular(m_choice)),
              "rr_choice_glmm.csv", output_dir)

    # Statement level: do the most-looked-at posts also get followed/shared most?
    statement_level <- choice_trials %>%
        group_by(componentTitle, Condition) %>%
        summarise(
            n_participants  = n(),
            mean_TDT_ms     = mean(reading_dwell_ms),
            prop_chosen     = mean(chosen),
            .groups         = "drop"
        )

    statement_models <- map_dfr(conditions, function(cond) {
        m <- lm(mean_TDT_ms ~ prop_chosen, data = filter(statement_level, Condition == cond))
        coefs(m) %>% mutate(Condition = cond, n_statements = nobs(m), r_squared = summary(m)$r.squared, .before = 1)
    })
    saveTable(statement_models, "rr_statement_level_regressions.csv", output_dir)

    message("\nSecondary analyses:")
    print(as.data.frame(bind_rows(
        coefs(m_choice) %>% mutate(analysis = "choice GLMM", .before = 1),
        statement_models %>%
            filter(term == "prop_chosen") %>%
            transmute(analysis = paste("statement level,", Condition), term, estimate, se, df, statistic, p_value)
    ) %>%
        mutate(across(where(is.numeric), ~ signif(.x, 3)))), row.names = FALSE)

    secondary_results <- list(
        choice_model        = m_choice,
        statement_level     = statement_level,
        statement_models    = statement_models
    )

    return(secondary_results)
}

##### Load data #####
message("Loading responses ...")
responses <- loadResponses(resp_csv, rt_csv, stimuli_xlsx, stimuli_sheet, pretest_csv)
participant_results <- participantCompletion(responses)

eye <- loadEyeTracking(et_csv, responses, participant_results$completers, derived_dir)
saccade_results <- saccadeTolerance(eye$fixations)

##### Call QC and exclusion functions #####
gaze_qc <- gazeRateQC(responses, eye, participant_results, output_dir)
manipulation_check <- manipulationCheck(responses$resp_wide, participant_results$completion, eye$mc_offered, output_dir)

reading <- readingRuns(eye$fixations, responses, participant_results, saccade_results$horiz_tol_deg)

exclusions <- exclusionGates(responses, participant_results, manipulation_check, gaze_qc,
                             reading$prevalence, eye$mc_offered, output_dir)
trial_reading <- filter(reading$trial_reading, participantId %in% exclusions$analysis_ids)

dv_descriptives <- dvDescriptives(trial_reading, reading$prevalence, exclusions$analysis_sample,
                                  responses$components, saccade_results$saccades,
                                  saccade_results$horiz_tol_deg, output_dir)

##### Call pilot model function #####
analysis_data <- buildAnalysisTrials(trial_reading, responses, exclusions$analysis_sample, derived_dir)
pilot_results <- pilotModels(analysis_data$analysis_trials, output_dir)

##### Call registered-report analysis functions #####
rr_trials <- rrDerivedVariables(analysis_data$analysis_trials, analysis_data$ratings, responses$stimuli)
rr_results <- rrModelComparison(rr_trials, output_dir)
rr_outcome_neutral <- rrOutcomeNeutralChecks(rr_trials, output_dir)
rr_secondary <- rrSecondaryAnalyses(trial_reading, responses, exclusions$analysis_sample, output_dir)

##### Session info #####
writeLines(capture.output(sessionInfo()), file.path(output_dir, "session_info.txt"))
message("\nDone. Outputs in ", output_dir)
