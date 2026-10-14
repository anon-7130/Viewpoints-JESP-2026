# =============================================================================
# Simulation-based sensitivity power analysis, one hypothesis at a time
# -----------------------------------------------------------------------------
# For each hypothesis (H1/H2, H3, H4, H5, H6) this script finds the smallest
# true effect that would be detected with the target power (default 90%) at
# alpha = .05 (two-sided, correct sign required) in the minimum analysed
# sample (N = 500; 250 per condition), using the hypothesis's own
# mixed-effects model. Results are reported in ms and as Cohen's d.
#
# Cohen's d = (predicted change in dwell time over a stated predictor span)
#             / SD_total, where SD_total = sqrt(sum of variance components)
#             (Westfall, Kenny & Judd, 2014).
#   H1/H2 : span = full agreement scale (1 -> 7, 6 points)
#   H3/H5/H6 : span = one segment, midpoint -> endpoint (3 points)
#   H4 : Z is binary, so d = beta_Z / SD_total
#
# Statement positions are the median pretest placements of the 88 pilot
# statements and the ideal point is rescaled as in 3_pilot_study/analysis.R,
# so D and Z are built exactly as in the registered analysis.
#
# Agreement ratings are resampled from the pilot: whole participants, half
# from each side of centre (stratified recruitment).
#
# Requires: lme4 (and base package 'parallel'). Run with
#          5_sensitivity_analysis/ as the working directory, after
#          3_pilot_study/analysis.R.
# Runtime: one fit on 44,000 trials takes ~2-3 s; N_SIM = 1000 means 5,000
#          fits (about 1 hour on 3 cores). Use N_SIM = 100 for a quick test.
#          Set the environment variable N_CORES to limit the cores used.
# =============================================================================

library(lme4)
library(parallel)

# -----------------------------------------------------------------------------
# 0. SETTINGS -- edit here
# -----------------------------------------------------------------------------
SEED          <- 2026
N_PER_COND    <- 250        # minimum analysed sample per condition
N_SIM         <- 1000       # simulated datasets (each = 5 model fits)
ALPHA         <- .05
TARGET_POWER  <- .90
N_CORES       <- as.integer(Sys.getenv("N_CORES", max(1, detectCores() - 1)))

# Variance components from the pilot (ms; Table 2)
B0            <- 1265       # intercept (following condition)
B_COND        <- -360       # sharing - following
SD_PART       <- 591        # participant random intercept
SD_STMT       <- 253        # statement random intercept
SD_COND_SLOPE <- 100        # by-statement condition slope: NOT estimated in
                            # pilot -> assumption; vary it (e.g., 50, 100, 200)
SD_RESID      <- 1009
RESID_DIST    <- "skewed"   # "normal" or "skewed" (gamma, same SD; skew = 1.41)

RATINGS_SOURCE    <- "pilot"   # "pilot" (resample) or "model" (below)
PILOT_TRIALS_FILE <- "../3_pilot_study/data/derived/analysis_trials.rds"

# Rating-generation model (used only if RATINGS_SOURCE is "model")
#   latent agreement = AGREE_MAX - AGREE_DIST * |theta_i - position_s|
#                      + statement offset + noise, rounded to 1..7
THETA_MEAN    <- 1.0        # |mean| ideal point of left / right stratum (-2..2)
THETA_SD      <- 0.6
AGREE_MAX     <- 6.0
AGREE_DIST    <- 1.3
SD_AGREE_STMT <- 0.5
SD_AGREE_NOISE<- 1.2
SD_S_NOISE    <- 1.0        # social-circle rating = own latent + this noise

# -----------------------------------------------------------------------------
# 1. STIMULI: the 88 pilot statements (11 topics x 8 statements). Position =
#    median placement by the pretest raters (1..5, centred to -2..+2), as in
#    3_pilot_study/analysis.R. Statements are ordered by topic, then ID.
# -----------------------------------------------------------------------------
PRETEST_FILE  <- "../2_stimuli_validation/data/processed/clean_data.csv"

pretest <- read.csv(PRETEST_FILE)
pretest <- pretest[pretest$response_type == "LIKERT_GRID" &
                   !is.na(pretest$target_category) &
                   pretest$target_category != "" &
                   !is.na(pretest$response_value), ]
pos_tab <- aggregate(response_value ~ component_title + topic, pretest, median)
pos_tab <- pos_tab[order(pos_tab$topic, pos_tab$component_title), ]
stopifnot(nrow(pos_tab) == 88, all(table(pos_tab$topic) == 8))
stim <- data.frame(
  stmt  = 1:88,
  id    = pos_tab$component_title,
  topic = as.integer(factor(pos_tab$topic)),
  pos   = pos_tab$response_value - 3
)

# The raw ideology score is largest when every statement gets the endpoint
# rating on its own side: 3 x mean |position|. Half of that maximum puts the
# ideal point on the -2..+2 metric of statement positions.
ideal_point_divisor <- 3 * mean(abs(stim$pos)) / 2

# Ideology score as in the manuscript: scale value x (rating - 4),
# averaged within topic, then across topics. Rescaled to the -2..+2
# metric of statement positions with ideal_point_divisor.
ideal_points <- function(pid, stmt, A) {
  item <- stim$pos[stmt] * (A - 4)
  topic_mean <- tapply(item, list(pid, stim$topic[stmt]), mean)
  rowMeans(topic_mean) / ideal_point_divisor
}

# Variance-component SD used as the denominator of d. With 0/1-coded
# condition and independent statement intercept/slope, the statement variance
# averaged over the two conditions is var(intercept) + 0.5 * var(slope).
SD_TOTAL <- sqrt(SD_PART^2 + SD_STMT^2 + 0.5 * SD_COND_SLOPE^2 + SD_RESID^2)

# Pilot ratings; strata by the sign of the ideal point (0 counts as right)
if (RATINGS_SOURCE == "pilot") {
  if (!file.exists(PILOT_TRIALS_FILE)) {
    stop("Run 3_pilot_study/analysis.R first: ", PILOT_TRIALS_FILE, " is missing.")
  }
  pilot_trials <- readRDS(PILOT_TRIALS_FILE)
  pilot_trials <- pilot_trials[!is.na(pilot_trials$agreement_personal) &
                               !is.na(pilot_trials$agreement_contacts), ]
  stopifnot(all(pilot_trials$componentTitle %in% stim$id))
  pilot_ratings <- data.frame(
    pid  = match(pilot_trials$participantId, unique(pilot_trials$participantId)),
    stmt = match(pilot_trials$componentTitle, stim$id),
    A    = pilot_trials$agreement_personal,
    S    = pilot_trials$agreement_contacts
  )
  stopifnot(all(pos_tab$topic[pilot_ratings$stmt] == pilot_trials$topic))
  pilot_rows  <- split(seq_len(nrow(pilot_ratings)), pilot_ratings$pid)
  pilot_ideal <- ideal_points(pilot_ratings$pid, pilot_ratings$stmt, pilot_ratings$A)
  pilot_left  <- as.integer(names(pilot_ideal)[pilot_ideal < 0])
  pilot_right <- as.integer(names(pilot_ideal)[pilot_ideal >= 0])
} else if (RATINGS_SOURCE != "model") {
  stop('RATINGS_SOURCE must be "pilot" or "model".')
}

# -----------------------------------------------------------------------------
# 2. RATINGS AND DERIVED VARIABLES
# -----------------------------------------------------------------------------
clip7 <- function(x) pmin(7, pmax(1, round(x)))

simulate_ratings <- function(n_per_cond) {
  n   <- 2 * n_per_cond
  # Stratified recruitment: half left, half right, balanced across conditions
  strat <- rep(c(-1, 1), each = n / 2)
  cond  <- rep(rep(c(0, 1), each = n / 4), times = 2)
  if (RATINGS_SOURCE == "model") {
    theta <- pmin(2, pmax(-2, rnorm(n, strat * THETA_MEAN, THETA_SD)))
    d <- expand.grid(stmt = stim$stmt, pid = seq_len(n))
    d$pos   <- stim$pos[d$stmt]
    d$topic <- stim$topic[d$stmt]
    stmt_off <- rnorm(88, 0, SD_AGREE_STMT)
    latA <- AGREE_MAX - AGREE_DIST * abs(theta[d$pid] - d$pos) +
            stmt_off[d$stmt] + rnorm(nrow(d), 0, SD_AGREE_NOISE)
    d$A <- clip7(latA)
    d$S <- clip7(latA + rnorm(nrow(d), 0, SD_S_NOISE))
  } else {
    pick <- c(pilot_left[sample.int(length(pilot_left), n / 2, replace = TRUE)],
              pilot_right[sample.int(length(pilot_right), n / 2, replace = TRUE)])
    rows <- pilot_rows[pick]
    d <- pilot_ratings[unlist(rows), c("stmt", "A", "S")]
    d$pid   <- rep(seq_len(n), lengths(rows))
    d$pos   <- stim$pos[d$stmt]
    d$topic <- stim$topic[d$stmt]
    rownames(d) <- NULL
  }
  d$C <- cond[d$pid]
  d
}

add_derived <- function(d) {
  ideal <- ideal_points(d$pid, d$stmt, d$A)
  d$ideal <- ideal[as.character(d$pid)]
  d$D <- abs(d$pos - d$ideal)
  d$Z <- as.numeric(sign(d$pos) == sign(d$ideal) & abs(d$pos) > abs(d$ideal))
  d$R <- 1 - d$C
  d$Am <- pmin(d$A - 4, 0); d$Ap <- pmax(d$A - 4, 0)
  d$Sm <- pmin(d$S - 4, 0); d$Sp <- pmax(d$S - 4, 0)
  d$RAm <- d$R * d$Am; d$RAp <- d$R * d$Ap
  d$CSm <- d$C * d$Sm; d$CSp <- d$C * d$Sp
  d$pid  <- factor(d$pid)
  d$stmt <- factor(d$stmt)
  d
}

# Outcome without any hypothesised effect (random effects + residual)
simulate_base_outcome <- function(d) {
  n_p <- nlevels(d$pid)
  u_p <- rnorm(n_p, 0, SD_PART)
  w0  <- rnorm(88, 0, SD_STMT)
  w1  <- rnorm(88, 0, SD_COND_SLOPE)
  e <- if (RESID_DIST == "normal") {
    rnorm(nrow(d), 0, SD_RESID)
  } else {
    (rgamma(nrow(d), shape = 2) - 2) / sqrt(2) * SD_RESID
  }
  s <- as.integer(as.character(d$stmt))
  B0 + B_COND * d$C + u_p[as.integer(d$pid)] + w0[s] + w1[s] * d$C + e
}

# -----------------------------------------------------------------------------
# 3. HYPOTHESES: effect added to the data, model fitted, success criterion
#    `b` is the effect magnitude in ms per scale point (H4: ms).
#    Success = every focal coefficient has p < ALPHA (two-sided, Wald z) and
#    the predicted sign. Focal predictors vary within participants
#    (Satterthwaite df > 8,000 in the pilot), so the z approximation is exact
#    enough and avoids slow df computations.
# -----------------------------------------------------------------------------
RE <- "(1 | pid) + (1 + C | stmt)"

hyps <- list(
  H1_H2 = list(
    label  = "H1/H2: personal-agreement slope (M1)",
    effect = function(d, b) b * d$A,
    fixed  = "A + C",
    signs  = c(A = +1),       # H2 is the mirror image (same MDE by symmetry)
    span   = 6),
  H3 = list(
    label  = "H3: inverted-V in personal agreement (M2)",
    effect = function(d, b) b * d$Am - b * d$Ap,
    fixed  = "Am + Ap + C",
    signs  = c(Am = +1, Ap = -1),
    span   = 3),
  H4 = list(
    label  = "H4: acrophily, Z controlling for D (M5)",
    effect = function(d, b) b * d$Z,
    fixed  = "D + Z + C",
    signs  = c(Z = +1),
    span   = 1),
  H5 = list(
    label  = "H5: V-shape in social-circle agreement, sharing (M3)",
    effect = function(d, b) -b * d$CSm + b * d$CSp,
    fixed  = "C + CSm + CSp",
    signs  = c(CSm = -1, CSp = +1),
    span   = 3),
  H6 = list(
    label  = "H6: H3 pattern in following + H5 pattern in sharing (M6)",
    effect = function(d, b) b * d$RAm - b * d$RAp - b * d$CSm + b * d$CSp,
    fixed  = "C + RAm + RAp + CSm + CSp",
    signs  = c(RAm = +1, RAp = -1, CSm = -1, CSp = +1),
    span   = 3)
)

ctrl <- lmerControl(calc.derivs = FALSE)

fit_focal <- function(d, y, h) {
  d$y <- y
  fit <- suppressMessages(suppressWarnings(
    lmer(as.formula(paste("y ~", h$fixed, "+", RE)), data = d,
         REML = FALSE, control = ctrl)))
  cf <- summary(fit)$coefficients[names(h$signs), , drop = FALSE]
  list(est = cf[, "Estimate"], se = cf[, "Std. Error"],
       singular = isSingular(fit))
}

# -----------------------------------------------------------------------------
# 4. WHY ONE FIT PER DATASET IS ENOUGH
#    Every hypothesised effect is a linear combination of the fitted model's
#    own fixed-effect columns (e.g., H3 adds b*Am - b*Ap to y and M2 contains
#    Am and Ap). Adding X %*% c to y leaves the (profiled) ML likelihood for
#    the variance components unchanged, so the variance estimates and SEs are
#    identical and the fixed-effect estimates shift by exactly c. Each
#    dataset is therefore fitted once without the effect, and the z values
#    for any effect size b follow exactly as
#        z_k(b) = (estimate_k + sign_k * b) / SE_k.
#    verify_shift() below confirms this numerically by refitting.
#    Success = every focal coefficient has p < ALPHA (two-sided) AND the
#    predicted sign. Focal predictors vary within participants (Satterthwaite
#    df > 8,000 in the pilot), so Wald z is used instead of t.
# -----------------------------------------------------------------------------
one_sim <- function(i) {
  set.seed(SEED + i)
  d    <- add_derived(simulate_ratings(N_PER_COND))
  base <- simulate_base_outcome(d)
  # Rating diagnostics
  diag <- data.frame(nZ_median = median(tapply(d$Z, d$pid, sum)),
                     share_A1 = mean(d$A == 1), share_A7 = mean(d$A == 7),
                     r_AS = cor(d$A, d$S))
  out <- lapply(names(hyps), function(k) {
    h <- hyps[[k]]
    r <- fit_focal(d, base, h)
    data.frame(sim = i, hyp = k, coef = names(h$signs), sign = h$signs,
               est = r$est, se = r$se, singular = r$singular, diag)
  })
  do.call(rbind, out)
}

verify_shift <- function(k, b, seed = 1) {
  set.seed(seed)
  d <- add_derived(simulate_ratings(N_PER_COND)); base <- simulate_base_outcome(d)
  h <- hyps[[k]]
  r0 <- fit_focal(d, base, h); r1 <- fit_focal(d, base + h$effect(d, b), h)
  data.frame(coef = names(h$signs),
             predicted_est = r0$est + h$signs * b, refit_est = r1$est,
             se_null = r0$se, se_refit = r1$se)
}

# -----------------------------------------------------------------------------
# 5. SIMULATION
# -----------------------------------------------------------------------------
cl <- makeCluster(N_CORES)
invisible(clusterEvalQ(cl, library(lme4)))
clusterExport(cl, ls(globalenv()))
cat(sprintf("Fitting %d models on %d cores ...\n", N_SIM * length(hyps), N_CORES))
t0  <- Sys.time()
sims <- do.call(rbind, parLapply(cl, seq_len(N_SIM), one_sim))
stopCluster(cl)
cat("Done in", format(round(Sys.time() - t0, 1)), "\n")

# -----------------------------------------------------------------------------
# 6. POWER CURVES AND MINIMUM DETECTABLE EFFECTS
# -----------------------------------------------------------------------------
z_crit <- qnorm(1 - ALPHA / 2)

power_at <- function(k, b) {
  x <- sims[sims$hyp == k, ]
  z <- (x$sign * x$est + b) / x$se          # z in the predicted direction
  mean(tapply(z > z_crit, x$sim, all))      # all focal coefficients succeed
}

mde_for <- function(k, target) {
  hi <- 1
  while (power_at(k, hi) < target) hi <- hi * 2
  uniroot(function(b) power_at(k, b) - target, c(0, hi), tol = 1e-3)$root
}

summary_tab <- do.call(rbind, lapply(names(hyps), function(k) {
  h <- hyps[[k]]
  m90 <- mde_for(k, TARGET_POWER); m80 <- mde_for(k, .80)
  data.frame(
    hypothesis   = h$label,
    unit         = ifelse(k == "H4", "ms", "ms per scale point"),
    MDE_90       = round(m90, 1),
    change_ms_90 = round(m90 * h$span, 1),
    d_90         = round(m90 * h$span / SD_TOTAL, 3),
    MDE_80       = round(m80, 1),
    d_80         = round(m80 * h$span / SD_TOTAL, 3),
    d_span       = ifelse(k == "H4", "Z = 0 vs 1",
                     ifelse(k == "H1_H2", "1 -> 7 (6 points)",
                            "midpoint -> endpoint (3 points)"))
  )
}))

# Power curves for plotting/reporting (0.25 to 2 x the 90% MDE)
curves <- do.call(rbind, lapply(names(hyps), function(k) {
  m <- mde_for(k, TARGET_POWER)
  b <- signif(m * seq(.25, 2, by = .05), 3)
  pw <- sapply(b, function(x) power_at(k, x))
  data.frame(hyp = k, b = b, d = b * hyps[[k]]$span / SD_TOTAL, power = pw,
             mc_se = sqrt(pw * (1 - pw) / N_SIM))
}))

# Statement-level secondary analysis (n = 88 posts per condition): analytic
r_mde <- function(n, power) tanh((qnorm(1 - ALPHA / 2) + qnorm(power)) / sqrt(n - 3))

cat("\nStatement positions (median pretest placement):\n")
print(table(stim$pos))
cat(sprintf("Ideal-point divisor: %.7f\n", ideal_point_divisor))
if (RATINGS_SOURCE == "pilot") {
  cat(sprintf("Ratings resampled from %d pilot participants (%d left, %d right of centre)\n",
              length(pilot_rows), length(pilot_left), length(pilot_right)))
} else {
  cat("Ratings generated from the rating model\n")
}
cat("SD_total (denominator of d):", round(SD_TOTAL), "ms\n")
cat("Median statements with Z = 1 per participant (H4 coverage):",
    median(sims$nZ_median), "\n")
cat(sprintf("Personal ratings at 1 / at 7: %.1f%% / %.1f%%; r(personal, social circle) = %.2f (medians across datasets)\n",
            100 * median(sims$share_A1), 100 * median(sims$share_A7), median(sims$r_AS)))
cat("Share of singular fits:", round(mean(sims$singular), 3), "\n\n")
print(summary_tab, row.names = FALSE)
cat(sprintf("\nStatement-level regressions (n = 88): r >= %.2f (90%%), r >= %.2f (80%%)\n",
            r_mde(88, .90), r_mde(88, .80)))

write.csv(summary_tab, "sensitivity_power_summary.csv", row.names = FALSE)
write.csv(curves,      "sensitivity_power_curves.csv",  row.names = FALSE)
write.csv(sims,        "sensitivity_power_sims.csv",    row.names = FALSE)

# Optional numerical check of the shift property (takes ~10 s):
# print(verify_shift("H6", b = 40))
