# Calibration helpers: ParSetID parsing, scoring, sampling, range narrowing, iteration logging

#' Extract parameter set ID (ParSetID) from run names
#'
#' The one ParSetID parser used across rhutils. The ParSetID is the trailing number after an
#' underscore, e.g. run name "Wardsoil_cal_RunID123_28" -> 28, param table column "ParSetID_28" -> 28.
#' Numeric input (or strings that are only digits) is returned as numeric unchanged.
#' A run name with no trailing "_<n>" (e.g. a single run "Wardsoil_cal_RunID123") gives NA -
#' the RunID is never mistaken for a ParSetID. Output is always the same length as the input.
#'
#' @param run_ids Character (or numeric) vector of run identifiers
#'
#' @return Numeric vector of parameter set IDs, same length as \code{run_ids}
#'
#' @export
#'
#' @examples
#' extract_paramset_id(c("Wardsoil_cal_RunID123_28", "ParSetID_15", "Wardsoil_cal_RunID123", "7"))
#'
extract_paramset_id <- function(run_ids) {
  if (is.numeric(run_ids)) {
    return(as.numeric(run_ids))
  }
  run_ids <- as.character(run_ids)
  out <- as.numeric(stringr::str_extract(run_ids, "(?<=_)\\d+$"))
  digits_only <- !is.na(run_ids) & grepl("^\\d+$", run_ids)
  out[digits_only] <- as.numeric(run_ids[digits_only])
  out
}

#' Create sensitivity lookup key
#'
#' Builds the lookup key for a Variable/Def_file pair in sensitivity results:
#' \code{make.names("Variable--Def_file")}. \code{get_sensitivity_vector()} applies the same
#' \code{make.names()} to the sensitivity row names, so keys match whether or not the
#' row names were mangled.
#'
#' @param variable Character. Parameter variable name
#' @param def_file Character. Definition file name
#'
#' @return Character. Mangled key formatted as "Variable--Def_file"
#'
#' @export
#'
#' @examples
#' make_sens_key("porosity_0", "defs.soil_loam.def")
#'
make_sens_key <- function(variable, def_file) {
  make.names(paste0(variable, "--", def_file))
}

#' Compute composite calibration score
#'
#' Combines NSE, NSElog, PBIAS, and RMSE into a single weighted score for
#' multi-objective calibration. RMSE is normalized by a fixed obs_mean
#' to ensure comparability across iterations.
#'
#' @param eval_df Data frame or data table with one row per parameter set.
#'   Must contain the columns specified in \code{*_col} parameters.
#' @param obs_mean Numeric. Mean of observed streamflow (required for RMSE
#'   normalization). Compute once from your observation source and reuse
#'   across all iterations for cross-iteration comparability.
#' @param weights Named numeric vector with weights for NSE, NSElog, PBIAS, RMSE.
#'   Names are automatically normalized by relative sum. Default is 0.4, 0.2, 0.2, 0.2.
#' @param nse_col Character. Column name for NSE values (default "NSE")
#' @param nselog_col Character. Column name for log-NSE values (default "NSElog")
#' @param pbias_col Character. Column name for percent bias (default "PBIAS")
#' @param rmse_col Character. Column name for RMSE values (default "RMSE")
#' @param pbias_scale Numeric. Scale factor for PBIAS normalization (default 25)
#' @param floor Numeric. Lower bound applied to every component (default -1), so one very poor
#'   metric (e.g. NSE = -20) can't dominate the score. NSE and NSElog were previously unbounded
#'   below; use \code{floor = -Inf} to reproduce scores logged before this change.
#'
#' @details Components: \code{NSE} and \code{NSElog} as-is, \code{PBIAS} as \code{1 - |PBIAS|/pbias_scale},
#'   \code{RMSE} as \code{1 - RMSE/obs_mean}; each is clamped to \code{[floor, 1]} and the weighted mean is returned.
#'
#' @return Numeric vector of composite scores (one per row of eval_df)
#'
#' @export
#'
#' @examples
#' # See vignette for integration example
#'
compute_composite_score <- function(
  eval_df,
  obs_mean = NULL,
  weights = c(NSE = 0.4, NSElog = 0.2, PBIAS = 0.2, RMSE = 0.2),
  nse_col = "NSE",
  nselog_col = "NSElog",
  pbias_col = "PBIAS",
  rmse_col = "RMSE",
  pbias_scale = 25,
  floor = -1
) {
  if (inherits(eval_df, "data.table")) {
    eval_df <- as.data.frame(eval_df)
  }

  components <- list()
  used_w <- c()
  clamp <- function(x) pmax(pmin(x, 1), floor)

  if (!is.na(weights["NSE"]) && weights["NSE"] > 0 && nse_col %in% names(eval_df)) {
    components$NSE <- clamp(eval_df[[nse_col]])
    used_w["NSE"] <- weights["NSE"]
  }
  if (!is.na(weights["NSElog"]) && weights["NSElog"] > 0 && nselog_col %in% names(eval_df)) {
    components$NSElog <- clamp(eval_df[[nselog_col]])
    used_w["NSElog"] <- weights["NSElog"]
  }
  if (!is.na(weights["PBIAS"]) && weights["PBIAS"] > 0 && pbias_col %in% names(eval_df)) {
    components$PBIAS <- clamp(1 - abs(eval_df[[pbias_col]]) / pbias_scale)
    used_w["PBIAS"] <- weights["PBIAS"]
  }
  if (!is.na(weights["RMSE"]) && weights["RMSE"] > 0 && rmse_col %in% names(eval_df)) {
    if (is.null(obs_mean)) {
      stop("compute_composite_score: supply obs_mean to normalize RMSE (needed for cross-iteration comparability).")
    }
    nrmse <- eval_df[[rmse_col]] / obs_mean
    components$RMSE <- clamp(1 - nrmse)
    used_w["RMSE"] <- weights["RMSE"]
  }

  if (length(components) == 0) {
    stop(
      "compute_composite_score: none of the requested metric columns were found - check *_col arguments against names(eval_df): ",
      paste(names(eval_df), collapse = ", ")
    )
  }

  used_w <- used_w / sum(used_w)
  mat <- do.call(cbind, components)
  as.numeric(mat %*% used_w[colnames(mat)])
}

#' Compute Kling-Gupta Efficiency
#'
#' Compute KGE and its components (correlation, bias ratio, variability ratio)
#' for a pair of simulated and observed time series. KGE balances correlation,
#' variability bias, and mean bias in a single metric bounded around 1.0 (perfect simulation).
#'
#' @param sim Numeric vector of simulated values
#' @param obs Numeric vector of observed values (must match length of \code{sim})
#'
#' @return Named numeric vector with elements:
#'   \item{KGE}{Kling-Gupta Efficiency}
#'   \item{r}{Pearson correlation coefficient}
#'   \item{alpha}{Variability ratio (sd_sim / sd_obs)}
#'   \item{beta}{Bias ratio (mean_sim / mean_obs)}
#'
#' @export
#'
#' @examples
#' sim <- rnorm(100, mean = 10, sd = 2)
#' obs <- sim + rnorm(100, sd = 0.5)
#' compute_kge(sim, obs)
#'
compute_kge <- function(sim, obs) {
  # thin wrapper around hydroGOF::KGE (2009 formulation), returning the components as a named vector
  k <- hydroGOF::KGE(sim = sim, obs = obs, method = "2009", out.type = "full", na.rm = TRUE)
  c(KGE = unname(k$KGE.value), r = unname(k$KGE.elements[["r"]]), alpha = unname(k$KGE.elements[["Alpha"]]),
    beta = unname(k$KGE.elements[["Beta"]]))
}

# Latin Hypercube sampling lives in RHESSysIOinR: use
#   RHESSysIOinR::IOin_def_pars_latin_hypercube(pars_list, n = n)
# (same list(Def_file, Variable, Value = c(lo, hi)) input as qmc_par_sample below).

#' Quasi-Monte Carlo (scrambled Sobol sequence) parameter sampling
#'
#' Generates a space-filling sample of parameter combinations using a scrambled Sobol
#' quasi-random (QMC) sequence. This is a sampling design, \strong{not} a Sobol
#' sensitivity-index design - for that see \code{RHESSysIOinR::IOin_def_pars_sobol_sa()}.
#' Gives better joint coverage than independent uniform draws, especially with 20+ free
#' parameters, and a design can be extended later with \code{skip} (same \code{seed}).
#'
#' @param param_ranges List of parameter specifications, each
#'   \code{list(Def_file, Variable, Value = c(lo, hi))}. Elements are used by position, so
#'   unnamed entries like \code{list(soil_def, Variable = "m", Value = c(0.1, 0.5))} work.
#'   A length-1 \code{Value} is held constant (repeated \code{n} times).
#' @param n Integer. Number of samples to draw (powers of 2 give best Sobol balance)
#' @param log_params Optional character vector of "Def_file::Variable" keys to sample
#'   log-uniformly (only for strictly positive ranges)
#' @param seed Integer. Fixed seed for reproducible scrambled sequence (default 1)
#' @param skip Integer. Drop the first \code{skip} points (use to extend a previous design, default 0)
#' @return List of same structure as \code{param_ranges} with \code{Value}
#'   replaced by \code{n} sampled values per parameter
#' @details
#' The \code{randtoolbox} package must be installed.
#'
#' @export
qmc_par_sample <- function(param_ranges, n, log_params = character(0), seed = 1, skip = 0) {
  if (!requireNamespace("randtoolbox", quietly = TRUE)) {
    stop("qmc_par_sample requires the 'randtoolbox' package.")
  }
  is_range <- vapply(param_ranges, function(p) is.numeric(p[[3]]) && length(p[[3]]) == 2, logical(1))
  d <- sum(is_range)
  if (d > 1111) stop("randtoolbox Sobol supports up to 1111 dimensions")

  out <- param_ranges
  if (d > 0) {
    # unit hypercube, scrambling = 3 (Owen + Faure-Tezuka); returns (n + skip) x d
    u <- randtoolbox::sobol(n + skip, dim = d, scrambling = 3, seed = seed)
    u <- matrix(u, ncol = d)
    if (skip > 0) u <- u[(skip + 1):(skip + n), , drop = FALSE]
  }

  j <- 0
  for (i in seq_along(param_ranges)) {
    p <- param_ranges[[i]]
    if (!is_range[i]) {
      if (length(p[[3]]) != 1) {
        stop("qmc_par_sample: Value for '", p[[2]], "' (", p[[1]], ") must be a numeric range c(lo, hi) or a single value.")
      }
      out[[i]][[3]] <- rep(p[[3]], n)
      next
    }
    j <- j + 1
    lo <- min(p[[3]])
    hi <- max(p[[3]])
    key <- paste(p[[1]], p[[2]], sep = "::")

    if (key %in% log_params) {
      if (lo <= 0) stop("Cannot log-sample non-positive range: ", key)
      out[[i]][[3]] <- exp(log(lo) + u[, j] * (log(hi) - log(lo)))
    } else {
      out[[i]][[3]] <- lo + u[, j] * (hi - lo)
    }
  }
  unmatched <- setdiff(log_params, vapply(param_ranges, function(p) paste(p[[1]], p[[2]], sep = "::"), character(1)))
  if (length(unmatched) > 0) {
    warning("qmc_par_sample: log_params not found in param_ranges: ", paste(unmatched, collapse = ", "), call. = FALSE)
  }
  out # same structure as param_ranges, but Value is now a vector of length n
}

#' Sobol sequence parameter sampling (deprecated)
#'
#' Deprecated - renamed to \code{qmc_par_sample()} so it isn't confused with the Sobol
#' sensitivity-index design in \code{RHESSysIOinR::IOin_def_pars_sobol_sa()}.
#' @param ... Passed to \code{qmc_par_sample()}
#' @export
sobol_par_sample <- function(...) {
  .Deprecated("qmc_par_sample", package = "rhutils")
  qmc_par_sample(...)
}

#' Select behavioral parameter sets
#'
#' Ranks parameter sets by a performance score and returns the top-performing
#' subset (by percentage or minimum count, whichever is larger). Used to
#' identify which runs to use for sensitivity analysis and range narrowing.
#'
#' Also attaches a \code{paramset_id} column extracted from the run string,
#' needed for downstream joins to parameter tables.
#'
#' @param eval_df Data frame or data table with one row per parameter set,
#'   must contain the score column specified by \code{score_col}
#' @param score_col Character. Column name for ranking score (default "composite")
#' @param top_pct Numeric. Fraction (0-1) of runs to retain (default 0.10 = top 10\%)
#' @param min_n Integer. Minimum number of runs to retain even if top_pct gives fewer
#'   (default 10)
#' @param run_col Character. Column name containing run identifiers (default "ParSetID")
#'
#' @return Data frame subset of \code{eval_df} with top-performing rows, sorted
#'   best-first, plus added \code{paramset_id} column
#'
#' @export
#'
#' @examples
#' # See vignette for integration example
#'
select_behavioral_runs <- function(eval_df, score_col = "composite", top_pct = 0.10, min_n = 10, run_col = "ParSetID") {
  if (inherits(eval_df, "data.table")) {
    eval_df <- as.data.frame(eval_df)
  }

  n_top <- max(min_n, ceiling(top_pct * nrow(eval_df)))
  n_top <- min(n_top, nrow(eval_df))
  ord <- order(eval_df[[score_col]], decreasing = TRUE)
  out <- eval_df[ord[seq_len(n_top)], , drop = FALSE]
  out$paramset_id <- extract_paramset_id(out[[run_col]])
  out
}

#' Extract sensitivity vector
#'
#' Pulls one metric column from a sensitivity data frame and returns it as a
#' named vector keyed by the Variable--Def_file identifiers.
#'
#' @param sens Data frame of sensitivity results (from cal_defpar_sens),
#'   with row names = Variable--Def_file keys and columns for metrics
#' @param stat Character. Which metric to extract: "NSE", "NSElog", "PBIAS",
#'   "RMSE", or "any" (default). "any" returns the max |sensitivity| across
#'   all metrics, so a parameter only qualifies as "insensitive" if it doesn't
#'   affect ANY tracked metric.
#'
#' @return Named numeric vector of sensitivity values, names are make_sens_key() keys (make.names("Variable--Def_file"))
#'
#' @export
#'
#' @examples
#' # See vignette for integration example
#'
get_sensitivity_vector <- function(sens, stat = "any") {
  if (identical(stat, "any")) {
    metric_cols <- intersect(c("NSE", "NSElog", "PBIAS", "RMSE"), colnames(sens))
    v <- apply(abs(sens[, metric_cols, drop = FALSE]), 1, max)
    names(v) <- make.names(rownames(sens))
    return(v)
  }
  if (!stat %in% colnames(sens)) {
    stop("get_sensitivity_vector: '", stat, "' not in sens columns: ", paste(colnames(sens), collapse = ", "))
  }
  v <- sens[[stat]]
  # make.names() so names match make_sens_key() (src() row names are "Variable--Def_file", unmangled)
  names(v) <- make.names(rownames(sens))
  v
}

#' Narrow calibration ranges based on behavioral runs
#'
#' Refines parameter ranges for the next iteration by:
#' 1. Selecting behavioral (top-performing) runs
#' 2. For sensitive parameters: shrinking ranges to encompass the behavioral set
#' 3. For insensitive parameters: freezing at the best run's value
#'
#' Input and output use the same list(Def_file, Variable=, Value=c(lo,hi)) structure
#' as parameter specification lists, so output can be assigned directly to
#' calibration parameter sets.
#'
#' @param parstable Data frame from defpars_list2df(), one row per parameter, with Variable, Def_file, and Run_N columns (one per parameter set)
#' @param eval_df Data frame/data table of evaluation results with composite score and metrics
#' @param current_pars_cal List of current parameter specifications (from soil_pars_cal or similar)
#' @param sens Data frame of sensitivity results (from cal_defpar_sens)
#' @param score_col Character. Column to rank on (default "composite")
#' @param sens_stat Character. Sensitivity metric to use (default "any")
#' @param top_pct Numeric. Fraction of runs to select as behavioral (default 0.10)
#' @param min_n Integer. Minimum behavioral runs (default 10)
#' @param sens_thresh Numeric. Sensitivity threshold below which to freeze parameters (default 0.01)
#' @param pad_pct Numeric. Padding to add around behavioral range, as fraction of range width (default 0.15 = 15\%)
#'
#' @return List of same structure as \code{current_pars_cal}, with narrowed or frozen \code{Value} ranges
#'
#' @export
#'
#' @examples
#' # See vignette for integration example
#'
narrow_calibration_ranges <- function(
  parstable,
  eval_df,
  current_pars_cal,
  sens,
  score_col = "composite",
  sens_stat = "any",
  top_pct = 0.10,
  min_n = 10,
  sens_thresh = 0.01,
  pad_pct = 0.15
) {
  behavioral <- select_behavioral_runs(eval_df, score_col = score_col, top_pct = top_pct, min_n = min_n)
  behav_cols <- paste0("ParSetID_", behavioral$paramset_id)
  behav_cols <- behav_cols[behav_cols %in% names(parstable)]
  if (length(behav_cols) == 0) {
    stop(
      "narrow_calibration_ranges: none of the behavioral paramset IDs matched columns in parstable - ",
      "check that eval$ParSetID lines up with parstable's ParSetID_N suffix."
    )
  }
  best_col <- paste0("ParSetID_", behavioral$paramset_id[1])

  sens_vec <- get_sensitivity_vector(sens, stat = sens_stat)

  new_pars_cal <- current_pars_cal
  for (i in seq_along(current_pars_cal)) {
    entry <- current_pars_cal[[i]]
    def_file <- entry[[1]]
    variable <- entry[[2]]

    row_i <- which(parstable$Variable == variable & parstable$Def_file == def_file)
    if (length(row_i) == 0) {
      warning("narrow_calibration_ranges: '", variable, "' / '", def_file, "' not found in parstable - left unchanged.")
      next
    }
    row_i <- row_i[1]

    behav_vals <- as.numeric(parstable[row_i, behav_cols])
    key <- make_sens_key(variable, def_file)

    if (key %in% names(sens_vec) && abs(sens_vec[[key]]) < sens_thresh) {
      new_pars_cal[[i]][[3]] <- as.numeric(parstable[row_i, best_col])
      next
    }

    lo <- min(behav_vals)
    hi <- max(behav_vals)
    pad <- (hi - lo) * (pad_pct / 2)
    orig_lo <- min(as.numeric(entry[[3]]))
    orig_hi <- max(as.numeric(entry[[3]]))
    new_lo <- max(orig_lo, lo - pad)
    new_hi <- min(orig_hi, hi + pad)
    if (new_hi <= new_lo) {
      new_lo <- lo
      new_hi <- hi
    }
    new_pars_cal[[i]][[3]] <- c(new_lo, new_hi)
  }
  new_pars_cal
}

#' Log calibration iteration results
#'
#' Records summary statistics and diagnostics for a calibration iteration
#' to disk. Call once per iteration after computing evaluation metrics and
#' composite score. Used by \code{plot_calibration_progress()} and
#' \code{suggest_next_iteration()} to track fit evolution.
#'
#' Logged entries include the parameter set ID for each best metric, allowing
#' you to identify which runs achieved optimal performance on each criterion.
#'
#' @param iteration Integer. Iteration number
#' @param eval_df Data frame/data table with evaluation results
#' @param score_col Character. Column name for ranking score (default "composite")
#' @param current_ranges List of parameter ranges from this iteration
#'   (optional, for plotting)
#' @param sens Data frame of sensitivity results (optional, for suggestions)
#' @param log_path Character. File path for RDS log file
#'   (default "robj/calibration_log.RDS")
#' @param run_col Character. Column name containing run/parameter set identifiers
#'   (default "ParSetID")
#'
#' @return Invisibly, the full log list (with new entry appended). Each entry
#'   contains:
#'   \item{best_score}{Best composite score}
#'   \item{best_score_paramset_id}{Parameter set ID achieving best composite score}
#'   \item{best_NSE, best_NSElog, best_PBIAS, best_RMSE}{Best values for each metric: highest NSE/NSElog,
#'     PBIAS closest to 0, lowest RMSE}
#'   \item{best_NSE_paramset_id, ...}{Parameter set IDs achieving best for each metric}
#'   plus iteration, timestamp, n_runs, ranges, all_eval, sens
#'
#' @export
#'
#' @examples
#' # See vignette for integration example
#'
log_calibration_iteration <- function(
  iteration,
  eval_df,
  score_col = "composite",
  current_ranges = NULL,
  sens = NULL,
  log_path = "robj/calibration_log.RDS",
  run_col = "ParSetID"
) {
  if (inherits(eval_df, "data.table")) {
    eval_df <- as.data.frame(eval_df)
  }
  best_row <- eval_df[which.max(eval_df[[score_col]]), ]

  entry <- list(
    iteration = iteration,
    timestamp = Sys.time(),
    n_runs = nrow(eval_df),
    best_score = max(eval_df[[score_col]], na.rm = TRUE),
    best_score_paramset_id = extract_paramset_id(best_row[[run_col]]),
    median_score = stats::median(eval_df[[score_col]], na.rm = TRUE),
    p75_score = as.numeric(stats::quantile(eval_df[[score_col]], 0.75, na.rm = TRUE)),
    ranges = current_ranges,
    all_eval = eval_df,
    sens = sens
  )

  for (m in intersect(c("NSE", "NSElog", "PBIAS", "RMSE"), names(eval_df))) {
    best_m_idx <- switch(m,
      PBIAS = which.min(abs(eval_df[[m]])),
      RMSE = which.min(eval_df[[m]]),
      which.max(eval_df[[m]])
    )
    entry[[paste0("best_", m)]] <- eval_df[[m]][best_m_idx]
    entry[[paste0("best_", m, "_paramset_id")]] <- extract_paramset_id(eval_df[[run_col]][best_m_idx])
  }

  dir.create(dirname(log_path), showWarnings = FALSE, recursive = TRUE)
  log <- if (file.exists(log_path)) readRDS(log_path) else list()
  log[[length(log) + 1]] <- entry
  saveRDS(log, log_path)
  invisible(log)
}

#' Plot calibration progress across iterations
#'
#' Creates a two-panel summary:
#' 1. Composite score evolution (best, 75th percentile, median)
#' 2. Parameter range shrinkage relative to iteration 1
#'
#' Read from a log file created by \code{log_calibration_iteration()}.
#'
#' @param log_path Character. File path to calibration log RDS
#'   (default "robj/calibration_log.RDS")
#'
#' @return Invisibly, a data frame with iteration summaries
#'
#' @details
#' Requires at least 2 logged iterations to display the range shrinkage panel.
#'
#' @export
#'
#' @examples
#' # See vignette for integration example
#'
plot_calibration_progress <- function(log_path = "robj/calibration_log.RDS") {
  log <- readRDS(log_path)

  it <- vapply(log, function(x) x$iteration, numeric(1))
  best <- vapply(log, function(x) x$best_score, numeric(1))
  med <- vapply(log, function(x) x$median_score, numeric(1))
  p75 <- vapply(log, function(x) x$p75_score, numeric(1))

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par))
  graphics::par(mfrow = c(1, 2))

  plot(
    it,
    best,
    type = "b",
    pch = 19,
    col = "darkgreen",
    ylim = range(c(best, med, p75), na.rm = TRUE),
    xlab = "Iteration",
    ylab = "Composite score",
    main = "Fit progress across iterations"
  )
  graphics::lines(it, p75, type = "b", pch = 17, col = "steelblue")
  graphics::lines(it, med, type = "b", pch = 1, col = "gray50")
  graphics::legend(
    "bottomright",
    legend = c("best", "75th pct", "median"),
    col = c("darkgreen", "steelblue", "gray50"),
    pch = c(19, 17, 1),
    bty = "n",
    cex = 0.8
  )

  range_widths <- lapply(log, function(x) {
    if (is.null(x$ranges)) {
      return(NULL)
    }
    rg <- x$ranges
    if (is.data.frame(rg) || is.matrix(rg)) {
      # Rows = parameters (min, max); fall back to a "param"/"parameter" column for names
      rg <- as.data.frame(rg)
      nm <- rownames(rg)
      nm_col <- intersect(c("param", "parameter", "name", "par"), names(rg))
      if (length(nm_col) > 0) {
        nm <- as.character(rg[[nm_col[1]]])
      }
      num <- rg[vapply(rg, is.numeric, logical(1))]
      if (ncol(num) < 2 || nrow(num) == 0) {
        return(NULL)
      }
      w <- abs(num[[2]] - num[[1]])
      names(w) <- nm
      return(w)
    }
    w <- vapply(
      rg,
      function(r) if (is.numeric(r) && length(r) >= 2) diff(range(r, na.rm = TRUE)) else NA_real_,
      numeric(1)
    )
    if (is.null(names(w)) || all(names(w) == "")) {
      names(w) <- paste0("p", seq_along(w))
    }
    w
  })
  range_widths <- range_widths[!vapply(range_widths, is.null, logical(1))]

  common_params <- if (length(range_widths) >= 2) {
    Reduce(intersect, lapply(range_widths, names))
  } else {
    character(0)
  }

  if (length(common_params) > 0) {
    w <- do.call(rbind, lapply(range_widths, function(x) x[common_params]))
    w_norm <- sweep(w, 2, w[1, ], "/")
    w_norm[!is.finite(w_norm)] <- NA
    cols <- grDevices::hcl.colors(ncol(w_norm), "Dark 3")
    graphics::matplot(
      seq_len(nrow(w_norm)),
      w_norm,
      type = "l",
      lty = 1,
      col = cols,
      xlab = "Iteration (with ranges logged)",
      ylab = "Range width (relative to first)",
      main = "Parameter range shrinkage"
    )
    graphics::legend(
      "topright",
      legend = common_params,
      col = cols,
      lty = 1,
      cex = 0.55,
      ncol = 2,
      bty = "n"
    )
  } else {
    plot.new()
    graphics::text(
      0.5,
      0.5,
      "Need >= 2 iterations with named\nparameter ranges logged"
    )
  }

  invisible(data.frame(iteration = it, best_score = best, median_score = med, p75_score = p75))
}

#' Suggest next calibration iteration
#'
#' Decision support for whether to continue sampling. Compares improvement
#' between the last two iterations against a threshold and reports on
#' remaining sensitivity to help deciding whether to:
#' 1. Keep sampling (improvement is steady)
#' 2. Run a confirmatory batch and stop (plateau reached)
#' 3. Switch to different parameters (all current ones insensitive)
#'
#' Requires at least 2 iterations logged.
#'
#' @param log_path Character. File path to calibration log RDS
#' @param improve_thresh Numeric. Minimum fractional improvement to continue
#'   (default 0.02 = 2\%)
#' @param sens_thresh Numeric. Sensitivity threshold for "low sensitivity"
#'   classification (default 0.05)
#' @param sens_stat Character. Which sensitivity metric to use (default "any")
#' @param current_n Integer. Current sample size; if NULL, uses last iteration's
#'   n_runs to suggest next size
#'
#' @return Invisibly, a list with improvement rate and last two iterations' data
#'
#' @details
#' Prints diagnostic text to console including suggestions.
#'
#' @export
#'
#' @examples
#' # See vignette for integration example
#'
suggest_next_iteration <- function(
  log_path = "robj/calibration_log.RDS",
  improve_thresh = 0.02,
  sens_thresh = 0.05,
  sens_stat = "any",
  current_n = NULL
) {
  log <- readRDS(log_path)
  if (length(log) < 2) {
    cat("Only", length(log), "iteration(s) logged - run at least 2 before assessing convergence.\n")
    return(invisible(NULL))
  }

  last <- log[[length(log)]]
  prev <- log[[length(log) - 1]]
  improvement <- (last$best_score - prev$best_score) / max(abs(prev$best_score), 1e-6)
  n_next <- if (!is.null(current_n)) current_n else last$n_runs

  cat(sprintf("Iteration %s: best score = %.4f (prev %.4f), change = %+.1f%%\n", last$iteration, last$best_score, prev$best_score, improvement * 100))

  if (improvement < improve_thresh) {
    cat("\nImprovement has slowed below your", improve_thresh * 100, "% threshold.\n")

    if (!is.null(last$sens)) {
      sens_vec <- get_sensitivity_vector(last$sens, stat = sens_stat)
      low <- names(sens_vec)[abs(sens_vec) < sens_thresh]
      if (length(low) >= length(sens_vec)) {
        cat("All currently varied parameters show low sensitivity to the fit metric(s).\n")
        cat("-> A plateaued fit + low sensitivity everywhere usually means remaining error\n")
        cat("   isn't explainable by these soil parameters alone. Worth considering: a different\n")
        cat("   or additional parameter group (routing, vegetation), checking forcing/climate\n")
        cat("   data quality, or revisiting model structure for this catchment.\n")
      } else if (length(low) > 0) {
        cat("Low-sensitivity parameters (candidates to freeze and drop from sampling):\n")
        print(low)
      } else {
        cat("Remaining parameters are still sensitivity-relevant - plateau may just mean\n")
        cat("you're close to the achievable optimum for this parameter set; consider a\n")
        cat("smaller confirmatory batch before stopping.\n")
      }
    } else {
      cat("No `sens` was logged for this iteration - pass sens= to log_calibration_iteration()\n")
      cat("next time so this function can tell you which parameters to fix vs. keep sampling.\n")
    }

    cat(
      "\nSuggested next step: one confirmatory batch of n =",
      max(20, round(n_next * 0.6)),
      "tightly around the current best; stop if it doesn't move.\n"
    )
  } else {
    suggested_n <- ceiling(n_next * 1.25)
    cat("\nStill improving at a meaningful rate.\n")
    cat("Suggested next iteration: keep narrowing, sample size ~", suggested_n, "\n")
  }

  invisible(list(improvement = improvement, prev = prev, last = last))
}

#' Parameter value summary for the best (top percentile) parameter sets by any statistic
#'
#' Summarise parameter values for the parameter sets whose performance statistic is in the best
#' \code{pctle} fraction of runs. Handles metric direction: higher is better for NSE/NSElog/R2/composite,
#' lower for RMSE, and closest to zero for PBIAS. (Ported from the Ward project's 3.0_cal_helpers.R.)
#'
#' @param ranges def_pars list, \code{list(Def_file, Variable, Value)} (by position, names optional), where
#'   \code{Value[i]} is the value used by parameter set ParSetID \code{i + id_offset}.
#' @param eval_df data.frame with a \code{ParSetID} column and the statistic column (e.g. from \code{cal_eval()}).
#' @param stat Column of \code{eval_df} to rank on (default "composite").
#' @param pctle Fraction (0-1] of runs to keep, e.g. 0.1 = best 10\%.
#' @param higher_better TRUE if larger values of \code{stat} are better (default: all but RMSE and PBIAS).
#' @param abs_stat TRUE to rank on \code{abs(stat)}, closest to zero is best (default: TRUE for PBIAS).
#' @param id_offset Integer. ParSetID of the first element of each \code{Value} minus 1; use the
#'   \code{runIDoffset} given to \code{run_rhessys_multi()} (default 0).
#'
#' @return data.frame: Def_file, Variable, n, min, mean, max, sd (one row per parameter).
#' @export
#'
#' @examples
#' # par_ranges_by_stat_pctle(ranges, eval_df, stat = "NSE", pctle = 0.1)
par_ranges_by_stat_pctle <- function(ranges, eval_df, stat = "composite", pctle = 0.1,
                                     higher_better = !(stat %in% c("RMSE", "PBIAS")),
                                     abs_stat = (stat == "PBIAS"),
                                     id_offset = 0) {
  if (!stat %in% names(eval_df)) stop("stat '", stat, "' not found in eval_df")
  if (pctle <= 0 || pctle > 1) stop("pctle must be in (0, 1]")
  eval_df <- as.data.frame(eval_df)

  s <- eval_df[[stat]]
  if (abs_stat) s <- abs(s)
  if (!higher_better || abs_stat) s <- -s # make "larger = better"
  ok <- !is.na(s)

  cutoff <- stats::quantile(s[ok], probs = 1 - pctle, names = FALSE)
  idx <- eval_df$ParSetID[ok & s >= cutoff] - id_offset

  out <- lapply(ranges, function(r) {
    v <- suppressWarnings(as.numeric(r[[3]][idx]))
    data.frame(
      Def_file = r[[1]], Variable = r[[2]], n = sum(!is.na(v)),
      min = suppressWarnings(min(v, na.rm = TRUE)), mean = mean(v, na.rm = TRUE),
      max = suppressWarnings(max(v, na.rm = TRUE)), sd = stats::sd(v, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

#' Check calibration outputs for missing or incomplete runs
#'
#' RHESSys runs that fail (crash, bad parameters, killed HPC jobs) leave either no output file or a
#' truncated one. This lists the output csvs in \code{out_dir}, matches them to ParSetIDs, and flags
#' runs that are missing (if \code{expected_ids} is given) or shorter than the others. Use it after
#' collecting output (especially from HPC, where exit statuses aren't returned to R) and before
#' \code{cal_eval()}.
#'
#' @param out_dir Output directory
#' @param expected_ids Optional numeric vector of expected ParSetIDs, e.g. \code{seq_len(n_sets)}
#' @param pattern Regex for the output files to check (default "_basin.*[.]csv$")
#' @param min_rows Minimum number of data rows for a complete run. Default NULL uses the largest
#'   row count found (i.e. all runs are expected to cover the same period).
#'
#' @return data.frame with ParSetID, file, n_rows, status ("ok", "short", "empty", "missing"), invisibly;
#'   a warning lists any runs that are not "ok".
#' @export
check_cal_outputs <- function(out_dir, expected_ids = NULL, pattern = "_basin.*[.]csv$", min_rows = NULL) {
  files <- list.files(out_dir, pattern = pattern, full.names = TRUE)
  ids <- extract_paramset_id(tools::file_path_sans_ext(basename(files)))
  n_rows <- vapply(files, function(f) max(length(readLines(f, warn = FALSE)) - 1L, 0L), integer(1))
  out <- data.frame(ParSetID = ids, file = basename(files), n_rows = unname(n_rows), stringsAsFactors = FALSE)
  if (is.null(min_rows)) min_rows <- if (nrow(out) > 0) max(out$n_rows) else 0
  out$status <- ifelse(out$n_rows == 0, "empty", ifelse(out$n_rows < min_rows, "short", "ok"))
  if (!is.null(expected_ids)) {
    missing_ids <- setdiff(expected_ids, out$ParSetID)
    if (length(missing_ids) > 0) {
      out <- rbind(out, data.frame(ParSetID = missing_ids, file = NA_character_, n_rows = NA_integer_, status = "missing"))
    }
  }
  out <- out[order(out$ParSetID), ]
  rownames(out) <- NULL
  bad <- out[out$status != "ok", ]
  cat("check_cal_outputs: ", sum(out$status == "ok"), " complete runs (", min_rows, " rows), ",
      nrow(bad), " incomplete/missing.\n", sep = "")
  if (nrow(bad) > 0) {
    warning("Incomplete or missing runs - ",
            paste(sprintf("ParSetID %s: %s", bad$ParSetID, bad$status), collapse = "; "), call. = FALSE)
  }
  invisible(out)
}
