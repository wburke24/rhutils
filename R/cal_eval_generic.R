#' cal_eval_generic
#'
#' Calculates calibration statistics (NSE, log NSE, PBIAS, RMSE, R2) for every run in \code{sim}
#' against \code{obs}, at either daily or monthly timestep, for any variable/column names.
#' Simulated and observed values are paired by \strong{joining on date} (or year-month), so missing
#' observations, gaps, and runs of different lengths are handled correctly; only dates present in
#' both are used. This is the core used by \code{cal_eval()}.
#'
#' @param sim Simulated data data.frame/data.table, with a \code{run} column, a date column, and the value column.
#'   For monthly evaluation, \code{year} and \code{month} columns can be used instead of a date column.
#' @param obs Observed data data.frame. Daily: date column + value. Monthly (\code{obs_is_monthly}): \code{year},
#'   \code{month}, and value columns.
#' @param value_col Column name to compare (used for both sim and obs unless overridden)
#' @param sim_value_col Column name in sim data to compare. Overrides `value_col` for sim if provided.
#' @param obs_value_col Column name in obs data to compare. Overrides `value_col` for obs if provided.
#' @param date_col Column name containing dates (used for both sim and obs unless overridden)
#' @param sim_date_col Column name in sim data containing dates. Overrides `date_col` for sim if provided.
#' @param obs_date_col Column name in obs data containing dates. Overrides `date_col` for obs if provided.
#' @param eval_monthly Evaluate at monthly timestep (TRUE/FALSE)
#' @param obs_is_monthly Observed data is monthly (TRUE/FALSE)
#' @param echo_best Echo best run by NSE (TRUE/FALSE)
#' @param agg_by Aggregation method to monthly ('mean' or 'sum')
#' @param log_eps Constant added to sim and obs before taking logs for NSElog (default 1e-6)
#' @param sort_by_nse Logical. Return rows sorted by NSE (best first) instead of by ParSetID. Default FALSE -
#'   results are in ParSetID order so they line up with parameter tables.
#'
#' @return data.table, one row per run: \code{run, NSE, NSElog, PBIAS, RMSE, R2, ParSetID, n}, where \code{n} is the
#'   number of paired days/months used. Ordered by ParSetID (unless \code{sort_by_nse}). Runs with fewer paired
#'   values than the others (e.g. a run that crashed partway) are reported in a warning.
#' @export

cal_eval_generic = function(sim, obs, value_col = NULL, sim_value_col = NULL, obs_value_col = NULL,
                            date_col = "date", sim_date_col = NULL, obs_date_col = NULL,
                            eval_monthly = F, obs_is_monthly = F, echo_best = T, agg_by = "mean",
                            log_eps = 1e-6, sort_by_nse = FALSE) {
  # -------------------- CHECKS --------------------
  if (obs_is_monthly & !eval_monthly) {
    cat("Observed data is monthly, setting eval_monthly = TRUE.\n")
    eval_monthly = T
  }
  cat(ifelse(eval_monthly, "Evaluating at monthly timestep.\n", "Evaluating at daily timestep.\n"))
  if (!agg_by %in% c("mean", "sum")) stop("agg_by must be 'mean' or 'sum'.")
  aggfun = if (agg_by == "mean") mean else sum

  if (is.null(sim_value_col)) sim_value_col = value_col
  if (is.null(obs_value_col)) obs_value_col = value_col
  if (is.null(sim_value_col) || is.null(obs_value_col)) stop("Supply value_col, or both sim_value_col and obs_value_col.")
  if (is.null(sim_date_col)) sim_date_col = date_col
  if (is.null(obs_date_col)) obs_date_col = date_col

  sim = data.table::as.data.table(sim)
  obs = data.table::as.data.table(obs)

  if (!"run" %in% names(sim)) stop("Simulated data must contain column 'run'.")
  if (!sim_value_col %in% names(sim)) stop("Simulated data must contain column '", sim_value_col, "'.")
  if (!obs_value_col %in% names(obs)) stop("Observed data must contain column '", obs_value_col, "'.")

  sim_has_date = sim_date_col %in% names(sim)
  sim_has_month_cols = all(c("year", "month") %in% names(sim))

  # -------------------- BUILD JOIN KEYS --------------------
  # key: Date (daily) or integer year*12 + month (monthly)
  if (!eval_monthly) {
    if (!sim_has_date || !inherits(sim[[sim_date_col]], "Date")) {
      stop("Simulated data '", sim_date_col, "' column must exist and be of class Date.")
    }
    if (!obs_date_col %in% names(obs) || !inherits(obs[[obs_date_col]], "Date")) {
      stop("Observed data '", obs_date_col, "' column must exist and be of class Date.")
    }
    s = data.table::data.table(run = as.character(sim$run), tkey = sim[[sim_date_col]], sim = as.numeric(sim[[sim_value_col]]))
    o = data.table::data.table(tkey = obs[[obs_date_col]], obs = as.numeric(obs[[obs_value_col]]))
    fmt_key = function(k) format(k, "%Y-%m-%d")
  } else {
    if (sim_has_date && inherits(sim[[sim_date_col]], "Date")) {
      sy = as.integer(format(sim[[sim_date_col]], "%Y"))
      sm = as.integer(format(sim[[sim_date_col]], "%m"))
    } else if (sim_has_month_cols) {
      sy = as.integer(sim$year)
      sm = as.integer(sim$month)
    } else {
      stop("Simulated data needs a Date column '", sim_date_col, "' or numeric 'year' and 'month' columns.")
    }
    cat("Aggregating simulated to monthly by ", agg_by, ".\n", sep = "")
    s = data.table::data.table(run = as.character(sim$run), tkey = sy * 12L + sm, sim = as.numeric(sim[[sim_value_col]]))
    s = s[, list(sim = aggfun(sim)), by = c("run", "tkey")]

    if (obs_is_monthly) {
      if (!all(c("year", "month") %in% names(obs))) stop("Observed monthly data must contain columns 'year' and 'month'.")
      o = data.table::data.table(tkey = as.integer(obs$year) * 12L + as.integer(obs$month), obs = as.numeric(obs[[obs_value_col]]))
    } else {
      if (!obs_date_col %in% names(obs) || !inherits(obs[[obs_date_col]], "Date")) {
        stop("Observed data '", obs_date_col, "' column must be of class Date.")
      }
      cat("Aggregating observed to monthly by ", agg_by, ".\n", sep = "")
      o = data.table::data.table(
        tkey = as.integer(format(obs[[obs_date_col]], "%Y")) * 12L + as.integer(format(obs[[obs_date_col]], "%m")),
        obs = as.numeric(obs[[obs_value_col]])
      )
      o = o[, list(obs = aggfun(obs)), by = "tkey"]
    }
    fmt_key = function(k) sprintf("%04d-%02d", (k - 1L) %/% 12L, (k - 1L) %% 12L + 1L)
  }

  # -------------------- JOIN (inner, on date/month) --------------------
  o = o[!is.na(obs)]
  if (anyDuplicated(o$tkey)) stop("Observed data has duplicate dates/months.")
  m = merge(s, o, by = "tkey")
  m = m[!is.na(sim)]
  if (nrow(m) == 0) stop("No overlapping dates between simulated and observed data. Check data.")
  cat("Using overlapping dates from ", fmt_key(min(m$tkey)), " to ", fmt_key(max(m$tkey)), ".\n", sep = "")

  # -------------------- STATS --------------------
  eval_df = m[, list(
    NSE = hydroGOF::NSE(sim = sim, obs = obs),
    NSElog = hydroGOF::NSE(sim = sim + log_eps, obs = obs + log_eps, fun = log),
    PBIAS = hydroGOF::pbias(sim = sim, obs = obs),
    RMSE = hydroGOF::rmse(sim = sim, obs = obs),
    R2 = if (stats::sd(sim) > 0) stats::cor(sim, obs)^2 else NA_real_,
    n = .N
  ), by = "run"]

  # runs in sim with no overlap at all
  no_overlap = setdiff(unique(s$run), eval_df$run)
  if (length(no_overlap) > 0) {
    warning(length(no_overlap), " run(s) have no dates overlapping the observed data and were not evaluated: ",
            paste(no_overlap, collapse = ", "), call. = FALSE)
  }

  # ParSetID: trailing _<n> of the run name
  ParSetID = extract_paramset_id(eval_df$run)
  if (any(is.na(ParSetID))) {
    if (nrow(eval_df) == 1) {
      cat("One run found, without a ParSetID suffix - setting ParSetID to 1\n")
      ParSetID[is.na(ParSetID)] = 1
    } else {
      cat("Warning: Could not extract ParSetID from some run names. ParSetID will be NA for those runs.\n")
    }
  }
  eval_df$ParSetID = ParSetID
  data.table::setcolorder(eval_df, c("run", "NSE", "NSElog", "PBIAS", "RMSE", "R2", "ParSetID", "n"))
  eval_df = if (sort_by_nse) eval_df[order(-eval_df$NSE)] else eval_df[order(eval_df$ParSetID, eval_df$run)]

  short = eval_df$n < max(eval_df$n)
  if (any(short)) {
    warning(sum(short), " run(s) cover fewer dates than the others (incomplete/failed runs?): ",
            paste0(eval_df$run[short], " (n = ", eval_df$n[short], ")", collapse = ", "),
            " vs n = ", max(eval_df$n), call. = FALSE)
  }
  cat("Evaluated", nrow(eval_df), "runs.\n")

  if (echo_best) {
    cat("\n=== Best run by NSE ===\n")
    print(eval_df[which.max(eval_df$NSE)])
  }
  return(eval_df)
}
