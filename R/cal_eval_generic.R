#' cal_eval_generic
#'
#' Calculates calibration statistics: nse, lognse, pbias, rmse, and r2, at either daily or monthly.
#' @param sim Simulated data data.frame
#' @param obs Observed data data.frame
#' @param value_col Column name to compare (used for both sim and obs unless overridden)
#' @param sim_value_col Column name in sim data to compare. Overrides `value_col` for sim if provided.
#' @param obs_value_col Column name in obs data to compare. Overrides `value_col` for obs if provided.
#' @param date_col Column name containing dates (used for both sim and obs unless overridden)
#' @param sim_date_col Column name in sim data containing dates. Overrides `date_col` for sim if provided.
#' @param obs_date_col Column name in obs data containing dates. Overrides `date_col` for obs if provided.
#' @param eval_monthly Evaluate at monthly timestep (TRUE/FALSE)
#' @param obs_is_monthly Observed data is monthly (TRUE/FALSE)
#' @param echo_best Echo best run by NSE (TRUE/FALSE)
#' @param agg_by Aggregation method ('mean' or 'sum')
#' @export

cal_eval_generic = function(sim, obs, value_col, sim_value_col = NULL, obs_value_col = NULL,
                            date_col = "date", sim_date_col = NULL, obs_date_col = NULL,
                            eval_monthly = F, obs_is_monthly = F, echo_best = T, agg_by = "mean") {
  # -------------------- REQUIREMENTS --------------------
  # hydroGOF and data.table functions accessed via ::

  # -------------------- CHECKS --------------------
  # if observed is monthly, eval must be monthly, do this automatically
  if (obs_is_monthly & !eval_monthly) {
    cat("Observed data is monthly, setting eval_monthly = TRUE.\n")
    eval_monthly = T
  }
  if (eval_monthly) {
    cat("Evaluating at monthly timestep.\n")
    # zoo functions accessed via ::
  } else {
    cat("Evaluating at daily timestep.\n")
  }

  # Resolve per-dataset column names
  if (is.null(sim_value_col)) sim_value_col = value_col
  if (is.null(obs_value_col)) obs_value_col = value_col
  if (is.null(sim_date_col)) sim_date_col = date_col
  if (is.null(obs_date_col)) obs_date_col = date_col

  # Convert to data.frame if needed
  sim = as.data.frame(sim)
  obs = as.data.frame(obs)

  sim_has_month_cols = all(c("year", "month") %in% names(sim))

  if (!"run" %in% names(sim)) {
    stop("Simulated data must contain column 'run'.")
  }
  sim$run = as.character(sim$run)

  # Check columns
  if (!eval_monthly & !all(c(sim_date_col, sim_value_col) %in% names(sim))) {
    stop(paste0("Simulated data must contain columns '", sim_date_col, "' and '", sim_value_col, "'."))
  } else if (eval_monthly & !sim_has_month_cols & !all(c(sim_date_col, sim_value_col) %in% names(sim))) {
    stop(paste0(
      "Simulated data must contain either columns 'run', 'year', 'month', and '", sim_value_col,
      "', or columns 'run', '", sim_date_col, "', and '", sim_value_col, "'."
    ))
  } else if (eval_monthly & sim_has_month_cols & !all(c("run", "year", "month", sim_value_col) %in% names(sim))) {
    stop(paste0("Simulated data must contain columns 'run', 'year', 'month', and '", sim_value_col, "'."))
  }
  if (!obs_is_monthly & !all(c(obs_date_col, obs_value_col) %in% names(obs))) {
    stop(paste0("Observed data must contain columns '", obs_date_col, "' and '", obs_value_col, "'."))
  } else if (obs_is_monthly & !all(c("year", "month", obs_value_col) %in% names(obs))) {
    stop(paste0("Observed monthly data must contain columns 'year', 'month', and '", obs_value_col, "'."))
  }

  # Check date formats
  if (!eval_monthly | (eval_monthly & !sim_has_month_cols)) {
    if (!inherits(sim[[sim_date_col]], "Date")) {
      stop(paste0("Simulated data '", sim_date_col, "' column must be of class Date."))
    }
  }
  if (eval_monthly & sim_has_month_cols & (!is.numeric(sim$year) | !is.numeric(sim$month))) {
    stop("Simulated data 'year' and 'month' columns must be numeric.")
  }
  if (!obs_is_monthly & !inherits(obs[[obs_date_col]], "Date")) {
    stop(paste0("Observed data '", obs_date_col, "' column must be of class Date."))
  } else {
    if (obs_is_monthly & (!is.numeric(obs$year) | !is.numeric(obs$month))) {
      stop("Observed monthly data 'year' and 'month' columns must be numeric.")
    }
  }

  # ---------------------- AGGREGATE & CLIP --------------------
  # if evaluating at monthly, aggregate daily to monthly for sim, for obs if needed
  if (eval_monthly) {
    if (!sim_has_month_cols) {
      sim$year = as.numeric(format(sim[[sim_date_col]], "%Y"))
      sim$month = as.numeric(format(sim[[sim_date_col]], "%m"))
    }

    if (agg_by == "mean") {
      cat("Aggregating to monthly by mean.\n")
      sim_mn = aggregate(sim[, sim_value_col], by = list(sim$run, sim$year, sim$month), FUN = mean)
    } else if (agg_by == "sum") {
      cat("Aggregating to monthly by sum.\n")
      sim_mn = aggregate(sim[, sim_value_col], by = list(sim$run, sim$year, sim$month), FUN = sum)
    } else {
      stop("agg_by must be 'mean' or 'sum'.")
    }
    names(sim_mn) = c("run", "year", "month", sim_value_col)
    sim_mn$yearmon = zoo::as.yearmon(paste(sim_mn$year, sim_mn$month), "%Y %m")
    sim = sim_mn

    if (!obs_is_monthly) {
      obs$year = as.numeric(format(obs[[obs_date_col]], "%Y"))
      obs$month = as.numeric(format(obs[[obs_date_col]], "%m"))
      if (agg_by == "mean") {
        cat("Aggregating to monthly by mean.\n")
        obs_mn = aggregate(obs[, obs_value_col], by = list(obs$year, obs$month), FUN = mean)
      } else if (agg_by == "sum") {
        cat("Aggregating to monthly by sum.\n")
        obs_mn = aggregate(obs[, obs_value_col], by = list(obs$year, obs$month), FUN = sum)
      }
      names(obs_mn) = c("year", "month", obs_value_col)
      # obs_mn$yearmon = as.yearmon(paste(obs_mn$year, obs_mn$month), "%Y %m")
      obs = obs_mn
    }
    obs$yearmon = zoo::as.yearmon(paste(obs$year, obs$month), "%Y %m")

    # ---------------- CLIP TO OVERLAP --------------------
    min_comb = max(min(sim$yearmon), min(obs$yearmon))
    max_comb = min(max(sim$yearmon), max(obs$yearmon))
    cat("Data clipped to overlapping date range from ", format(min_comb, "%Y-%m"), " to ", format(max_comb, "%Y-%m"), ".\n", sep = "")
    obs = obs[obs$yearmon >= min_comb & obs$yearmon <= max_comb, ]
    sim = sim[sim$yearmon >= min_comb & sim$yearmon <= max_comb, ]
    if (nrow(sim) == 0 | nrow(obs) == 0) {
      stop("No overlapping months in simulated and observed data. Check data.")
    }
    # check that all months in obs are in sim, if not, remove from sim to have same entries
    if (!all(obs$yearmon %in% sim$yearmon)) {
      cat("Observed data months not all found in simulated data. Subsetting simulated to use what's available.\n")
      if (!any(obs$yearmon %in% sim$yearmon)) {
        stop("Observed months not found in simulated months. Check data.")
      } else {
        sim = sim[sim$yearmon %in% obs$yearmon, ]
        sim = sim[order(sim$year, sim$month), ]
        obs = obs[order(obs$year, obs$month), ]
        # if (nrow(sim) != nrow(obs)) {
        #   stop("After subsetting, simulated and observed data do not have the same number of rows. Check data.")
        # }
      }
    }

  } else {
    # ---------------- CLIP TO OVERLAP --------------------
    min_comb = max(min(sim[[sim_date_col]]), min(obs[[obs_date_col]]))
    max_comb = min(max(sim[[sim_date_col]]), max(obs[[obs_date_col]]))
    cat("Data clipped to overlapping date range from ", format(min_comb, "%Y-%m-%d"), " to ", format(max_comb, "%Y-%m-%d"), ".\n", sep = "")
    obs = obs[obs[[obs_date_col]] >= min_comb & obs[[obs_date_col]] <= max_comb, ]
    sim = sim[sim[[sim_date_col]] >= min_comb & sim[[sim_date_col]] <= max_comb, ]
    if (nrow(sim) == 0 | nrow(obs) == 0) {
      stop("No overlapping days in simulated and observed data. Check data.")
    }
    # check for contiguous dates, by day, assume sim has all days
    # if missing from obs, remove from sim to have same entries
    if (length(seq(min(obs[[obs_date_col]]), max(obs[[obs_date_col]]), by = "day")) != nrow(obs)) {
      cat("Observed data does not have contiguous daily dates. Subsetting simulated to use what's available.\n")
      if (!all(obs[[obs_date_col]] %in% sim[[sim_date_col]])) {
        stop("Observed dates not all found in simulated dates. Check data.")
      } else {
        sim = sim[sim[[sim_date_col]] %in% obs[[obs_date_col]], ]
        sim = sim[order(sim[[sim_date_col]]), ]
        obs = obs[order(obs[[obs_date_col]]), ]
        # if (nrow(sim) != nrow(obs)) {
        #   stop("After subsetting, simulated and observed data do not have the same number of rows. Check data.")
        # }
      }
    }
  }

  # Fix run IDs
  run_IDs = unique(sim$run)
  run_IDs = run_IDs[order(as.numeric(stringr::str_extract(run_IDs, "(?<=_)\\d+$")))]

  cat("Evaluating", length(run_IDs), "runs.\n")

  eval_fun = function(i, sim_value_col, obs_value_col) {
    nse = hydroGOF::NSE(sim = sim[sim$run == i, sim_value_col], obs = obs[, obs_value_col])
    r = summary(lm(obs[, obs_value_col] ~ sim[sim$run == i, sim_value_col]))$r.squared
    pbias = hydroGOF::pbias(sim = sim[sim$run == i, sim_value_col], obs = obs[, obs_value_col])
    nse_log = hydroGOF::NSE(
      sim = sim[sim$run == i, sim_value_col] + 0.0000001,
      obs = obs[, obs_value_col] + 0.0000001,
      fun = log
    )
    rmse = hydroGOF::rmse(sim = sim[sim$run == i, sim_value_col], obs = obs[, obs_value_col])
    return(list(i, nse, nse_log, pbias, rmse, r))
  }
  eval_list = lapply(run_IDs, FUN = eval_fun, sim_value_col = sim_value_col, obs_value_col = obs_value_col)
  eval_df = rbindlist(eval_list)
  names(eval_df) = c("run", "NSE", "NSElog", "PBIAS", "RMSE", "R2")
  if (echo_best) {
    cat("\n=== Best run by NSE ===\n")
    print(eval_df[order(eval_df$NSE, decreasing = T), ][1, ])
  }
  return(eval_df)


  # if (eval_monthly) {
    # eval_fun_mn = function(i, compare_col) {
    #     nse = hydroGOF::NSE(sim = sim_mn[sim_mn$run == i, compare_col], obs = obs_mn[, compare_col])
    #     r = summary(lm(obs_mn[, compare_col] ~ sim_mn[sim_mn$run == i, compare_col]))$r.squared
    #     pbias = hydroGOF::pbias(sim = sim_mn[sim_mn$run == i, compare_col], obs = obs_mn[, compare_col])
    #     nse_log = hydroGOF::NSE(sim = sim_mn[sim_mn$run == i, compare_col] + 0.000001, 
    #       obs = obs_mn[, compare_col] + 0.000001, fun = log)
    #     rmse = hydroGOF::rmse(sim = sim_mn[sim_mn$run == i, compare_col], obs = obs_mn[, compare_col])
    #     return(list(i, nse, nse_log, pbias, rmse, r))
    # }
    # eval_list = lapply(run_IDs, FUN = eval_fun_mn)
    # eval_df = rbindlist(eval_list)
    # names(eval_df) = c("run", "NSE", "NSElog", "PBIAS", "RMSE", "R2")
    # if (echo_best) {
    #     cat("\n=== Best run by NSE ===\n")
    #     print(eval_df[order(eval_df$NSE, decreasing = T),][1, ])
    # }
    # return(eval_df)
  # }

}

