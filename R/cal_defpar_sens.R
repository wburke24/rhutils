#' cal_defpar_sens
#' 
#' Sensetivity of of calibration evaluation statistics (NSE, NSElog, PBIAS, RMSE, r2) to changes in defintion file parameters.
#' @param out_dir Output directory containing simulation results
#' @param obs_source Path to observed data or data.frame
#' @param input_def_pars Input definition parameters list
#' @param monthly Use monthly aggregation (TRUE/FALSE)
#' @param sortby Metric to sort by (NSE, NSElog, PBIAS, RMSE, r2)
#' @param add_base Add base_flow to streamflow (TRUE/FALSE)
#' @param def_changes_by_stat Output combined dataframe of parameters and stats (TRUE/FALSE)
#' @param ... Passed to \code{cal_eval()}, e.g. \code{sim_units = "m"}.
#' @details Parameter rows and evaluation rows are matched by ParSetID (parameter table column suffix
#'   \code{ParSetID_<n>} vs. \code{eval$ParSetID}) before the regression. Parameter sets with no
#'   evaluated output (missing/failed runs) are dropped and reported.
#' @export
cal_defpar_sens = function(out_dir, obs_source, input_def_pars = NULL, monthly = F, sortby = "NSE", def_changes_by_stat = F, ...) {

  cat("==================== Reading Data ====================\n")
  sortops = c("NSE", "NSElog", "PBIAS", "RMSE", "r2")
  if (!sortby %in% sortops) {
    stop(paste0("sortby arg must be one of: ", paste0(sortops, collapse = " ")))
  }
  sortnum = which(sortops == sortby)

  # ============================== GET DEF PARS IN USABLE FORMAT ==============================
  if (is.null(input_def_pars)) {
    defpar_df = read_pars_table(out_dir = out_dir)
  } else if (is.list(input_def_pars)) {
    defpar_df = defpars_list2df(input_def_pars)
  } else {
    stop("Missing defpars, either use input_def_pars to input a R list, or include an 'all_def_changes' csv table in the out_dir/params dir.\n")
  }

  defpar_df_t = as.data.frame(t(defpar_df[,c(3:ncol(defpar_df))]))
  names(defpar_df_t) = paste0(defpar_df$Variable,"--", defpar_df$Def_file)
  # ParSetID of each row (from the ParSetID_<n> column names), kept before any type conversion
  par_ids = extract_paramset_id(names(defpar_df)[3:ncol(defpar_df)])

  par_lens = apply(defpar_df_t,MARGIN = 2, function(X) length(unique(X)))
  if (any(par_lens == 1)) {
    cat("Omitting parameters that do not vary across runs.\n")
    defpar_df_t = defpar_df_t[,par_lens != 1 ]
  }

  # DEF PARS  - MAKE SURE ALL ARE NUMERIC, CAN'T HANDLE CHARACTER INPUTS FOR SOME REASON
  nacol <- rep(FALSE, ncol(defpar_df_t))
  if (any(apply(defpar_df_t,2, is.character))) {
    cat("Converting parameter table from character to numeric.\n")
    dfnew = as.data.frame(apply(defpar_df_t,2, function(x) suppressWarnings(as.numeric(x)), simplify = F))
    nacol = apply(dfnew, 2, function(x) any(is.na(x)) )
    if (any(nacol)) {
      cat(
        "Some or all of parameter table contained character values not convertible to numeric, e.g., allocation_flag.\n",
        "Omitting for sensitivity analysis.\n"
      )
      # for here, replace with original data, later remove
      dfnew[,which(nacol)] = defpar_df_t[,which(nacol)] 
    }
    defpar_df_t = dfnew
  }


  # ============================== GET CAL EVALUATION STATS ==============================
  sim_DT = get_basin_daily(out_dir)
  vars = names(sim_DT)[!names(sim_DT) %in% c("day", "month", "year", "basinID", "run", "date", "wy", "yd")]
  if (!"streamflow" %in% vars) stop("Must have streamflow in output.")
  if (!"base_flow" %in% vars) { 
    warning("No base_flow in output, so not adding to streamflow for evaluation stats.") 
  }
  cat("\n==================== Calibration Stats ====================\n")
  eval = cal_eval(Qsim = sim_DT, Qobs = obs_source, monthly = monthly, ...)

  # ---- align eval rows to parameter rows by ParSetID ----
  aligned = align_eval_to_pars(eval, defpar_df_t, par_ids)
  eval = aligned$eval
  defpar_df_t = aligned$pars

  # cols are stats and params, rows are runs, harder to read this one
  if (def_changes_by_stat) {
    cat("*** Outputting combined dataframe of parameters and evaluation stats. ***\n")
    eval_par_comb = cbind(eval, defpar_df_t)
    return(eval_par_comb)
  }

  # USING SRC - STANDARDIZED REGRESSION COEFFICIENTS
  # IF N RUNS IS TOO SMALL, WILL GET NAS IN RESPONSE DATA
  cat("\n==================== Parameter Sensitivity ====================\n")
  cat("Using 'sensitivity::src()' for sensitivity analysis.\n")
  if (nrow(defpar_df_t) <= 5 || nrow(defpar_df_t) < ncol(defpar_df_t)) {
    cat("WARNING: Limited runs, relative to parameter count, may lead to NAs in output. Consider additional runs.\n")
  }

  if (any(nacol)) {
  # for here, replace with original data, later remove
  defpar_df_t = defpar_df_t[,-which(nacol)] 
  }
 
  testing = F
  if (testing) {
    X = defpar_df_t
    colnames(X) = paste0("par",1:ncol(X))
    str(X)
    
    X = as.data.frame(apply(X,2, function(v) suppressWarnings(as.numeric(v)), simplify = F))
    str(X)
    signif(X)

    y = eval$NSE
    
    rank = FALSE
    logistic = FALSE
    nboot = 0
    conf = 0.95

    data <- data.frame(Y = y, X)

    names(data)
    names(defpar_df_t)

    if (nboot == 0) {
        src <- data.frame(original = sensitivity:::estim.src(data, logistic))
        rownames(src) <- colnames(X)
    }
    out <- list(X = X, y = y, rank = rank, nboot = nboot, conf = conf, 
        call = match.call())
    class(out) <- "src"
    if (!rank) {
        out$SRC <- src
    }


  }

  sens_nse = sensitivity::src(X = defpar_df_t, y = eval$NSE)
  sens_NSElog = sensitivity::src(X = defpar_df_t, y = eval$NSElog)
  sens_PBIAS = sensitivity::src(X = defpar_df_t, y = eval$PBIAS)
  sens_RMSE = sensitivity::src(X = defpar_df_t, y = eval$RMSE)
  sens_R2 = sensitivity::src(X = defpar_df_t, y = eval$R2)


  # almost certain this doesn't need to be ordered or anything
  outdf = cbind(
    sens_nse$SRC,
    sens_NSElog$SRC,
    sens_PBIAS$SRC,
    sens_RMSE$SRC,
    sens_R2$SRC
  )
  names(outdf) = sortops
  
  outdf = outdf[order(abs(outdf[,sortnum]), decreasing = T ),]

  return(outdf)
}
# internal: match eval rows (by eval$ParSetID) to parameter table rows (par_ids), dropping
# parameter sets that have no evaluation (missing/failed runs) and evaluations with no parameters
align_eval_to_pars = function(eval, pars, par_ids) {
  if (length(par_ids) != nrow(pars)) stop("align_eval_to_pars: par_ids must have one entry per parameter row.")
  if (any(is.na(par_ids))) {
    stop("Could not get ParSetIDs from the parameter table column names (expected 'ParSetID_<n>').")
  }
  if (any(is.na(eval$ParSetID))) stop("Some eval rows have no ParSetID - cannot match evaluations to parameter sets.")
  idx = match(par_ids, eval$ParSetID)
  no_eval = par_ids[is.na(idx)]
  no_pars = setdiff(eval$ParSetID, par_ids)
  if (length(no_eval) > 0) {
    warning(length(no_eval), " parameter set(s) have no evaluated output and are omitted from the sensitivity: ParSetID ",
            paste(no_eval, collapse = ", "), call. = FALSE)
  }
  if (length(no_pars) > 0) {
    warning(length(no_pars), " evaluated run(s) have no matching parameter set and are omitted: ParSetID ",
            paste(no_pars, collapse = ", "), call. = FALSE)
  }
  keep = !is.na(idx)
  list(eval = eval[idx[keep], ], pars = pars[keep, , drop = FALSE], par_ids = par_ids[keep])
}
