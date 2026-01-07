#' cal_defpar_sens
#' 
#' Sensetivity of of calibration evaluation statistics (NSE, NSElog, PBIAS, RMSE, r2) to changes in defintion file parameters.
#' @export
cal_defpar_sens = function(out_dir, obs_source, input_def_pars = NULL, monthly = F, sortby = "NSE", add_base = T, def_changes_by_stat = F) {

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
  if ("base_flow" %in% vars & add_base){
    sim_DT$streamflow = sim_DT$streamflow  + sim_DT$base_flow
    cat("Adding base_flow to streamflow when calculating eval stats.\n")
  }
  cat("\n==================== Calibration Stats ====================\n")
  eval = cal_eval(Qsim = sim_DT, Qobs = obs_source, monthly = monthly)

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