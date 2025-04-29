#' cal_eval
#'
#' Takes either the paths to simulated and observed streamflow, or the dataframes/datatables themselves, and calculates
#' nse, lognse, pbias, rmse, and r2, at either daily or monthly. Requires get_basin_daily from rhutils and the datatable packges.
#' @export

cal_eval = function(Qsim, Qobs, monthly = F, echo_best = T) {
  # this should handle if you give it a path
  # it doesn't fix streamflow not including baseflow when using output filters,
  # so i usually input the data after i've fixed that, and not the path
  if (is.character(Qobs)) {
    cat("Reading observed streamflow data from: ",Qobs, "\n",sep="")
    Qobs = fread(Qobs)
  }
  if (is.character(Qsim)) {
    cat("Reading simulated streamflow data from: ",Qsim, "\n",sep="")
    Qsim = rhutils::get_basin_daily(out_dir)
  }

  if (!"Flow_mmd" %in% names(Qobs)) {
    cat("Observed streamflow needs column named 'Flow_mmd'.\n")
    return(NULL)
  }

  if (!"streamflow" %in% names(Qsim)) {
    cat("Simulated data missing 'streamflow' output.\n")
    return(NULL)
  }
  min_comb = max(min(Qsim$date), min(Qobs$Date))
  max_comb = min(max(Qsim$date), max(Qobs$Date))
  cat("Data clipped to overlapping date range from ",format(min_comb,"%Y-%m-%d")," to ",format(max_comb,"%Y-%m-%d"),".\n", sep = "")
  Qobs = Qobs[Qobs$Date >= min_comb & Qobs$Date <= max_comb,]
  Qsim = Qsim[Qsim$date >= min_comb & Qsim$date <= max_comb,]

  if (max(Qsim$streamflow) < 1) {
    cat("Max simulated streamflow is < 1 - assuming streamflow is in M, converting to mm.\n")
    Qsim$streamflow = Qsim$streamflow * 1000
  } else {
    cat("Max simulated streamflow is > 1 - assuming streamflow is already in mm, no conversion done.\n")
  }

  run_IDs = unique(Qsim$run)
  run_IDs = run_IDs[order(as.numeric(stringr::str_extract(run_IDs, "(?<=_)\\d+$")))]

  if (monthly) {
    Qsim_mn = aggregate(Qsim$streamflow, by = list(Qsim$run, Qsim$year, Qsim$month), FUN = mean)
    names(Qsim_mn) = c("run", "year","month","streamflow")
    Qobs$year = as.numeric(format(Qobs$Date, "%Y"))
    Qobs$month = as.numeric(format(Qobs$Date, "%m"))
    Qobs_mn = aggregate(Qobs$Flow_mmd, by = list(Qobs$year, Qobs$month), FUN = mean)
    names(Qobs_mn) = c("year","month","Flow_mmd")

    eval_fun_mn = function(i) {
      nse = hydroGOF::NSE(sim = Qsim_mn[Qsim_mn$run == i, "streamflow"], obs = Qobs_mn$Flow_mmd)
      r = summary(lm(Qobs_mn$Flow_mmd ~ Qsim_mn[Qsim_mn$run == i, "streamflow"] ))$r.squared
      pbias = hydroGOF::pbias(sim = Qsim_mn[Qsim_mn$run == i, "streamflow"], obs = Qobs_mn$Flow_mmd)
      nse_log = hydroGOF::NSE(sim = Qsim_mn[Qsim_mn$run == i, "streamflow"]+ 0.000001,obs = Qobs_mn$Flow_mmd+ 0.000001, fun=log)
      rmse = hydroGOF::rmse(sim = Qsim_mn[Qsim_mn$run == i, "streamflow"], obs = Qobs_mn$Flow_mmd)
      return(list(i, nse, nse_log,pbias,rmse, r))
    }
    eval_list = lapply(run_IDs, FUN = eval_fun_mn)
    eval_df = rbindlist(eval_list)
    names(eval_df) = c("run", "NSE", "NSElog", "PBIAS","RMSE", "R2")

    if (echo_best) {
      cat("\n=== Best run by NSE ===\n")
      print(eval_df[order(eval_df$NSE, decreasing = T),][1,])
    }

    return(eval_df)
  }

  eval_fun = function(i) {
    nse = hydroGOF::NSE(sim = Qsim[Qsim$run == i, streamflow], obs = Qobs$Flow_mmd)
    r = summary(lm(Qobs$Flow_mmd ~ Qsim[Qsim$run == i, streamflow] ))$r.squared
    pbias = hydroGOF::pbias(sim = Qsim[Qsim$run == i, streamflow], obs = Qobs$Flow_mmd)
    nse_log = hydroGOF::NSE(sim = Qsim[Qsim$run == i, streamflow]+ 0.000001, obs = Qobs$Flow_mmd+ 0.000001, fun=log)
    rmse = hydroGOF::rmse(sim = Qsim[Qsim$run == i, streamflow], obs = Qobs$Flow_mmd)
    return(list(i, nse, nse_log, pbias, rmse, r))
  }
  eval_list = lapply(run_IDs, FUN = eval_fun)
  eval_df = rbindlist(eval_list)
  names(eval_df) = c("run", "NSE", "NSElog", "PBIAS", "RMSE", "R2")

  if (echo_best) {
    cat("\n=== Best run by NSE ===\n")
    print(eval_df[order(eval_df$NSE, decreasing = T),][1,])
  }
  return(eval_df)
}
