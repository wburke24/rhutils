#' cal_eval_generic
#'
#' Takes either the paths to simulated and observed GENERIC DATA he dataframes/datatables themselves, and calculates
#' nse, lognse, pbias, rmse, and r2, at either daily or monthly. Requires get_basin_daily from rhutils and the datatable packges.
#' @export

cal_eval_generic = function(sim, obs, col, monthly = F, echo_best = T) {
  # this should handle if you give it a path
  # it doesn't fix  not including baseflow when using output filters,
  # so i usually input the data after i've fixed that, and not the path
  if (is.character(obs)) {
    cat("Reading observed data from: ",obs, "\n",sep="")
    obs = fread(obs)
  }
  if (is.character(sim)) {
    cat("Reading simulated data from: ",sim, "\n",sep="")
    sim = rhutils::get_basin_daily(out_dir)
  }

  min_comb = max(min(sim$date), min(obs$Date))
  max_comb = min(max(sim$date), max(obs$Date))
  cat("Data clipped to overlapping date range from ",format(min_comb,"%Y-%m-%d")," to ",format(max_comb,"%Y-%m-%d"),".\n", sep = "")
  obs = obs[obs$Date >= min_comb & obs$Date <= max_comb,]
  sim = sim[sim$date >= min_comb & sim$date <= max_comb,]

  run_IDs = unique(sim$run)
  run_IDs = run_IDs[order(as.numeric(stringr::str_extract(run_IDs, "(?<=_)\\d+$")))]

  if (monthly) {
    Qsim_mn = aggregate(sim[,col], by = list(sim$run, sim$year, sim$month), FUN = mean)
    names(Qsim_mn) = c("run", "year","month",col)
    obs$year = as.numeric(format(obs$Date, "%Y"))
    obs$month = as.numeric(format(obs$Date, "%m"))
    Qobs_mn = aggregate(obs$Flow_mmd, by = list(obs$year, obs$month), FUN = mean)
    names(Qobs_mn) = c("year","month","Flow_mmd")

    eval_fun_mn = function(i) {
      nse = hydroGOF::NSE(sim = Qsim_mn[Qsim_mn$run == i, col], obs = Qobs_mn$Flow_mmd)
      r = summary(lm(Qobs_mn$Flow_mmd ~ Qsim_mn[Qsim_mn$run == i, col] ))$r.squared
      pbias = hydroGOF::pbias(sim = Qsim_mn[Qsim_mn$run == i, col], obs = Qobs_mn$Flow_mmd)
      nse_log = hydroGOF::NSE(sim = Qsim_mn[Qsim_mn$run == i, col]+ 0.000001,obs = Qobs_mn$Flow_mmd+ 0.000001, fun=log)
      rmse = hydroGOF::rmse(sim = Qsim_mn[Qsim_mn$run == i, col], obs = Qobs_mn$Flow_mmd)
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
    nse = hydroGOF::NSE(sim = sim[sim$run == i, col], obs = obs$Flow_mmd)
    r = summary(lm(obs$Flow_mmd ~ sim[sim$run == i, col] ))$r.squared
    pbias = hydroGOF::pbias(sim = sim[sim$run == i, col], obs = obs$Flow_mmd)
    nse_log = hydroGOF::NSE(sim = sim[sim$run == i, col]+ 0.000001, obs = obs$Flow_mmd+ 0.000001, fun=log)
    rmse = hydroGOF::rmse(sim = sim[sim$run == i, col], obs = obs$Flow_mmd)
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
