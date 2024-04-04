# Calibration utilities

# ================================================================================
#' @export
cal_eval = function(sim_DT, Qobs, monthly = F) {
  if (!"Flow_mmd" %in% names(Qobs)) {
    cat("Observed streamflow needs column named 'Flow_mmd'")
    return(NULL)
  }

  if (!"streamflow" %in% names(sim_DT)) {
    cat("Simulated data missing 'streamflow' output")
    return(NULL)
  }
  min_comb = max(min(sim_DT$date), min(Qobs$Date))
  max_comb = min(max(sim_DT$date), max(Qobs$Date))
  Qobs = Qobs[Qobs$Date >= min_comb & Qobs$Date <= max_comb,]
  sim_DT = sim_DT[sim_DT$date >= min_comb & sim_DT$date <= max_comb,]
  sim_DT$streamflow = sim_DT$streamflow * 1000
  run_IDs = unique(sim_DT$run)
  run_IDs = run_IDs[order(as.numeric(gsub("[^0-9]+_", "",run_IDs)))] #

  if (monthly) {
    sim_DT_mn = aggregate(sim_DT$streamflow, by = list(sim_DT$run, sim_DT$year, sim_DT$month), FUN = mean)
    names(sim_DT_mn) = c("run", "year","month","streamflow")
    Qobs$year = as.numeric(format(Qobs$Date, "%Y"))
    Qobs$month = as.numeric(format(Qobs$Date, "%m"))
    Qobs_mn = aggregate(Qobs$Flow_mmd, by = list(Qobs$year, Qobs$month), FUN = mean)
    names(Qobs_mn) = c("year","month","Flow_mmd")

    eval_fun_mn = function(i) {
      nse = NSE(sim_DT_mn[sim_DT_mn$run == i, "streamflow"], Qobs_mn$Flow_mmd)
      r = summary(lm(Qobs_mn$Flow_mmd ~ sim_DT_mn[sim_DT_mn$run == i, "streamflow"] ))$r.squared
      pbias = hydroGOF::pbias(Qobs_mn$Flow_mmd,sim_DT_mn[sim_DT_mn$run == i, "streamflow"])
      return(list(i, nse, r, pbias))
    }
    eval_list = lapply(run_IDs, FUN = eval_fun_mn)
    eval_df = rbindlist(eval_list)
    names(eval_df) = c("run", "NSE", "R2", "PBIAS")
    return(eval_df)
  }

  eval_fun = function(i) {
    nse = NSE(sim_DT[sim_DT$run == i, streamflow], Qobs$Flow_mmd)
    r = summary(lm(Qobs$Flow_mmd ~ sim_DT[sim_DT$run == i, streamflow] ))$r.squared
    pbias = hydroGOF::pbias(Qobs$Flow_mmd,sim_DT[sim_DT$run == i, streamflow])
    return(list(i, nse, r, pbias))
  }
  eval_list = lapply(run_IDs, FUN = eval_fun)
  eval_df = rbindlist(eval_list)
  names(eval_df) = c("run", "NSE", "R2", "PBIAS")
  return(eval_df)
}

# ================================================================================
#' @export
def_changes_by_eval = function(out_dir, eval, stat = "NSE") {
  inputpars = read_pars_table(out_dir)
  if (stat == "NSE") {
    tmp = cbind(inputpars[,c(1,2)], inputpars[,-c(1,2)][,order(eval$NSE, decreasing = T)])
    chgvars = tmp[ apply(tmp[,-c(1,2)],1,FUN = function(X){length(unique(X))}) > 1, ]
    chgvars_stat = rbind(unname(c("NSE", "NSE", signif(unlist(eval[order(eval$NSE, decreasing = T),"NSE"]),3 ))),
                         unname(c("PBIAS", "PBIAS", signif(unlist(eval[order(eval$PBIAS, decreasing = T),"PBIAS"]),3 ))),
                         chgvars)
  }
  rownames(chgvars_stat) = NULL
  return(chgvars_stat)
}

# ================================================================================
#' @export
pars_sens = function(out_dir, eval) {
  inputpars = get_changed_inputpars(out_dir)
  pars = t(inputpars[,-c(1,2)])
  colnames(pars) = paste0(inputpars$variable,"__",inputpars$def_file)
  lenunq = apply(pars, 2, function(X) length(unique(X)))
  inputpars_diff = pars[, lenunq > 1]
  srcout = sensitivity::src(inputpars_diff, eval$NSE)
  return(srcout)
}
