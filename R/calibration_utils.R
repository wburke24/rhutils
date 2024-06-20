# Calibration utilities
# except for cal_eval

# ================================================================================


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
