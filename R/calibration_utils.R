# Calibration utilities
# except for cal_eval

# ================================================================================
#' @export
def_changes_by_evaldf = function(defpar_df, eval, stat = "NSE") {
  # assume that eval is already ordered by calibration run id

  defpar_parfile = defpar_df[,c("Parameter","File")]
  defpar_df_pars = defpar_df[, names(defpar_df) != c("Parameter","File")]
  # double check they're in order, jk they always should be
  # names(defpar_df_pars) = names(defpar_df_pars)[order(as.numeric(gsub("Run_","",names(defpar_df_pars))))]

  # remove pars that don't vary
  parsvary = apply(defpar_df_pars,1,FUN = function(X){length(unique(X)) > 1})
  defpar_parfile = defpar_parfile[parsvary,]
  defpar_df_pars = defpar_df_pars[parsvary,]

  # take the non-name cols of the eval df, transpose, and put above the parameters
  # for now, use cols for names, not rownames
  evaltrans = unname(data.frame(t(eval[,-1])))
  names(evaltrans) = names(defpar_df_pars)
  statnames = rownames(evaltrans)
  rownames(evaltrans) = NULL
  parsstats = rbind( evaltrans, defpar_df_pars)

  # combine the names, duplicate the stat for thw two cols
  comb_names = rbind(data.frame(Parameter = statnames,File = statnames), defpar_parfile)

  # order the stats and pars by the selected stat
  parsstats = parsstats[,order(as.numeric(unlist(unname(parsstats[comb_names$Parameter == stat,]))), decreasing = T)]

  comb_parsstats = cbind(comb_names, parsstats)

  # if (stat == "NSE") {
  #   tmp = cbind(inputpars[,c(1,2)], inputpars[,-c(1,2)][,order(eval$NSE, decreasing = T)])
  #   chgvars = tmp[ apply(tmp[,-c(1,2)],1,FUN = function(X){length(unique(X))}) > 1, ]
  #   chgvars_stat = rbind(unname(c("NSE", "NSE", signif(unlist(eval[order(eval$NSE, decreasing = T),"NSE"]),3 ))),
  #                        unname(c("PBIAS", "PBIAS", signif(unlist(eval[order(eval$PBIAS, decreasing = T),"PBIAS"]),3 ))),
  #                        chgvars)
  # }
  # rownames(chgvars_stat) = NULL
  # return(chgvars_stat)

  return(comb_parsstats)

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
defpars_sens = function(defpars, eval, stat = "NSE") {

  if (is.data.frame(defpars)) {
    defpar_df = defpars
  } else if (is.list(defpars)) {
    defpar_df = defpars_list2df(defpars)
  } else {
    cat("Input defpar must be a list or dataframe")
    return(NULL)
  }
  eval = as.data.frame(eval)

  pars = defpar_df_t2parcols(defpar_df)

  # remove pars that don't vary
  parsvary = apply(pars,2,FUN = function(X){length(unique(X)) > 1})
  pars_diff = pars[,unname(parsvary)]
  srcout = sensitivity::src(pars_diff,eval[,stat])

  return(srcout)
}



# function(out_dir, eval) {
#   inputpars = get_param_table(out_dir)
#
#   pars = t(inputpars[,-c(1,2)])
#   colnames(pars) = paste0(inputpars$variable,"__",inputpars$def_file)
#   lenunq = apply(pars, 2, function(X) length(unique(X)))
#   inputpars_diff = pars[, lenunq > 1]
#   srcout = sensitivity::src(inputpars_diff, eval$NSE)
#   return(srcout)
# }
