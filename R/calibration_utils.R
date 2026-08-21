# Calibration utilities
# except for cal_eval
# ================================================================================
#' Save parameter sensitivity tables to a PDF
#'
#' @param pars_sens_out List of sensitivity result tables
#' @param output_path Output PDF path
#' @param pdfwidth PDF width in inches
#' @param pdfheight PDF height in inches
#' @export
pars_sens_output_tables = function(pars_sens_out, output_path = "pars_sens_tables.pdf", pdfwidth = 14, pdfheight = 14) {
  # Use fully qualified calls to avoid search path dependencies
  pdf(output_path, width = pdfwidth, height = pdfheight)
  for (i in seq_along(pars_sens_out)) {
  df <- pars_sens_out[[i]]
  tbl <- gridExtra::tableGrob(df, rows = NULL)  # no row names
  # Bold column headers
    header_gpar <- grid::gpar(fontface = "bold", fill = "#D3D3D3")
    tbl$grobs[tbl$layout$name == "colhead"] <- lapply(
      tbl$grobs[tbl$layout$name == "colhead"],
      function(g) grid::editGrob(g, gp = header_gpar)
    )
  title <- grid::textGrob(names(pars_sens_out)[i], gp = grid::gpar(fontsize = 14, fontface = "bold"))
  # grid.newpage()
  gridExtra::grid.arrange(title, tbl, ncol = 1, heights = c(0.05, 1))
  }
  dev.off()
  cat("Wrote output tables to pdf:",output_path,"\n")
}
# Calibration utilities
# except for cal_eval

# ================================================================================
#' Combine definition changes with evaluation statistics
#'
#' @param defpar_df Data frame of definition parameter changes
#' @param eval Data frame of evaluation metrics
#' @param stat Statistic name used for ordering
#' @export
def_changes_by_evaldf = function(defpar_df, eval_df, order_by_stat = TRUE, stat = "NSE") {

  # check that defpar_df has only Variable, Def_file
  if (!all(c("Variable", "Def_file") %in% names(defpar_df))) {
    stop("defpar_df must contain columns 'Variable' and 'Def_file'.")
  }  
  # check that the defpar_df and eval have the same number of runs
  if (ncol(defpar_df) - 2 != nrow(eval_df)) {
    stop("defpar_df and eval_df must have the same number of runs (columns in defpar_df minus 2 for Variable and Def_file, and rows in eval_df).")
  }

  defpar_parfile = defpar_df[,c("Variable","Def_file")]
  defpar_df_pars = defpar_df[, !names(defpar_df) %in% c("Variable","Def_file")]
  
  # remove pars that don't vary
  parsvary = apply(defpar_df_pars,1,FUN = function(X){length(unique(X)) > 1})
  defpar_parfile = defpar_parfile[parsvary,]
  defpar_df_pars = defpar_df_pars[parsvary,]

  # new version - use the ParSetID numbers, which are the trailing digits of the defpar_df column names and the value in eval$ParSetID
  # get the ParSetID from the defpar_df column names
  defpar_parsetIDs = as.numeric(stringr::str_extract(names(defpar_df_pars), "(?<=_)\\d+$"))
  if (any(is.na(defpar_parsetIDs))) {
    stop("Could not extract ParSetID from defpar_df column names. Column names must end with '_<ParSetID>'.")
  }
  if (!"ParSetID" %in% names(eval_df)) {
    stop("eval dataframe must contain a 'ParSetID' column.")
  }
  # get the ParSetID from the eval df
  eval_parsetIDs = eval_df$ParSetID
  # check that they match
  if (!all(sort(defpar_parsetIDs) == sort(eval_parsetIDs))) {
    stop("ParSetID values in defpar_df and eval do not match.")
  }
  # order the eval df by ParSetID
  eval_ordered = eval_df[match(defpar_parsetIDs, eval_parsetIDs),]
  # remove the ParSetID column from eval_ordered
  # eval_ordered_noparset = eval_ordered[, !names(eval_ordered)%in% c("ParSetID")]
  # eval_ordered_noparset <- eval_ordered[, .SD, .SDcols = !"ParSetID"]

  # transpose the eval_ordered_noparset
  evaltrans = unname(data.frame(t(eval_ordered)))
  names(evaltrans) = names(defpar_df_pars)
  statnames = rownames(evaltrans)
  rownames(evaltrans) = NULL
  parsstats = rbind(evaltrans, defpar_df_pars)

  # combine the names, duplicate the stat for thw two cols
  comb_names = rbind(data.frame(Variable = statnames,Def_file = statnames), defpar_parfile)

  # order the stats and pars by the selected stat
  if (order_by_stat) {
    if (stat %in% statnames) {
      parsstats = parsstats[,order(as.numeric(unlist(unname(parsstats[comb_names$Variable == stat,]))), decreasing = TRUE)]
    } else {
      warning(paste0("Selected stat '",stat,"' not found in the evaluation metrics. Not ordering by stat."))
    }
  }
  comb_parsstats = cbind(comb_names, parsstats)

  return(comb_parsstats)
}



# # ================================================================================
# #' Summarize definition changes ordered by evaluation metric
# #'
# #' @param out_dir Output directory containing parameter tables
# #' @param eval Data frame of evaluation metrics
# #' @param stat Statistic name used for ordering
# #' @export
# def_changes_by_eval = function(out_dir, eval, stat = "NSE") {
#   inputpars = read_pars_table(out_dir)
#   if (stat == "NSE") {
#     tmp = cbind(inputpars[,c(1,2)], inputpars[,-c(1,2)][,order(eval$NSE, decreasing = T)])
#     chgvars = tmp[ apply(tmp[,-c(1,2)],1,FUN = function(X){length(unique(X))}) > 1, ]
#     chgvars_stat = rbind(unname(c("NSE", "NSE", signif(unlist(eval[order(eval$NSE, decreasing = T),"NSE"]),3 ))),
#                          unname(c("PBIAS", "PBIAS", signif(unlist(eval[order(eval$PBIAS, decreasing = T),"PBIAS"]),3 ))),
#                          chgvars)
#   }
#   rownames(chgvars_stat) = NULL
#   return(chgvars_stat)
# }

# ================================================================================
#' Compute parameter ranges for top-performing runs
#'
#' @param defchg_nse Combined parameter/statistics table
#' @param pct Quantile threshold used to select runs
#' @param list Logical; return list instead of data frame
#' @export
par_ranges_by_pctle = function(defchg_nse, pct = .90, list = FALSE) {
  nse = as.numeric(defchg_nse[1,3:ncol(defchg_nse)])
  pctl = quantile(x = nse, probs = pct)
  pars = defchg_nse[,3:ncol(defchg_nse)]
  pars_pctle = pars[,pars[1,] >= pctl]
  ranges = data.frame(min = as.numeric(apply(pars_pctle,1,min)), max = as.numeric(apply(pars_pctle,1,max)))
  # signif(ranges, digits = 5)
  ranges = cbind(Variable = defchg_nse[,1], Def_file = defchg_nse[,2], ranges)
  cat(pct * 100, "th Percentile NSE - Ranges of Pars\n------------------------------------\n")
  print(ranges)
  if (list) {
    rangeslist = ranges[!ranges$Variable %in% c("NSE", "PBIAS", "NSElog", "RMSE", "R2"),]
    pars_list = list()
    for (i in 1:nrow(rangeslist)) {
      pars_list[[i]] = list(
        "Def_file" = rangeslist[i, "Def_file"],
        "Variable" = rangeslist[i, "Variable"],
        "Value" = c(rangeslist[i, "min"], rangeslist[i, "max"])
      )
    }
    return(pars_list)
  }
  return(ranges)
}

# ================================================================================
# #' @export
# def_changes_by_eval_stat = function(out_dir, obs_source, input_def_pars = NULL, monthly = F, sortby = "NSE", add_base = T) {

#   sortops = c("NSE", "NSElog", "PBIAS", "RMSE", "r2")
#   if (!sortby %in% sortops) {
#     stop(paste0("sortby arg must be one of: ", paste0(sortops, collapse = " ")))
#   }
#   sortnum = which(sortops == sortby)

#   # ============================== GET DEF PARS IN USABLE FORMAT ==============================
#   if (is.null(input_def_pars)) {
#     defpar_df = read_pars_table(out_dir = out_dir)
#   } else if (is.list(input_def_pars)) {
#     defpar_df = defpars_list2df(input_def_pars)
#   } else {
#     stop("Missing defpars, either use input_def_pars to input a R list, or include an 'all_def_changes' csv table in the out_dir/params dir.\n")
#   }
#   defpar_df_t = as.data.frame(t(defpar_df[,c(3:ncol(defpar_df))]))
#   names(defpar_df_t) = paste0(defpar_df$Variable,"--", defpar_df$Def_file)

#   par_lens = apply(defpar_df_t,MARGIN = 2, function(X) length(unique(X)))
#   if (any(par_lens == 1)) {
#     cat("Omitting parameters that do not vary across runs.\n")
#     defpar_df_t = defpar_df_t[,par_lens != 1 ]
#   }

#   # ============================== GET CAL EVALUATION STATS ==============================
#   sim_DT = get_basin_daily(out_dir)
#   vars = names(sim_DT)[!names(sim_DT) %in% c("day", "month", "year", "basinID", "run", "date", "wy", "yd")]
#   if (!"streamflow" %in% vars) stop("Must have streamflow in output.")
#   if ("base_flow" %in% vars & add_base){
#     sim_DT$streamflow = sim_DT$streamflow  + sim_DT$base_flow
#     cat("Adding base_flow to streamflow when calculating eval stats.")
#   } 
#   eval = cal_eval(Qsim = sim_DT, Qobs = obs_source, monthly = monthly)



#   return(chgvars_stat)
# }

# ================================================================================
# #' @export
# defpars_sens = function(defpars, eval, stat = "NSE") {

#   if (is.data.frame(defpars)) {
#     defpar_df = defpars
#   } else if (is.list(defpars)) {
#     defpar_df = defpars_list2df(defpars)
#   } else {
#     cat("Input defpar must be a list or dataframe")
#     return(NULL)
#   }
#   eval = as.data.frame(eval)

#   pars = defpar_df_t2parcols(defpar_df)

#   # remove pars that don't vary
#   parsvary = apply(pars,2,FUN = function(X){length(unique(X)) > 1})
#   pars_diff = pars[,unname(parsvary)]
#   srcout = sensitivity::src(pars_diff,eval[,stat])

#   return(srcout)
# }

