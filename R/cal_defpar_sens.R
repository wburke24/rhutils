#' cal_defpar_sens
#' 
#' Sensetivity of of calibration evaluation statistics (NSE, NSElog, PBIAS, RMSE, r2) to changes in defintion file parameters.
#' @export
cal_defpar_sens = function(out_dir, obs_source, input_def_pars = NULL, monthly = F, sortby = "NSE", add_base = T) {

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

  # ============================== GET CAL EVALUATION STATS ==============================
  sim_DT = get_basin_daily(out_dir)
  vars = names(sim_DT)[!names(sim_DT) %in% c("day", "month", "year", "basinID", "run", "date", "wy", "yd")]
  if (!"streamflow" %in% vars) stop("Must have streamflow in output.")
  if ("base_flow" %in% vars & add_base){
    sim_DT$streamflow = sim_DT$streamflow  + sim_DT$base_flow
    cat("Adding base_flow to streamflow when calculating eval stats.")
  } 
  eval = cal_eval(Qsim = sim_DT, Qobs = obs_source, monthly = monthly)

  sens_nse = sensitivity::src(X = defpar_df_t, y = eval$NSE)
  sens_NSElog = sensitivity::src(X = defpar_df_t, y = eval$NSElog)
  sens_PBIAS = sensitivity::src(X = defpar_df_t, y = eval$PBIAS)
  sens_RMSE = sensitivity::src(X = defpar_df_t, y = eval$RMSE)
  sens_R2 = sensitivity::src(X = defpar_df_t, y = eval$R2)

  nsedf = sens_nse$SRC
  names(sens_nse$SRC)

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