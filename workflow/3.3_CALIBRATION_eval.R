# ============================== 3.3 CALIBRATION EVAL + NEXT ITERATION ==============================
# Evaluate one batch against observed streamflow, compute sensitivity, log the iteration, and build the
# narrowed ranges for the next batch (newpars_out, picked up by 3.1).
# Adapted from the Ward project (CARB/Ward/R/3.2_CALIBRATION.R).
library(RHESSysIOinR)
library(rhutils)

name = "MYSITE_soil_cal"
out_dir = find_most_recent_output_dir()
obs_source = "clim/MYSITE_observed_streamflow" # needs Date (Date) and Flow_mmd (mm/day) columns

runid = find_runID(out_dir, numeric = TRUE)
load(paste0("robj/", name, "_RunID", runid, ".RData"))
n_sets = length(input_def_pars[[1]][[3]])

# -------------------- Check output --------------------
# missing/truncated runs (failed RHESSys runs) are flagged here and again in cal_eval
run_check = check_cal_outputs(out_dir, expected_ids = seq_len(n_sets))

# -------------------- Eval --------------------
# rows are in ParSetID order (eval_daily[order(-NSE)] to rank); output-filter basin streamflow is in m
eval_daily = cal_eval(Qsim = out_dir, Qobs = obs_source, plots = TRUE, sim_units = "m")
eval_mn = cal_eval(Qsim = out_dir, Qobs = obs_source, monthly = TRUE, sim_units = "m")

# fixed obs mean (same gauge/period every iteration) so composite scores are comparable across iterations
obs_mean = mean(data.table::fread(obs_source)$Flow_mmd, na.rm = TRUE)
eval_daily$composite = compute_composite_score(eval_daily, obs_mean = obs_mean)

# -------------------- Sensitivity --------------------
input_def_pars_varied = get_only_varied_defpars_list(input_def_pars)
parstable = defpars_list2df(input_def_pars_varied)
sens = cal_defpar_sens(out_dir = out_dir, obs_source = obs_source, input_def_pars = input_def_pars_varied, sim_units = "m")

# -------------------- Log + progress --------------------
log_calibration_iteration(iteration = runid, eval_df = eval_daily, current_ranges = input_def_pars_varied, sens = sens)
plot_calibration_progress()
suggest_next_iteration()

# -------------------- Next iteration --------------------
# behavioral (top 10%) min/max +- padding; parameters with |SRC| < sens_thresh are frozen at the best set's value
newpars = narrow_calibration_ranges(
  parstable = parstable,
  eval_df = eval_daily,
  current_pars_cal = input_def_pars_varied,
  sens = sens,
  pad_pct = 0.25
)
defpars_list2df(newpars)

# summary of the best 10% by any metric (direction handled: RMSE low, PBIAS near 0)
par_ranges_by_stat_pctle(input_def_pars_varied, eval_daily, stat = "composite", pctle = 0.1)
par_ranges_by_stat_pctle(input_def_pars_varied, eval_daily, stat = "PBIAS", pctle = 0.1)

# sample the next batch - 3.1 uses newpars_out if it exists
newpars_out = IOin_def_pars_latin_hypercube(newpars, n = 300)
# newpars_out = qmc_par_sample(newpars, n = 256)

# perturb around the best set instead
# best = defpar_extract_by_ParSetID(input_def_pars_varied, eval_daily$ParSetID[which.max(eval_daily$composite)])
# newpars_out = IOin_def_pars_simple(best, n = 100, pct_range = 0.1)

# -------------------- Sensitivity pooled over several batches --------------------
multirun_sens = FALSE
if (multirun_sens) {
  out_dirs = c("output/CAL_RunID1_rh_out_...", "output/CAL_RunID2_rh_out_...")
  deflist = lapply(out_dirs, function(d) {
    load(paste0("robj/", name, "_RunID", find_runID(d, numeric = TRUE), ".RData"))
    input_def_pars # full list, so parameters held constant in a batch get that batch's value
  })
  sens_multi = cal_defpar_sens_multi(out_dirs = out_dirs, obs_source = obs_source, input_def_pars_list = deflist, sim_units = "m")
}
