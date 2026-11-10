# ============================== 3.2 CALIBRATION RUN ==============================
# Runs the batch built in 3.1_CALIBRATION_inputs.R, either locally (parallel) or on an HPC.
# Adapted from the Ward project (CARB/Ward/R/3.2_CALIBRATION.R).
library(RHESSysIOinR)
library(rhutils)

# load the batch from 3.1 (or source 3.1 directly)
name = "MYSITE_soil_cal"
runid = GetRunID()
load(paste0("robj/", name, "_RunID", runid, ".RData"))

# -------------------- Local (parallel) --------------------
runlocal = FALSE
if (runlocal) {
  # returns ParSetID + RHESSys exit status for each run, failed runs are reported in a warning.
  # NOTE: parallel workers use the *installed* RHESSysIOinR, so reinstall after updating the package.
  run_status = run_rhessys_multi(
    input_rhessys = input_rhessys,
    hdr_files = input_hdr,
    def_pars = input_def_pars,
    tec_data = input_tec_data,
    output_filter = output_filter,
    parallel = TRUE,
    n_cores = parallel::detectCores() - 1
  )
  out_dir = collect_output(basename = paste0(name, "_RunID", runid, "_"))
}

# -------------------- HPC --------------------
hpc = FALSE
if (hpc) {
  # write all inputs and return the commands without running
  rh_out = run_rhessys_multi(
    input_rhessys = input_rhessys,
    hdr_files = input_hdr,
    def_pars = input_def_pars,
    tec_data = input_tec_data,
    output_filter = output_filter,
    return_cmd = TRUE,
    return_file_list = TRUE
  )
  rh_out_flat = rh_out_list_flatten(rh_out)
  file_list = build_rh_input_file_list(
    generated_files = rh_out_flat$rh_file_list,
    input_rhessys = input_rhessys,
    input_hdr = input_hdr,
    input_tec_data = input_tec_data
  )
  test_hpc_ssh(user = "USER@HOST", key = "~/rsa_key")
  cat(" ===", length(rh_out_flat$rh_cmds), "commands to transfer to HPC === \n")
  cmd_file = transfer_to_hpc(
    rh_commands = rh_out_flat$rh_cmds,
    rh_file_list = file_list,
    name = name,
    remote_user = "USER@HOST",
    remote_dest = "/path/on/hpc/PROJECT/",
    rhessys_binary_remote = "/RHESSys/rhessys/rhessys7.5",
    transfer_method = "rsync",
    ssh_key = "~/rsa_key",
    submit_job = TRUE,
    job_script = "./scripts/run_job.sh"
  )
}

# -------------------- Get HPC output --------------------
gethpc = FALSE
if (gethpc) {
  out_dir = getHPC(usr = "USER@HOST", dest = "/path/on/hpc/PROJECT/", ssh_key = "~/rsa_key",
                   collect_output_script = "./scripts/collect_output.sh")
  # rename to include the RunID
  new_out_dir = file.path(dirname(out_dir), paste0("CAL_", find_runID(out_dir), "_", basename(out_dir)))
  if (!dir.exists(new_out_dir)) {
    file.rename(out_dir, new_out_dir)
    out_dir = new_out_dir
  }
}

# exit statuses don't come back from the HPC - check for missing/truncated output instead
# run_check = check_cal_outputs(out_dir, expected_ids = seq_along(input_def_pars[[1]][[3]]))

# -------------------- Single run (debugging) --------------------
singlerun = FALSE
if (singlerun) {
  run_rhessys_single(
    input_rhessys = input_rhessys,
    hdr_files = input_hdr,
    def_pars = defpar_extract_by_ParSetID(input_def_pars, 1),
    tec_data = input_tec_data,
    output_filter = output_filter,
    write_run_metadata = TRUE
  )
  out_dir = collect_output()
}
