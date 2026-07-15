# utils for running rhessys

# ================================================================================
#' Execute RHESSys commands in parallel
#'
#' @param run_cmds List of command strings to execute
#' @param n_cores Number of cores to use for parallel execution
#' @export
parallel_runs = function(run_cmds, n_cores = NULL) {
  # run commands must be a list where each element is a rhessys run
  if (!is.list(run_cmds)) {
    stop("run_cmds must be a list of rhessys command line calls.\n")
  }

  if (is.null(n_cores)) {
    n_cores = max(1L, parallel::detectCores() - 1L)
  }
  cl = parallel::makeCluster(n_cores)
  parallel::clusterExport(cl = cl, varlist = c("run_cmds"), envir = environment())
  parallel::parLapply(cl = cl, X = seq_along(run_cmds), fun = function(X, run_cmds) { system(run_cmds[[X]])}, run_cmds = run_cmds)
  # stop the cluster
  parallel::stopCluster(cl)
  return("Finished Runs")
}

#' Flatten the output commands and file list of run_rhessys_multi
#' 
#' @param x list of rhessys run outputs from run_rhessys_multi
#' @export
rh_out_list_flatten = function(x) {
  rh_cmd_flat = unlist(lapply(x, function(x) {x$command}), recursive = F)

  rh_files_list = lapply(x, function(x) {x$generated_files})

  rh_flattened_list <- list(
    def_files     = unlist(lapply(rh_files_list, `[[`, "def_files")),
    hdr_files     = unlist(lapply(rh_files_list, `[[`, "hdr_file")),
    tec_files     = unlist(lapply(rh_files_list, `[[`, "tec_file")),
    outputfilters = unlist(lapply(rh_files_list, `[[`, "output_filter"))
  )
  # check for non unique files and remove duplicates
  rh_flattened_list <- lapply(rh_flattened_list, function(x) {
    if (length(x) > 0) {
      x[!duplicated(x)]
    } else {
      x
    }
  })

  return(list(rh_cmds = rh_cmd_flat, rh_file_list = rh_flattened_list))
}