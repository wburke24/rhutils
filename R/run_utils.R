# utils for running rhessys

# ================================================================================
#' @export
parallel_runs = function(run_cmds) {
  # run commands must be a list where each element is a rhessys run
  if (!is.list(run_cmds)) {
    stop("run_cmds must be a list of rhessys command line calls.\n")
  }

  library(parallel)

  n_cores = parallel::detectCores() - 1
  cl = parallel::makeCluster(n_cores)
  parallel::clusterExport(cl = cl, varlist = c("run_cmds"), envir = environment())
  parallel::parLapply(cl = cl, X = seq_along(run_cmds), fun = function(X, run_cmds) { system(run_cmds[[X]])}, run_cmds = run_cmds)
  # stop the cluster
  parallel::stopCluster(cl)
  return("Finished Runs")
}

