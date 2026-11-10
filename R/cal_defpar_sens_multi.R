#' cal_defpar_sens_multi
#'
#' Sensitivity analysis across multiple sets of calibration runs, combining parameter variations and evaluation statistics.
#' Parameters may not be varied uniformly across all run sets; the function tracks the percentage of run sets each parameter is varied in.
#'
#' @param out_dirs List or vector of output directories, each containing simulation results from a calibration set
#' @param obs_source Path to observed data or data.frame (applied to all run sets)
#' @param input_def_pars_list Optional list of definition parameter lists, one per run set. If NULL, parameters are read from each out_dir.
#'   Pass the full input_def_pars of each batch (not only the varied parameters) so that parameters held constant
#'   in a batch get that batch's actual value.
#' @param monthly Use monthly aggregation (TRUE/FALSE). Default FALSE.
#' @param sortby Metric to sort by (NSE, NSElog, PBIAS, RMSE, r2). Default "NSE".
#' @param add_base Ignored (base_flow is added in \code{cal_eval()}).
#' @param def_changes_by_stat Output combined dataframe of parameters and stats (TRUE/FALSE). Default FALSE.
#' @param ... Passed to \code{cal_eval()}, e.g. \code{sim_units = "m"}.
#'
#' @details
#' This function extends \code{\link{cal_defpar_sens}} to pool multiple independent calibration batches.
#' In each batch, evaluation rows are matched to parameter rows by ParSetID. Parameters that vary in at least
#' one batch are included; in batches where such a parameter was held constant, that batch's constant value is
#' used (not a placeholder). A parameter that is missing entirely from a batch's parameter list (so its value is
#' the def file default, unknown here) is dropped from the pooled regression and reported.
#'
#' @return
#' Data frame with sensitivity analysis results, including:
#' - Columns for each metric (NSE, NSElog, PBIAS, RMSE, r2) with standardized regression coefficients
#' - Column "pct_runs_varied" showing percentage of run sets the parameter was varied in (100 if varied in all sets)
#' - Sorted by the specified metric in decreasing order of absolute value
#'
#' @examples
#' \dontrun{
#'   dirs <- c("./run_set_1/output", "./run_set_2/output", "./run_set_3/output")
#'   obs_data <- read.csv("./obs_streamflow.csv")
#'   results <- cal_defpar_sens_multi(out_dirs = dirs, obs_source = obs_data, sortby = "NSE")
#' }
#'
#' @export
cal_defpar_sens_multi = function(out_dirs, obs_source, input_def_pars_list = NULL,
                                  monthly = FALSE, sortby = "NSE", add_base = TRUE, def_changes_by_stat = FALSE, ...) {

  cat("==================== cal_defpar_sens_multi: Reading Multiple Run Sets ====================\n")

  sortops = c("NSE", "NSElog", "PBIAS", "RMSE", "r2")
  if (!sortby %in% sortops) {
    stop(paste0("sortby arg must be one of: ", paste0(sortops, collapse = " ")))
  }
  sortnum = which(sortops == sortby)

  if (!is.list(out_dirs) && !is.vector(out_dirs)) {
    stop("out_dirs must be a list or vector of directory paths")
  }
  out_dirs = as.character(out_dirs)

  if (is.null(input_def_pars_list)) {
    input_def_pars_list = rep(list(NULL), length(out_dirs))
  } else if (!is.list(input_def_pars_list)) {
    stop("input_def_pars_list must be a list of parameter lists, one per out_dir")
  }
  if (length(input_def_pars_list) != length(out_dirs)) {
    stop(sprintf("Length mismatch: %d out_dirs but %d input_def_pars_list entries",
                 length(out_dirs), length(input_def_pars_list)))
  }

  # ============================== PROCESS EACH RUN SET ==============================
  cat(sprintf("Processing %d run set(s)...\n", length(out_dirs)))

  batch_pars = list()   # numeric parameter table per batch (rows = runs, aligned to eval)
  batch_varied = list() # names of parameters varied in each batch
  all_eval_stats = list()

  for (i in seq_along(out_dirs)) {
    cat(sprintf("\n  [%d/%d] Processing: %s\n", i, length(out_dirs), out_dirs[i]))

    if (is.null(input_def_pars_list[[i]])) {
      defpar_df = read_pars_table(out_dir = out_dirs[i])
    } else if (is.list(input_def_pars_list[[i]])) {
      defpar_df = defpars_list2df(input_def_pars_list[[i]])
    } else {
      stop(sprintf("Run set %d: Invalid input_def_pars, must be NULL or a list", i))
    }
    par_ids = extract_paramset_id(names(defpar_df)[3:ncol(defpar_df)])

    defpar_df_t = as.data.frame(t(defpar_df[, c(3:ncol(defpar_df))]), stringsAsFactors = FALSE)
    names(defpar_df_t) = paste0(defpar_df$Variable, "--", defpar_df$Def_file)
    if (any(duplicated(names(defpar_df_t)))) {
      cat("    Duplicate parameter entries - keeping the last (same as change_def_file)\n")
      defpar_df_t = defpar_df_t[, !duplicated(names(defpar_df_t), fromLast = TRUE), drop = FALSE]
    }

    # numeric only (drop e.g. allocation_flag)
    numdf = as.data.frame(lapply(defpar_df_t, function(x) suppressWarnings(as.numeric(x))), check.names = FALSE)
    nacol = vapply(numdf, function(x) any(is.na(x)), logical(1))
    if (any(nacol)) {
      cat(sprintf("    Omitting %d parameter(s) with non-numeric values\n", sum(nacol)))
      numdf = numdf[, !nacol, drop = FALSE]
    }
    varied = names(numdf)[vapply(numdf, function(x) length(unique(x)) > 1, logical(1))]
    cat(sprintf("    %d parameter(s) varied, %d held constant\n", length(varied), ncol(numdf) - length(varied)))

    # evaluation stats, aligned to parameter rows by ParSetID
    sim_DT = get_basin_daily(out_dirs[i])
    if (!"streamflow" %in% names(sim_DT)) stop(sprintf("Run set %d: Must have streamflow in output.", i))
    eval = cal_eval(Qsim = sim_DT, Qobs = obs_source, monthly = monthly, ...)
    aligned = align_eval_to_pars(eval, numdf, par_ids)

    eval_i = aligned$eval
    eval_i$batch = i
    all_eval_stats[[i]] = eval_i
    batch_pars[[i]] = aligned$pars
    batch_varied[[i]] = varied
    cat(sprintf("    Read %d runs\n", nrow(aligned$pars)))
  }

  # ============================== COMBINE ACROSS RUN SETS ==============================
  cat("\n==================== Combining Run Sets ====================\n")
  all_param_names = unique(unlist(batch_varied))
  cat(sprintf("Parameters varied in at least one run set: %d\n", length(all_param_names)))

  # parameters missing entirely from some batch can't be filled - drop them
  in_all = vapply(all_param_names, function(p) all(vapply(batch_pars, function(b) p %in% names(b), logical(1))), logical(1))
  if (any(!in_all)) {
    cat("Dropping parameter(s) not in every run set's parameter list (value unknown in those sets - pass full input_def_pars):\n")
    cat(paste0("    - ", all_param_names[!in_all], collapse = "\n"), "\n")
    all_param_names = all_param_names[in_all]
  }
  if (length(all_param_names) == 0) stop("No parameters left to analyze.")

  combined_defpar_df = do.call(rbind, lapply(batch_pars, function(b) b[, all_param_names, drop = FALSE]))
  rownames(combined_defpar_df) = NULL
  combined_eval = data.table::rbindlist(all_eval_stats)

  pct_varied_by_param = vapply(all_param_names, function(p) mean(vapply(batch_varied, function(v) p %in% v, logical(1))) * 100, numeric(1))
  cat("\nParameter variation summary:\n")
  cat(sprintf("  %d parameter(s) varied in all run sets (100%%)\n", sum(pct_varied_by_param == 100)))
  partial = pct_varied_by_param < 100
  if (any(partial)) {
    cat(sprintf("  %d parameter(s) varied in some run sets (constant value of each set used elsewhere)\n", sum(partial)))
    for (pname in names(pct_varied_by_param)[partial]) {
      cat(sprintf("    - %s: %.1f%% of run sets\n", pname, pct_varied_by_param[pname]))
    }
  }
  cat(sprintf("\nTotal combined runs: %d\n", nrow(combined_eval)))

  if (def_changes_by_stat) {
    cat("*** Outputting combined dataframe of parameters and evaluation stats. ***\n")
    return(cbind(combined_eval, combined_defpar_df))
  }

  # ============================== SENSITIVITY ANALYSIS ==============================
  cat("\n==================== Parameter Sensitivity (Combined) ====================\n")
  cat("Using 'sensitivity::src()' for sensitivity analysis.\n")
  if (nrow(combined_defpar_df) <= 5 || nrow(combined_defpar_df) < ncol(combined_defpar_df)) {
    cat("WARNING: Limited total runs relative to parameter count, may lead to NAs or unstable results.\n")
  }

  sens_nse = sensitivity::src(X = combined_defpar_df, y = combined_eval$NSE)
  sens_NSElog = sensitivity::src(X = combined_defpar_df, y = combined_eval$NSElog)
  sens_PBIAS = sensitivity::src(X = combined_defpar_df, y = combined_eval$PBIAS)
  sens_RMSE = sensitivity::src(X = combined_defpar_df, y = combined_eval$RMSE)
  sens_R2 = sensitivity::src(X = combined_defpar_df, y = combined_eval$R2)

  outdf = cbind(sens_nse$SRC, sens_NSElog$SRC, sens_PBIAS$SRC, sens_RMSE$SRC, sens_R2$SRC)
  names(outdf) = sortops

  # match on make.names() of both sides, in case rownames come back mangled
  outdf$pct_runs_varied = unname(pct_varied_by_param[match(make.names(rownames(outdf)), make.names(names(pct_varied_by_param)))])

  outdf = outdf[order(abs(outdf[, sortnum]), decreasing = TRUE), ]
  cat(sprintf("\nSensitivity analysis complete. Sorted by %s.\n", sortby))
  return(outdf)
}
