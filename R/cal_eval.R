#' cal_eval
#'
#' Streamflow calibration statistics (NSE, NSElog, PBIAS, RMSE, R2) for every run in an output directory or
#' simulated data table, against observed streamflow, daily or monthly. A streamflow-specific wrapper around
#' \code{cal_eval_generic()}: adds \code{base_flow} to \code{streamflow}, handles units, then pairs simulated
#' and observed values by date (join, not position) and computes the metrics.
#'
#' Results are returned in \strong{ParSetID order} so they line up row-for-row with parameter tables
#' (e.g. \code{defpars_list2df()}). Use \code{sort_by_nse = TRUE}, or \code{eval[order(-eval$NSE), ]}, for a ranked table.
#'
#' @param Qsim Simulated streamflow data (e.g. from \code{get_basin_daily()}) or path to an output directory
#' @param Qobs Observed streamflow data, or path to file. Needs columns \code{Date} (class Date) and \code{Flow_mmd} (mm/day).
#' @param plots Plot observed vs simulated (TRUE/FALSE)
#' @param plot_path Directory where output PDF will be written
#' @param plot_name Prefix used when naming output PDF
#' @param monthly Use monthly aggregation (TRUE/FALSE)
#' @param both Ignored (kept for backwards compatibility); call once with \code{monthly = FALSE} and once with \code{TRUE}.
#' @param echo_best Echo best run by NSE (TRUE/FALSE)
#' @param sim_units Units of simulated streamflow: "auto" (default, backwards compatible), "m" (converted to mm;
#'   output-filter basin streamflow is in m), or "mm" (no conversion).
#'   "auto" treats it as m if the max over all runs in the overlap period is < 1, which can be wrong in dry basins.
#' @param add_base Add \code{base_flow} to \code{streamflow} if present (default TRUE).
#' @param sort_by_nse Return rows sorted by NSE (best first) instead of by ParSetID (default FALSE).
#' @param log_eps Constant added before taking logs for NSElog (default 1e-6).
#'
#' @return data.table with columns \code{run, NSE, NSElog, PBIAS, RMSE, R2, ParSetID, n} (n = days/months used).
#' @export

cal_eval = function(Qsim, Qobs, plots = F, plot_path = "plots", plot_name = "cal_plots", monthly = F, both = F, echo_best = T,
                    sim_units = c("auto", "m", "mm"), add_base = TRUE, sort_by_nse = FALSE, log_eps = 1e-6) {
  sim_units = match.arg(sim_units)
  if (both) {
    warning("cal_eval: 'both' is ignored - call cal_eval() once with monthly = FALSE and once with monthly = TRUE.", call. = FALSE)
  }
  if (is.character(Qobs)) {
    cat("Reading observed streamflow data from: ", Qobs, "\n", sep = "")
    Qobs = data.table::fread(Qobs)
  }
  if (is.character(Qsim)) {
    cat("Reading simulated streamflow data from: ", Qsim, "\n", sep = "")
    Qsim = rhutils::get_basin_daily(Qsim)
  }
  if (!all(c("Date", "Flow_mmd") %in% names(Qobs))) {
    cat("Observed streamflow needs columns named 'Date' and 'Flow_mmd'.\n")
    return(NULL)
  }
  if (!"streamflow" %in% names(Qsim)) {
    cat("Simulated data missing 'streamflow' output.\n")
    return(NULL)
  }
  Qobs = data.table::as.data.table(Qobs)
  if (!inherits(Qobs$Date, "Date")) Qobs$Date = as.Date(Qobs$Date)

  sim = data.table::data.table(run = as.character(Qsim$run), date = Qsim$date, streamflow = Qsim$streamflow)
  if (add_base && "base_flow" %in% names(Qsim)) {
    sim$streamflow = sim$streamflow + Qsim$base_flow
    cat("Adding base_flow to streamflow when calculating eval stats.\n")
  }

  # ---- units ----
  if (sim_units == "m") {
    sim$streamflow = sim$streamflow * 1000
    cat("Converting simulated streamflow from m to mm.\n")
  } else if (sim_units == "auto") {
    in_overlap = sim$date >= min(Qobs$Date) & sim$date <= max(Qobs$Date)
    if (max(sim$streamflow[in_overlap], na.rm = TRUE) < 1) {
      cat("Max simulated streamflow is < 1 - assuming streamflow is in m, converting to mm (set sim_units to be explicit).\n")
      sim$streamflow = sim$streamflow * 1000
    }
  }

  # ---- evaluate (join on date) ----
  eval_df = cal_eval_generic(
    sim = sim, obs = Qobs,
    sim_value_col = "streamflow", obs_value_col = "Flow_mmd",
    sim_date_col = "date", obs_date_col = "Date",
    eval_monthly = monthly, agg_by = "mean",
    log_eps = log_eps, echo_best = FALSE, sort_by_nse = sort_by_nse
  )
  best_eval = eval_df[which.max(eval_df$NSE)]

  # ---- plots ----
  if (plots) {
    if (!dir.exists(plot_path)) {
      cat("Plot directory does not exist: ", plot_path, "\n", sep = "")
      return(eval_df)
    }
    overlap = sim$date >= min(Qobs$Date) & sim$date <= max(Qobs$Date)
    psim = sim[overlap]
    pobs = Qobs[Qobs$Date >= min(sim$date) & Qobs$Date <= max(sim$date), c("Date", "Flow_mmd")]
    if (monthly) {
      psim = psim[, list(streamflow = mean(streamflow)), by = list(run, x_time = as.Date(format(date, "%Y-%m-15")))]
      pobs = pobs[, list(Flow_mmd = mean(Flow_mmd)), by = list(x_time = as.Date(format(Date, "%Y-%m-15")))]
      plot_xlab = "Month"
    } else {
      psim$x_time = psim$date
      pobs$x_time = pobs$Date
      plot_xlab = "Date"
    }
    plot_file = file.path(plot_path, paste0(plot_name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"))
    n_runs = length(unique(psim$run))

    if (n_runs > 10) {
      plot_summary = psim[, list(
        streamflow_mean = mean(streamflow, na.rm = TRUE),
        streamflow_q10 = as.numeric(stats::quantile(streamflow, 0.10, na.rm = TRUE)),
        streamflow_q90 = as.numeric(stats::quantile(streamflow, 0.90, na.rm = TRUE))
      ), by = "x_time"]

      cal_plot_all = ggplot2::ggplot() +
        ggplot2::geom_ribbon(
          data = plot_summary,
          ggplot2::aes(x = x_time, ymin = streamflow_q10, ymax = streamflow_q90),
          fill = "steelblue", alpha = 0.2
        ) +
        ggplot2::geom_line(data = plot_summary, ggplot2::aes(x = x_time, y = streamflow_mean, color = "Sim mean"), linewidth = 1) +
        ggplot2::geom_line(data = pobs, ggplot2::aes(x = x_time, y = Flow_mmd, color = "Obs"), linewidth = 0.9) +
        ggplot2::scale_color_manual(values = c("Obs" = "black", "Sim mean" = "steelblue4")) +
        ggplot2::labs(title = "Streamflow timeseries (all runs summary)", x = plot_xlab, y = "Streamflow (mm/day)", color = NULL)
    } else {
      cal_plot_all = ggplot2::ggplot(psim) +
        ggplot2::geom_line(ggplot2::aes(x = x_time, y = streamflow, color = run), linewidth = 0.8, alpha = 0.85) +
        ggplot2::geom_line(data = pobs, ggplot2::aes(x = x_time, y = Flow_mmd), linewidth = 0.9, color = "black") +
        ggplot2::labs(title = "Streamflow timeseries (all runs)", x = plot_xlab, y = "Streamflow (mm/day)", color = "Run")
    }

    best_run_sim = psim[psim$run == best_eval$run]
    best_stats_label = paste0(
      "Run: ", best_eval$run,
      "\nNSE: ", round(best_eval$NSE, 3),
      "\nNSElog: ", round(best_eval$NSElog, 3),
      "\nPBIAS: ", round(best_eval$PBIAS, 3),
      "\nRMSE: ", round(best_eval$RMSE, 3),
      "\nR2: ", round(best_eval$R2, 3)
    )
    y_max = max(c(best_run_sim$streamflow, pobs$Flow_mmd), na.rm = TRUE)
    x_min = min(best_run_sim$x_time, na.rm = TRUE)

    cal_plot_best = ggplot2::ggplot(best_run_sim) +
      ggplot2::geom_line(ggplot2::aes(x = x_time, y = streamflow, color = "Sim"), linewidth = 1.2) +
      ggplot2::geom_line(data = pobs, ggplot2::aes(x = x_time, y = Flow_mmd, color = "Obs"), linewidth = 0.9) +
      ggplot2::scale_color_manual(values = c("Obs" = "black", "Sim" = "red")) +
      ggplot2::annotate("label", x = x_min, y = y_max, label = best_stats_label, hjust = 0, vjust = 1, size = 3,
                        fill = "white", alpha = 0.8) +
      ggplot2::labs(title = "Streamflow timeseries (best NSE run)", x = plot_xlab, y = "Streamflow (mm/day)", color = "Series")

    grDevices::pdf(plot_file, width = 11, height = 8.5)
    print(cal_plot_all)
    print(cal_plot_best)
    grDevices::dev.off()
    cat("Calibration plots written to: ", plot_file, "\n", sep = "")
  }

  if (echo_best) {
    cat("\n=== Best run by NSE ===\n")
    print(best_eval)
  }
  return(eval_df)
}
