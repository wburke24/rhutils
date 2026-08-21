#' cal_eval
#'
#' Takes either the paths to simulated and observed streamflow, or the dataframes/datatables themselves, and calculates
#' nse, lognse, pbias, rmse, and r2, at either daily or monthly. Requires get_basin_daily from rhutils and the datatable packges.
#' @param Qsim Simulated streamflow data or path to file
#' @param Qobs Observed streamflow data or path to file
#' @param plots Plot observed vs simulated (TRUE/FALSE)
#' @param plot_path Directory where output PDF will be written
#' @param plot_name Prefix used when naming output PDF
#' @param monthly Use monthly aggregation (TRUE/FALSE)
#' @param echo_best Echo best run by NSE (TRUE/FALSE)
#' @export

cal_eval = function(Qsim, Qobs, plots = F, plot_path = "plots", plot_name = "cal_plots", monthly = F, echo_best = T) {
  # this should handle if you give it a path
  # it doesn't fix streamflow not including baseflow when using output filters,
  # so i usually input the data after i've fixed that, and not the path
  if (is.character(Qobs)) {
    cat("Reading observed streamflow data from: ",Qobs, "\n",sep="")
    Qobs = data.table::fread(Qobs)
  }
  if (is.character(Qsim)) {
    cat("Reading simulated streamflow data from: ",Qsim, "\n",sep="")
    Qsim = rhutils::get_basin_daily(Qsim)
  }
  if (!"Flow_mmd" %in% names(Qobs)) {
    cat("Observed streamflow needs column named 'Flow_mmd'.\n")
    return(NULL)
  }
  if (!"streamflow" %in% names(Qsim)) {
    cat("Simulated data missing 'streamflow' output.\n")
    return(NULL)
  }
  if ("base_flow" %in% names(Qsim)) {
    Qsim$streamflow = Qsim$streamflow + Qsim$base_flow
    cat("Adding base_flow to streamflow when calculating eval stats.\n")
  }

  min_comb = max(min(Qsim$date), min(Qobs$Date))
  max_comb = min(max(Qsim$date), max(Qobs$Date))
  cat("Data clipped to overlapping date range from ",format(min_comb,"%Y-%m-%d")," to ",format(max_comb,"%Y-%m-%d"),".\n", sep = "")
  Qobs = Qobs[Qobs$Date >= min_comb & Qobs$Date <= max_comb,]
  Qsim = Qsim[Qsim$date >= min_comb & Qsim$date <= max_comb,]

  if (max(Qsim$streamflow) < 1) {
    cat("Max simulated streamflow is < 1 - assuming streamflow is in M, converting to mm.\n")
    Qsim$streamflow = Qsim$streamflow * 1000
  } else {
    cat("Max simulated streamflow is > 1 - assuming streamflow is already in mm, no conversion done.\n")
  }

  # New Terminology:
  # Run ID = complete run identifier, e.g. "Ward_cal_RunID1"
  # ParSetID = unique parameter set identifier WITHIN a run, e.g. "myrun_RUNID1_ParSetID101"
  # ParSetID will be the trailing digits per run, and either no prefix text or ParSetID
  # Run ID will always require RunIDX to limit confusion

  run_names = unique(Qsim$run)
  run_names = run_names[order(as.numeric(stringr::str_extract(run_names, "(?<=_)\\d+$")))]

  if (monthly) {
    Qsim = aggregate(Qsim$streamflow, by = list(Qsim$run, Qsim$year, Qsim$month), FUN = mean)
    names(Qsim) = c("run", "year", "month", "streamflow")
    Qobs$year = as.numeric(format(Qobs$Date, "%Y"))
    Qobs$month = as.numeric(format(Qobs$Date, "%m"))
    Qobs = aggregate(Qobs$Flow_mmd, by = list(Qobs$year, Qobs$month), FUN = mean)
    names(Qobs) = c("year", "month", "Flow_mmd")

    plot_sim = Qsim
    plot_obs = Qobs
    plot_sim$x_time = as.Date(sprintf("%04d-%02d-15", plot_sim$year, plot_sim$month))
    plot_obs$x_time = as.Date(sprintf("%04d-%02d-15", plot_obs$year, plot_obs$month))
    plot_xlab = "Month"
  } else {
    plot_sim = Qsim[, c("run", "date", "streamflow")]
    plot_obs = Qobs[, c("Date", "Flow_mmd")]
    plot_sim$x_time = plot_sim$date
    plot_obs$x_time = plot_obs$Date
    plot_xlab = "Date"
  }

  eval_fun = function(i) {
    nse = hydroGOF::NSE(sim = Qsim[Qsim$run == i, ]$streamflow, obs = Qobs$Flow_mmd)
    r = summary(lm(Qobs$Flow_mmd ~ Qsim[Qsim$run == i, ]$streamflow))$r.squared
    pbias = hydroGOF::pbias(sim = Qsim[Qsim$run == i, ]$streamflow, obs = Qobs$Flow_mmd)
    nse_log = hydroGOF::NSE(sim = Qsim[Qsim$run == i, ]$streamflow + 0.000001, obs = Qobs$Flow_mmd + 0.000001, fun = log)
    rmse = hydroGOF::rmse(sim = Qsim[Qsim$run == i, ]$streamflow, obs = Qobs$Flow_mmd)
    return(list(i, nse, nse_log, pbias, rmse, r))
  }

  eval_list = lapply(run_names, FUN = eval_fun)
  eval_df = data.table::rbindlist(eval_list)
  names(eval_df) = c("run", "NSE", "NSElog", "PBIAS", "RMSE", "R2")
  
  # try to parse/extract parsetid from run name, assuming it is the trailing digits after an underscore
  ParSetID = as.numeric(stringr::str_extract(eval_df$run, "(?<=_)\\d+$"))
  if (any(is.na(ParSetID))) {
    if (length(unique(ParSetID)) == 1) {
      cat("One unique parameter set found, missing ParSetID - setting ParSetID to 1\n", sep = "")
      ParSetID[is.na(ParSetID)] = 1
    } else {
      cat("Warning: Could not extract ParSetID from run names. ParSetID will be NA for some runs.\n")
    }
  }
  eval_df$ParSetID = ParSetID

  eval_df = eval_df[order(eval_df$NSE, decreasing = T), ]
  best_eval = eval_df[1, ]

  if (plots) {
    if (!dir.exists(plot_path)) {
      cat("Plot directory does not exist: ", plot_path, "\n", sep = "")
      return(eval_df)
    }

    plot_file = file.path(
      plot_path,
      paste0(plot_name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    )

    if (length(run_names) > 10) {
      plot_sim_dt = data.table::as.data.table(plot_sim)
      plot_summary = plot_sim_dt[, .(
        streamflow_mean = mean(streamflow, na.rm = TRUE),
        streamflow_q10 = as.numeric(stats::quantile(streamflow, 0.10, na.rm = TRUE)),
        streamflow_q90 = as.numeric(stats::quantile(streamflow, 0.90, na.rm = TRUE))
      ), by = x_time]

      cal_plot_all = ggplot2::ggplot() +
        ggplot2::geom_ribbon(
          data = plot_summary,
          ggplot2::aes(x = x_time, ymin = streamflow_q10, ymax = streamflow_q90),
          fill = "steelblue",
          alpha = 0.2
        ) +
        ggplot2::geom_line(
          data = plot_summary,
          ggplot2::aes(x = x_time, y = streamflow_mean, color = "Sim mean"),
          linewidth = 1
        ) +
        ggplot2::geom_line(
          data = plot_obs,
          ggplot2::aes(x = x_time, y = Flow_mmd, color = "Obs"),
          linewidth = 0.9
        ) +
        ggplot2::scale_color_manual(values = c("Obs" = "black", "Sim mean" = "steelblue4")) +
        ggplot2::labs(
          title = "Streamflow timeseries (all runs summary)",
          x = plot_xlab,
          y = "Streamflow (mm/day)",
          color = NULL
        )
    } else {
      cal_plot_all = ggplot2::ggplot(plot_sim) +
        ggplot2::geom_line(
          ggplot2::aes(x = x_time, y = streamflow, color = run),
          linewidth = 0.8,
          alpha = 0.85
        ) +
        ggplot2::geom_line(
          data = plot_obs,
          ggplot2::aes(x = x_time, y = Flow_mmd),
          linewidth = 0.9,
          color = "black"
        ) +
        ggplot2::labs(
          title = "Streamflow timeseries (all runs)",
          x = plot_xlab,
          y = "Streamflow (mm/day)",
          color = "Run"
        )
    }

    best_run_sim = plot_sim[plot_sim$run == best_eval$run, ]
    best_stats_label = paste0(
      "Run: ", best_eval$run,
      "\nNSE: ", round(best_eval$NSE, 3),
      "\nNSElog: ", round(best_eval$NSElog, 3),
      "\nPBIAS: ", round(best_eval$PBIAS, 3),
      "\nRMSE: ", round(best_eval$RMSE, 3),
      "\nR2: ", round(best_eval$R2, 3)
    )

    y_max = max(c(best_run_sim$streamflow, plot_obs$Flow_mmd), na.rm = TRUE)
    x_min = min(best_run_sim$x_time, na.rm = TRUE)

    cal_plot_best = ggplot2::ggplot(best_run_sim) +
      ggplot2::geom_line(
        ggplot2::aes(x = x_time, y = streamflow, color = "Sim"),
        linewidth = 1.2
      ) +
      ggplot2::geom_line(
        data = plot_obs,
        ggplot2::aes(x = x_time, y = Flow_mmd, color = "Obs"),
        linewidth = 0.9
      ) +
      ggplot2::scale_color_manual(values = c("Obs" = "black", "Sim" = "red")) +
      ggplot2::annotate(
        "label",
        x = x_min,
        y = y_max,
        label = best_stats_label,
        hjust = 0,
        vjust = 1,
        size = 3,
        # label.size = 0.2,
        fill = "white",
        alpha = 0.8
      ) +
      ggplot2::labs(
        title = "Streamflow timeseries (best NSE run)",
        x = plot_xlab,
        y = "Streamflow (mm/day)",
        color = "Series"
      )

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
