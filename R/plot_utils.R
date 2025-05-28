# Utilities for plotting

# ================================================================================
#' @export
g_legend<-function(a.gplot){
  tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# autoplot = function(DT, name = "basin_plots", dir = "plots", bitmap = F) {
#   vars = names(DT)[!names(DT) %in% c("day", "month", "year", "BasinID", "hillID", "zoneID", "patchID", "date")]
# }

# ================================================================================
# aggregates to basin monthly for now
#' @export

plotpdf_allvars = function(out_dir,
                           out_name = NULL,
                           step = "monthly",
                           pattern = "_basin",
                           aggvars = c("year", "month"),
                           pdfwidth = 7,
                           pdfheight = 7,
                           hide_legend = F,
                           summary_plots = F,
                           run_limit = 25,
                           runs = NULL) {

  cat("Finding all files matching: '",pattern,"' in dir: ",out_dir,"\n", sep="")
  files_in = list.files(path = out_dir, pattern = pattern, full.names = T)
  if (length(files_in) == 0) {
    stop("No files found at specified output directory '",out_dir,"' using pattern '",pattern,"'")
  }
  names = gsub(pattern = pattern, "",basename(files_in))
  names = gsub(".csv","",names)

  if (step == "monthly") {
    aggvars = c("year", "month")
    cat("Aggregating to monthly.\n")
  } else if (step == "yearly") {
    aggvars = c("year")
    cat("Aggregating to yearly.\n")
  } else {
    stop("Only valid steps are 'monthly' or 'yearly'")
  }

  DT_l = lapply(files_in, fread)
  vars = names(DT_l[[1]])[!names(DT_l[[1]]) %in% c("day", "month", "year", "basinID", "hillID", "zoneID", "patchID", "stratumID", "date", "sID", "run")]
  # uses dynamic aggregation function
  DT_l = mapply(agg_dyn, DT_l, names, MoreArgs = list(aggvars = aggvars, vars = vars), SIMPLIFY = F)
  DT = data.table::rbindlist(DT_l)

  if (!is.null(runs)) {
    DT = DT[DT$run %in% runs,]
  }

  # output a pdf
  if (is.null(out_name)) {
    out_name = "plots"
  }
  if (!dir.exists("plots")) {
    dir.create("plots")
  }
  # Summary plot when runs are too many
  time_var <- if (step == "monthly") "year_month" else "year"
  
  pdfname = file.path("plots", paste0(gsub(".pdf","", out_name),"_", format(Sys.time(), "%Y-%m-%d--%H-%M-%S"), ".pdf"  ) )
  pdf(pdfname, width = pdfwidth, height = pdfheight)

  # get number of runs, do summary plots if too many runs
  run_ct = length(unique(DT$run))

  for (i in seq_along(vars)) {
    # ------------------------------ SUMMARY PLOTS ------------------------------
    if (run_ct > run_limit | summary_plots) {
      # Calculate summaries
      DT_summary <- DT[, .(
          ymin = min(get(vars[i]), na.rm = TRUE),
          ymax = max(get(vars[i]), na.rm = TRUE),
          q25 = quantile(get(vars[i]), 0.25, na.rm = TRUE),
          q75 = quantile(get(vars[i]), 0.75, na.rm = TRUE),
          median = median(get(vars[i]), na.rm = TRUE),
          mean = mean(get(vars[i]), na.rm = TRUE)), by = time_var]

     tmpplot = ggplot(DT_summary, aes(x = .data[[time_var]])) +
        geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = "Min–Max"), alpha = 0.3) +
        geom_ribbon(aes(ymin = q25, ymax = q75, fill = "25th–75th Percentile"), alpha = 0.5) +
        geom_line(aes(y = mean, color = "Mean"), linewidth = 1) +
        geom_line(aes(y = median, color = "Median"), linetype = "dashed", linewidth = 1) +
        scale_fill_manual(name = "Range", values = c("Min–Max" = "grey80", "25th–75th Percentile" = "grey50")) +
        scale_color_manual(name = "Statistic", values = c("Mean" = "blue", "Median" = "black")) +
        guides(fill = guide_legend(order = 1), color = guide_legend(order = 2)) +
        ggtitle(paste0(vars[i], " (Summary)")) +
        xlab("Year or Year-Month") +
        ylab("M or kg/M2 or other")
  

  } else {
      # ------------------------------ STANDARD PLOTS ------------------------------
      tmpplot = ggplot(DT) +
        aes(
          x = .data[[time_var]],
          y = .data[[vars[i]]],
          color = as.factor(run),
          linetype = as.factor(run)
        ) +
        geom_line() +
        ggtitle(vars[i]) +
        xlab("Year or Year-Month") +
        ylab("M or kg/M2 or other") +
        labs(color = "Run", linetype = "Run")
    }

    tmpplot

    if (hide_legend) {
      tmpplot = tmpplot + theme(legend.position = "none")
    }
    plot(tmpplot)
  }
  
  suppressMessages(dev.off())

  cat("Wrote plots to PDF file: ",pdfname)
}
