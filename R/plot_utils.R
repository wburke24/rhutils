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
                           runs = NULL) {

  files_in = list.files(path = out_dir, pattern = pattern, full.names = T)
  if (length(files_in) == 0) {
    stop("No files found at specified output directory '",out_dir,"' using pattern '",pattern,"'")
  }
  names = gsub(pattern = pattern, "",basename(files_in))
  names = gsub(".csv","",names)

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
  pdfname = file.path("plots", paste0(gsub(".pdf","", out_name), gsub( ":", ".", sub( " ", "_", Sys.time())), ".pdf"  ))
  pdf(pdfname, width = pdfwidth, height = pdfheight)
  for (i in seq_along(vars)) {
    tmpplot = ggplot(DT) +
      aes(x = year_month,
          y = .data[[vars[i]]],
          color = as.factor(run),
          linetype = as.factor(run)) +
      geom_line() +
      ggtitle(vars[i]) +
      xlab("Year Month") +
      ylab("M or kg/M2 or other") +
      labs(color = "Run", linetype = "Run")
    tmpplot

    if (hide_legend) {
      tmpplot = tmpplot + theme(legend.position="none")
    }

    plot(tmpplot)
  }
  suppressMessages(dev.off())

  cat("Wrote plots to PDF file: ",pdfname)
}
