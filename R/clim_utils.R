# clim utils

# ================================================================================
#' Repeat a climate file n times
#'
#' @param clim Path to climate file or data.frame
#' @param file Output file prefix
#' @param n Number of repetitions
#' @param units Units for repetition (e.g., "years")
#' @export
clim_repeat = function(clim, file, n, units = "years") {

  if (is.character(clim)) {
    climdf = RHESSysIOinR::read_clim(clim_in = clim)
  } else if (is.data.frame(clim)) {
    climdf = clim
  }
  # trim to get only whole wy
  climdf = climdf[min(which(climdf$month == 10 & climdf$day == 1)):max(which(climdf$month == 9 & climdf$day == 30)),]
  # if years, make it a little long and trim at end, otherwise it's fine
  if (units %in% c("years", "year","Years", "Year")) {
    nyears = ceiling(lubridate::time_length(difftime(max(climdf$date), min(climdf$date)), "years"))
    nrep = ceiling(n/nyears)
  } else {
    nrep = n
  }
  dfout = do.call("rbind", replicate(nrep, climdf, simplify = FALSE))
  dfout$date = seq.POSIXt(from = min(dfout$date), by = "DSTday", length.out = length(dfout$date))
  dfout$date = as.POSIXlt(dfout$date)
  dfout$year = dfout$date$year + 1900
  dfout$month = dfout$date$mon + 1
  dfout$day = dfout$date$mday
  dfout$date = as.POSIXct(dfout$date)
  dfout$wy = data.table::fifelse(dfout$month >= 10, dfout$year + 1, dfout$year)
  if (units %in% c("years", "year","Years", "Year")) {
    dfout = dfout[1:which(dfout$month == 9 & dfout$day == 30 & dfout$wy == (min(dfout$wy) + n - 1)),]
  }

  header = paste(dfout$year[1], dfout$month[1], dfout$day[1], 1)
  out_suf = names(dfout)[!names(dfout) %in% c("date", "year", "month", "day", "wy", "yd", "wyd")]
  for (i in out_suf) {
    fname = paste0(file, ".", i)
    write(header, file = fname)
    data.table::fwrite(as.data.frame(dfout[,i]), file = fname, append = T)
  }

  if (is.character(clim)) {
    base_in = readLines(paste0(clim,".base"))
    base_out = gsub(clim, file, base_in)
    writeLines(base_out,con = paste0(file, ".base"))
  } else if (is.data.frame(clim)) {
    warning("Not enough info to make a basestation file, copy and update an existing one.")
  }

}

#' Plot climate data
#' 
#' @param clim_src Path to climate file
#' @param dest Path to save the plot
#' @param water_year Logical, whether to aggregate by water year
#' @export
plot_clim = function(clim_src, dest = "clim/clim_plot.png", water_year = TRUE) {
  clim = read_clim(clim_src)
  clim$date = as.POSIXct(clim$date)

  if (water_year) {
    clim_wy = aggregate(cbind(rain, tmin, tmax) ~ wy, data = clim, FUN = mean)
    clim_wy$rain = clim_wy$rain * 365.25 * 1000
    # find the scale factor to put the temperature on the same scale as precipitation
    scale = max(clim_wy$rain) / max(clim_wy$tmax)
    p = ggplot(clim_wy, aes(x = wy)) +
      geom_line(aes(y = rain, color = "Precipitation (mm)")) +
      geom_line(aes(y = tmin * scale, color = "Min Air Temperature (C)")) +
      geom_line(aes(y = tmax * scale, color = "Max Air Temperature (C)")) +
      scale_y_continuous(
        name = "Total Precipitation (mm)",
        sec.axis = sec_axis(~ ./scale, name = "Daily Air Temperature (C)")
      ) +
      labs(title = paste("Annual Climate Data from", clim_src), x = "Date") +
      theme_minimal() +
      theme(legend.position = "bottom")
    p

  } else {
    clim$rain = clim$rain * 1000
    scale = max(clim$rain) / max(clim$tmax)
    p = ggplot(clim, aes(x = date)) +
      geom_line(aes(y = tmin * scale, color = "Min Air Temperature (C)"), alpha = 0.5) +
      geom_line(aes(y = tmax * scale, color = "Max Air Temperature (C)"), alpha = 0.5) +
      geom_line(aes(y = rain, color = "Precipitation (mm)")) +
      scale_y_continuous(
        name = "Precipitation (mm)",
        sec.axis = sec_axis(~ ./scale, name = "Air Temperature (C)")
      ) +
      labs(title = paste("Daily Climate Data from", clim_src), x = "Date") +
      theme_minimal() +
      theme(legend.position = "bottom")
    p
  }
  
  ggsave(dest, plot = p, width = 10, height = 6)
}