# clim utils

# ================================================================================
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
