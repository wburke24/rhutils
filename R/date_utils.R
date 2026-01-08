# date utils

# ================================================================================
# output rhessys formatted range of dates only including complete water years
#' Water-year date range
#'
#' @param dates Vector of dates
#' @export
wy_range = function(dates) {
  datetxt = as.Date(dates)
  df <- data.frame(date = datetxt,
                   year = as.numeric(format(datetxt, format = "%Y")),
                   month = as.numeric(format(datetxt, format = "%m")),
                   day = as.numeric(format(datetxt, format = "%d")))
  st = df[min(which(df$month == 10 & df$day == 1)),]
  ed = df[max(which(df$month == 9 & df$day == 30)),]
  date_out = c(paste(paste(st[,c(2:4)],collapse = " "), "01" ),
               paste(paste(ed[,c(2:4)],collapse = " "), "24" ))
  return(date_out)
}

# ================================================================================
#' Add run and year-month indices
#'
#' @param X Data frame with year/month columns
#' @param Y Run identifier
#' @export
daily_dates = function(X, Y) {
  out = X
  out$run = Y
  out$year_month = zoo::as.yearmon(paste0(out$year,"-", out$month))
  out$ym_ind = as.numeric(out$year_month)
  return(out)
}

# ================================================================================
# date2rhdate = function(date_in) {
#   if(class(date_in) == "Date") {
#     date_out = paste0(format(date_in, "%Y %m %d"), " 01")
#     return(date_out)
#   } else if (is.chron(date_in)) {
#     date_in
#
#   } else {
#     cat("Must be Date format")
#     return(NULL)
#   }
# }

# ================================================================================
#' Add date-derived columns
#'
#' @param df Data frame with a date column
#' @export
add_dmy = function(df) {
  df$date = as.POSIXlt(df$date)
  df$year = df$date$year + 1900
  df$month = df$date$mon + 1
  df$day = df$date$mday
  df$wy = ifelse(df$month >= 10, df$year + 1, df$year)
  df$yd = lubridate::yday(df$date)
  df$wyd = RHESSysIOinR::get_waterYearDay(df$date)
  df$yearmon = zoo::as.yearmon(df$date, "%Y %m")
  return(df)
}
