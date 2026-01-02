# tec utils

# adds a tec event, using rhessys date format, reorders output
#' @export
add_tec = function(input_tec_data, date, tecname) {
  datesplit = unlist(strsplit(as.character(date), split = " "))
  input_tec_data[nrow(input_tec_data)+1,] = c(datesplit, tecname)
  input_tec_data = tec_sort(input_tec_data)
  return(input_tec_data)
}

#' Sort a tec data frame by date and time
#'
#' Takes a tec events data frame with `year`, `month`, `day`, and `hour`
#' columns and returns it sorted in chronological order.
#'
#' @param input_tec_data A data.frame containing at least the columns
#'   `year`, `month`, `day`, and `hour` to be used for chronological sorting.
#'
#' @return A data.frame with the same rows as `input_tec_data`, ordered by
#'   `year`, `month`, `day`, and `hour`.
#'
#' @examples
#' df <- data.frame(
#'   year = c(2001, 2000),
#'   month = c(1, 12),
#'   day = c(1, 31),
#'   hour = c(0, 23),
#'   event = c("a", "b")
#' )
#' tec_sort(df)
#'
#' @export
tec_sort = function(input_tec_data) {
  # set first 4 cols as numeric
  input_tec_data$year = as.numeric(input_tec_data$year)
  input_tec_data$month = as.numeric(input_tec_data$month)
  input_tec_data$day = as.numeric(input_tec_data$day)
  input_tec_data$hour = as.numeric(input_tec_data$hour)
  # now sort correctly
  input_tec_data = input_tec_data[order(input_tec_data$year, input_tec_data$month, input_tec_data$day, input_tec_data$hour),]
  return(input_tec_data)
}