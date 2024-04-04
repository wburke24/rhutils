# tec utils

# adds a tec event, using rhessys date format, reorders output
#' @export
add_tec = function(input_tec_data, date, tecname) {
  datesplit = unlist(strsplit(as.character(date), split = " "))
  input_tec_data[nrow(input_tec_data)+1,] = c(datesplit, name)
  input_tec_data = input_tec_data[order(input_tec_data$year, input_tec_data$month, input_tec_data$day, input_tec_data$hour),]
  return(input_tec_data)
}
