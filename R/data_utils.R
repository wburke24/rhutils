# data reading and aggregation utils

#' @export
get_basin_daily = function(out_dir) {
  basin_files_in = list.files(path = out_dir, pattern = ".*_basin.*\\.csv$", full.names = T)
  if (length(basin_files_in) == 0) {
    basin_files_in = list.files(path = out_dir, pattern = ".*\\.csv$", full.names = T)
  }
  run_names = gsub("_basin", "", gsub(".csv", "", basename(basin_files_in)))
  basin_daily_list = lapply(basin_files_in, fread)
  basin_daily_list = mapply(function(X,Y) {X$run = Y;return(X)} , basin_daily_list, run_names, SIMPLIFY = F)
  basin_daily_dt = rbindlist(basin_daily_list)
  basin_daily_dt = RHESSysIOinR::add_dates(basin_daily_dt)
  return(basin_daily_dt)
}

#' @export
basin_daily2mn = function(basin_daily) {
  vars_in_basin = names(basin_daily)[!names(basin_daily) %in% c("day", "month", "year", "basinID", "date", "wy", "yd", "run")]
  basin_mn = basin_daily[, lapply(.SD, mean), by=c("run", "year", "month"), .SDcols = vars_in_basin]
  basin_mn$year_month = zoo::as.yearmon(paste0(basin_mn$year,"-", basin_mn$month))
  basin_mn$ym_ind = as.numeric(basin_mn$year_month)
  return(basin_mn)
}

#' @export
basin_output_fixes = function(DT) {
  DT$date = lubridate::ymd(paste(DT$year, DT$month, DT$day, sep = "-"))
  DT[, evaporation_total:=evaporation+evaporation_surf+exfiltration_sat_zone+exfiltration_unsat_zone]
  DT[, transpiration_total:=transpiration_sat_zone+transpiration_unsat_zone]
  DT[,c("evaporation","evaporation_surf", "exfiltration_sat_zone", "exfiltration_unsat_zone", "transpiration_sat_zone", "transpiration_unsat_zone"):=NULL]
  return(DT)
}

# AGGREGATION TO BE LAPPLY'D ACROSS OUTPUTS
#' @export
agg_dyn = function(DT, name, aggvars, vars) {
  out = DT[, lapply(.SD, mean), by=aggvars, .SDcols = vars]
  out$run = name
  out$year_month = zoo::as.yearmon(paste0(out$year,"-", out$month))
  out$ym_ind = as.numeric(out$year_month)
  return(out)
}

#' @export
agg_YM = function(X, Y) {
  out = X[, lapply(.SD, mean), by=c("year", "month"), .SDcols = vars]
  out$run = Y
  out$year_month = zoo::as.yearmon(paste0(out$year,"-", out$month))
  out$ym_ind = as.numeric(out$year_month)
  return(out)
}

#' @export
agg_YM_strata = function(X, Y) {
  X[, sID := stratumID %% 10]
  out = X[, lapply(.SD, mean), by=c("year", "month", "sID"), .SDcols = vars]
  out$run = Y
  out$year_month = zoo::as.yearmon(paste0(out$year,"-", out$month))
  out$ym_ind = as.numeric(out$year_month)
  return(out)
}


# # group things
# mean2sim = function(DT, scenarios) {
#   DT_mn = DT[, .(mean(value)), by = c("run")]
#   DT_mn = merge.data.table(DT_mn, scenarios, by = "run", all = FALSE)
#   return(DT_mn)
# }
# mean2simCS = function(DT, scenarios) {
#   DT_mn = DT[, .(mean(value)), by = c("run", "stratumID")]
#   DT_mn = merge.data.table(DT_mn, scenarios, by = "run", all = FALSE)
#   return(DT_mn)
# }
# # MONTHLY
# mean2mn = function(DT, scenarios) {
#   DT_mn = DT[, .(mean(value)), by = c("run", "sim_yr", "wym")]
#   DT_mn = merge.data.table(DT_mn, scenarios, by = "run")
#   DT_mn$yr_mn = (DT_mn$wym + (DT_mn$sim_yr - min(DT_mn$sim_yr)) * 12)
#   return(DT_mn)
# }
# # ANNUAL
# mean2yr = function(DT, scenarios) {
#   DT_yr = DT[, .(mean(value)), by = c("run", "sim_yr")]
#   DT_yr = merge.data.table(DT_yr, scenarios, by = "run")
#   return(DT_yr)
# }
# max2yr = function(DT, scenarios) {
#   DT_yr = DT[, .(max(value)), by = c("run", "sim_yr")]
#   DT_yr = merge.data.table(DT_yr, scenarios, by = "run", all = FALSE)
#   return(DT_yr)
# }
# # CENTER OF MASS
# CoM = function(in_data) {
#   CoM =  match( TRUE, cumsum(in_data/sum(in_data)) > .5 ) - 1
#   return(CoM)
# }
# CoM2yr = function(DT, scenarios) {
#   DT_yr = DT[, .(CoM(value)), by = c("run", "sim_yr")]
#   DT_yr = merge.data.table(DT_yr, scenarios, by = "run")
#   return(DT_yr)
# }
