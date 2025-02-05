#' write_basestation
#'
#' @export

write_basestation <- function(dest, year_start_index = 1900, search_dist, netcdf_var_x = "lon", netcdf_var_y = "lat",
                              precip_multiplier = 0.001, rhum_multiplier = 0.01, temp_unit = "K",
                              IDs, dem, lat, lon, x = NULL, y = NULL, lai = 3.0, screen_height = 10.0,
                              netcdf_tmax_filename, netcdf_var_tmax = "daily_maximum_temperature",
                              netcdf_tmin_filename, netcdf_var_tmin = "daily_minimum_temperature",
                              netcdf_rain_filename, netcdf_var_rain = "precipitation_amount",
                              netcdf_huss_filename = NULL, netcdf_var_huss = "daily_mean_specific_humidity",
                              netcdf_rmax_filename = NULL, netcdf_var_rmax = "daily_maximum_relative_humidity",
                              netcdf_rmin_filename = NULL, netcdf_var_rmin = "daily_minimum_relative_humidity",
                              netcdf_rsds_filename = NULL, netcdf_var_rsds = "daily_mean_shortwave_radiation_at_surface",
                              netcdf_was_filename = NULL, netcdf_var_was = "daily_mean_wind_speed") {
  # handle maps/vars that vary spatially: IDs, dem, lat, lon, x, y, lai
  any2vect <- function(X) {
    if (is.null(X)) {
      return(NULL)
    }
    if (is.vector(X)) {
      return(X)
    }
    if (class(X) == "SpatRaster") {
      return(terra::values(X))
    }
    if (is.matrix(X)) {
      return(c(t(X)))
    }
    stop("Input data type isnt raster, matrix, vector, or NULL")
  }

  # vars_exist = rep(NA, 7)
  # c(IDs, dem, lat, lon, x, y, lai)
  bs_IDs <- any2vect(IDs)
  bs_dem <- any2vect(dem)
  bs_lat <- any2vect(lat)
  bs_lon <- any2vect(lon)
  bs_x <- any2vect(x)
  bs_y <- any2vect(y)
  bs_lai <- any2vect(lai)

  # for DEM, if missing values, set as -9999? or maybe just mean of dem for now
  if (any(is.nan(bs_dem))) {
    cat("DEM has NaNs, setting to mean dem elevation.\n")
    bs_dem[is.nan(bs_dem)] <- mean(bs_dem, na.rm = T)
  }

  # for lat,lon,x,y if isnot NULL, must be same length as
  if (!is.null(bs_lat) && length(bs_lat) != length(bs_IDs)) {
    stop("Length of lats is not same as ID's")
  }
  if (!is.null(bs_lon) && length(bs_lon) != length(bs_IDs)) {
    stop("Length of lons is not same as ID's")
  }
  if (!is.null(bs_x) && length(bs_x) != length(bs_IDs)) {
    stop("Length of x is not same as ID's")
  }
  if (!is.null(bs_y) && length(bs_y) != length(bs_IDs)) {
    stop("Length of y is not same as ID's")
  }

  if (is.null(bs_lat) & is.null(bs_lon) & is.null(bs_x) & is.null(bs_y)) {
    stop("Must have either lat/lon or x/y I think")
  }

  # dem and lai, if single val, repeat for length of IDs
  if (length(bs_lai) != length(bs_IDs) && length(bs_lai) == 1) {
    bs_lai <- rep(bs_lai, length(bs_IDs))
  }

  # header info
  bshead <- data.frame(
    value = c(length(bs_IDs), search_dist, year_start_index, 0.0, 1.0, precip_multiplier, rhum_multiplier, temp_unit, netcdf_var_x, netcdf_var_y),
    var = c("grid_cells", "location_searching_distance", "year_start_index", "day_offset", "leap_year_include", "precip_multiplier", "rhum_multiplier", "temperature_unit", "netcdf_var_x", "netcdf_var_y")
  )

  # file and var name info
  f <- data.frame(
    value = c(netcdf_tmax_filename, netcdf_var_tmax, netcdf_tmin_filename, netcdf_var_tmin, netcdf_rain_filename, netcdf_var_rain),
    var = c("netcdf_tmax_filename", "netcdf_var_tmax", "netcdf_tmin_filename", "netcdf_var_tmin", "netcdf_rain_filename", "netcdf_var_rain")
  )
  if (!is.null(netcdf_huss_filename)) {
    f <- rbind(f, data.frame(
      value = c(netcdf_huss_filename, netcdf_var_huss), var = c("netcdf_huss_filename", "netcdf_var_huss")
    ))
  }
  if (!is.null(netcdf_rmax_filename)) {
    f <- rbind(f, data.frame(
      value = c(netcdf_rmax_filename, netcdf_var_rmax), var = c("netcdf_rmax_filename", "netcdf_var_rmax")
    ))
  }
  if (!is.null(netcdf_rmin_filename)) {
    f <- rbind(f, data.frame(
      value = c(netcdf_rmin_filename, netcdf_var_rmin), var = c("netcdf_rmin_filename", "netcdf_var_rmin")
    ))
  }
  if (!is.null(netcdf_rsds_filename)) {
    f <- rbind(f, data.frame(
      value = c(netcdf_rsds_filename, netcdf_var_rsds), var = c("netcdf_rsds_filename", "netcdf_var_rsds")
    ))
  }
  if (!is.null(netcdf_was_filename)) {
    f <- rbind(f, data.frame(
      value = c(netcdf_was_filename, netcdf_var_was), var = c("netcdf_was_filename", "netcdf_var_was")
    ))
  }

  bs_stations <- rbind(bshead, f)

  for (i in seq_along(bs_IDs)) {
    bs_stations[nrow(bs_stations) + 1, ] <- c(bs_IDs[i], "base_station_id")
    if (!is.null(bs_lon) && !is.null(bs_lat)) {
      bs_stations[nrow(bs_stations) + 1, ] <- c(bs_lon[i], "lon")
      bs_stations[nrow(bs_stations) + 1, ] <- c(bs_lat[i], "lat")
    }
    if (!is.null(bs_x) && !is.null(bs_y)) {
      bs_stations[nrow(bs_stations) + 1, ] <- c(bs_x[i], "x")
      bs_stations[nrow(bs_stations) + 1, ] <- c(bs_y[i], "y")
    }
    bs_stations[nrow(bs_stations) + 1, ] <- c(bs_dem[i], "z_coordinate")
    bs_stations[nrow(bs_stations) + 1, ] <- c(bs_lai[i], "effective_lai")
    bs_stations[nrow(bs_stations) + 1, ] <- c(screen_height, "screen_height")
  }

  cat("Created basestation file. Basestation header info: \n")
  print(rbind(bshead, f))

  write.table(bs_stations, dest, row.names = F, col.names = F, quote = F)
  cat("Wrote basestation file '", dest, "'", sep = "")

  return("done")
}
