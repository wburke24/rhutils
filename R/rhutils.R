#' rhutils
#' 
#' Utility functions and workflows for RHESSys.
#' @author William Burke
#' @import ggplot2
#' @import gridExtra
#' @import leaflet
#' @import RHESSysIOinR
#' @import RHESSysPreprocessing
#' @import stringr
#' @importFrom data.table data.table as.data.table rbindlist shift setDT setnames fread fwrite merge.data.table
#' @importFrom dplyr recode
#' @importFrom grDevices colorRampPalette dev.copy2pdf dev.off pdf rainbow terrain.colors
#' @importFrom graphics hist legend par text
#' @importFrom hydroGOF NSE pbias rmse
#' @importFrom lubridate time_length yday ymd
#' @importFrom sensitivity src
#' @importFrom terra rast vect trim resample writeRaster mask crop project ext values focal cellSize merge sprc crs geom is.lonlat
#' @importFrom sf st_area st_as_sf st_geometry
#' @importFrom soiltexture TT.points.in.classes
#' @importFrom stats aggregate coefficients formula lm median quantile runif sd
#' @importFrom utils browseURL capture.output read.csv read.table str tail write.csv write.table
#' @importFrom whitebox wbt_aspect wbt_breach_depressions wbt_d8_flow_accumulation wbt_d8_pointer wbt_extract_streams wbt_horizon_angle wbt_jenson_snap_pour_points wbt_slope wbt_subbasins wbt_watershed
"_PACKAGE"