# get_stream_spatial_data
# 
# Get the streamflow, basin (possibly an encompassing basin), DEM, and soils data based on a USGS gauge ID. 
# Streamflow is from the USGS via dataRetrieval package. Basin map is from the from the USGS NHDPlus NLDI 
# using the nhdplusTools package. DEM is from the NED at ~30 meters, using the FedData Package. Soils data
# is from the POLARIS database, an optimization of SSURGO, using the XPolaris package.

# get packages
# install.packages(c("dataRetrieval", "waterData"))

library(dataRetrieval)
library(waterData)
library(terra)

# ------------------------------ EDIT HERE ------------------------------
# check online for id/dates
# 00060 	Discharge 		  00001 	Maximum
# 00065 	Gage Height 		00002 	Minimum
# 00010 	Temperature 		00003 	Mean
# 00400 	pH 	          	00008 	Median
site.ID    <- "11284400"
start.Date <- "1970-01-01" 
end.Date   <- "2022-09-30" 
data.code  <- "00060" # for streamflow
stat.code  <- "00003" # daily mean

# station.info <- readNWISsite(site.ID)

# THERE IS NO USGS GAUGE FOR THIS BASIN, instead use:
outlet_coords_proj = data.frame(x = 248939.9, y = 4413752.41)
outlet_coords_geo = c(39.8, -119.92) # this may be very wrong

basin_bounds = c(39.865731, 39.758146, -119.845207, -119.98462) # N S E W  

#utm zone
ZoneNumber = floor((outlet_coords_geo[2] + 180)/6) + 1

# produce plots?
plot = T

# ------------------------------ END EDIT ------------------------------

# guessing here but its either wgs 84 geo or utm in lat lon
# basin_ext = rast(crs = "EPSG:32611")
basin_ext = rast(crs = "EPSG:4326")
ext(basin_ext) = basin_bounds[c(4,3,2,1)]
values(basin_ext) = 1
writeRaster(x = basin_ext, filename = "preprocessing/spatial_source/basin_ext.tif",overwrite =T)

#make point data loc

basin_gauge <- st_as_sf(outlet_coords_proj, coords = c("x", "y"), crs = epsg_str)
st_write(basin_gauge, "preprocessing/spatial_source/basin_gauge.shp", delete_layer = TRUE)

# plot(basin_utm)
# plot(basin_gauge, add=T)

# buf = 0.05
# basin_buffer = basin_ext
# ext(basin_buffer) = basin_bounds[c(4,3,2,1)] + c(-buf, buf, -buf, buf)
# basin_buffer = basin_ext
# buf = 0.05
# ext(basin_buffer) = c(ext(basin_buffer)[1] - buf, ext(basin_buffer)[2] + buf, ext(basin_buffer)[3] - buf, ext(basin_buffer)[4] + buf)
# buf = 0.05
# ext(basin_buffer) = c(ext(basin_buffer)[1] - buf, ext(basin_buffer)[2] + buf, ext(basin_buffer)[3] - buf, ext(basin_buffer)[4] + buf)

# plot(basin_buffer)
# plot(basin_ext, add =T, col = "black")

hasgauge = F
if (hasgauge) {
  q_dv = readNWISdv(siteNumbers = site.ID, parameterCd = data.code)
  names(q_dv)
  q_dv = renameNWISColumns(q_dv)
  names(q_dv)

  if (plot) {
    library(ggplot2)
    varinfo = attr(q_dv, "variableInfo")
    siteinfo = attr(q_dv, "siteInfo")
    ts = ggplot(data = q_dv, aes(Date, Flow)) +
      geom_line() +
      xlab("") +
      ylab(varinfo$variableDescription) +
      ggtitle(siteinfo$station_nm)
  }

  # ------------------------------ Convert Units ------------------------------
  # Cubic feet per second (f^3/s) -> cubic meters per second (m^3/s) -> basin millimeters per day (mm/day)
  # cfs -> cms, cfs*0.028316847 = cms
  q_dv$Flow_cms = q_dv$Flow * 0.028316847
  # Get the station information - drainage area (drain_area_va) is in SQUARE MILES, sq m to sq km -> sq m * 2.58998811 = sq km
  drainge_area_km2 = station.info$drain_area_va * 2.58998811
  # m^3 per second / ( km^2 *1000 * 1000) = m per second
  q_dv$Flow_mmd = (q_dv$Flow_cms / (drainge_area_km2 * 1000000)) * (60*60*24) * 1000
  q_out = data.frame("Date" = q_dv$Date, "Flow_cfs" = q_dv$Flow, "Flow_mmd" = q_dv$Flow_mmd)

  sum(is.nan(q_out$Flow_cfs)+is.na(q_out$Flow_cfs))

  # ------------------------------ Output Table ------------------------------
  out_name = paste0("clim/", gsub(" ","_",station.info$station_nm), "_", format(min(q_out$Date),"%Y"), "_", format(max(q_out$Date),"%Y"))

  write.table(q_out, out_name ,sep="\t",col.names = T,row.names = F) ## val is cfs/s and  mm is mm/day

  # ------------------------------ END STREAMFLOW ------------------------------
}

# ------------------------------ START BASIN OUTLINE + DEM ------------------------------
library(nhdplusTools)
# https://rdrr.io/github/USGS-R/nhdplusTools/f/vignettes/plot_nhdplus.Rmd
library(sf)
library(FedData)
library(terra)
# library(rgdal)

# ------------------------------ GET MAPS ------------------------------
if (hasgauge) {
  # get_nldi_sources()
  nwissite <- list(featureSource = "nwissite", featureID = paste0("USGS-",site.ID))
  basin <- get_nldi_basin(nwissite)

  # get basin gauge
  crsepsg = st_crs(station.info$dec_coord_datum_cd)$epsg
  inputcoords =  data.frame(lon = station.info$dec_long_va, lat = station.info$dec_lat_va)
  basin_gauge <- st_as_sf(inputcoords, coords = c("lon", "lat"), crs = crsepsg)

  # reproject basin - matching gauge, should be nad83
  basin_nad83 = st_transform(basin, crs = crsepsg)
  # buffer layer to prevent edge issues with the dem, dist arc degrees or meters?
  basin_nad83_buffer = st_buffer(basin_nad83, dist = 30)
}

# grab the DEM map
NED <- get_ned(template = basin_ext, 
               label="RedRock",
               extraction.dir = paste0("preprocessing/spatial_source/RedRock/"),
               force.redo = T)

# PLOT - should cover basin
if (plot) {
  par(mfrow=c(1,1), mar=c(3,3,3,7))
  plot(NED)
  par(mfrow=c(1,1), mar=c(3,3,3,7), new=TRUE)
  plot(basin_buffer, add=T)
  par(mfrow=c(1,1), mar=c(3,3,3,7), new=TRUE)
  plot(basin_ext, add=T)
}

# ------------------------------ PROJECT MAPS - EDIT HERE ------------------------------
# projections -- wgs 84 utm 11n epsg:32611 || wgs 84 utm 10n epsg:32610
# ZoneNumber = floor((station.info$dec_long_va + 180)/6) + 1
epsg_str =  paste0("EPSG:326",ZoneNumber)
# FOR NED "+proj=longlat +datum=NAD83 +no_defs"
# ------------------------------ END EDIT ------------------------------
basin_utm = project(basin_ext, epsg_str)
NED_proj = terra::project(as(NED,"SpatRaster" ),epsg_str)
basin_gauge_proj = st_transform(basin_gauge, crs = epsg_str)
# PLOT
if (plot) {
  # jpeg("preprocessing/gauge_basin_dem.jpeg", quality = 150, width = 1000, height = 800)
  par(mfrow=c(1,1), mar=c(3,3,3,7))
  plot(NED_proj)
  par(mfrow=c(1,1), mar=c(3,3,3,7), new=TRUE)
  plot(basin_utm, add=T)
  par(mfrow=c(1,1), mar=c(3,3,3,7), new=TRUE)
  plot(basin_gauge_proj, add=T)
  # dev.off()
}

# ------------------------------ GET SOILS MAPS ------------------------------
# devtools::install_github("lhmrosso/XPolaris")
library("XPolaris")

# ID = gsub(" ","_",station.info$station_nm)
# bbox_df = as.data.frame(ID)
# bbox_df$lat = station.info$dec_lat_va
# bbox_df$long = station.info$dec_long_va

# bbox_df = data.frame(ID = c("a","b","c","d"),
#                      lat = st_bbox(basin_nad83)[c(2,2,4,4)],
#                      long = st_bbox(basin_nad83)[c(1,3,1,3)])

bbox_df = data.frame(ID = c("a","b","c","d"),
                     lat = ext(basin_ext)[c(2,2,4,4)],
                     long = ext(basin_ext)[c(1,3,1,3)])

xplot(locations = bbox_df)

df_ximages <- ximages(locations = bbox_df,
                      statistics = c('mean'),
                      variables = c('clay','sand','ksat'),
                      layersdepths = c('0_5'),
                      localPath = "preprocessing/spatial_source/")

# xsoil(ximages_output = df_ximages, localPath = "preprocessing/spatial_source/")
# clay = rast('./download/POLARISOut/mean/clay/0_5/lat3839_lon-121-120.tif')
# sand = rast('./download/POLARISOut/mean/sand/0_5/lat3839_lon-121-120.tif')
# ksat =  rast('./download/POLARISOut/mean/ksat/0_5/lat3839_lon-121-120.tif')
# clay.crop = crop(clay, dem)
# sand.crop = crop(sand, dem)

# ------------------------------ OUTPUT MAPS ------------------------------
writeRaster(NED_proj, "preprocessing/spatial_source/ned_dem_basinclip.tif")
st_write(basin_utm, dsn = "preprocessing/spatial_source/nldi_basin.shp")
st_write(basin_gauge_proj, dsn = "preprocessing/spatial_source/gauge_loc.shp")
# writeOGR(basin_utm, dsn = "preprocessing/spatial_source/",layer = "nldi_basin", driver ="ESRI Shapefile")
# writeOGR(basin_gauge_proj, dsn = "preprocessing/spatial_source/", layer = "gauge_loc", driver="ESRI Shapefile")


# ------------------------------ END ------------------------------
