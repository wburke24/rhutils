# clim_ncdf_makebasestation
# 
# Create a RHESSys basestation file based on Gridmet netcdf inputs
# 
# Requires
#   - createbaseinfo_netcdf.c compiled binary
# 
# Inputs:
#   - Processed/edited/subset aggregated (through time), netcdf climate data, via: #http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_MET_aggregated_catalog.html
#     (easies to use/download spatial subset to begin with, via the NetcdfSubset download option)
#     Has already been processed and modified via clim_ncdf_processgridmet
#   - gridment elevation metadata netcdf (from same source)
#   - Basin and DEM raster maps

library(ncdf4)
library(terra)

plots = T

# ------------------------------ INPUTS ------------------------------
# uncompiled source of the createbaseinfo_netcdf.c code
# ncbase_src = "../../Util/createbaseinfo_netcdf.c"

# USE SAME BASIN RASTER AS PREVIOUS SCRIPT (clim_1_ncdf_processgridmet.R)
basin = rast("preprocessing/whitebox/basin.tif")
DEM = rast("preprocessing/whitebox/dem.tif")

# location for maps to be used with base station creation
# map_dest = "clim/netcdfmaps"

# where to output a new zone map based on Netcdf grid
zone_dest = "preprocessing/whitebox/"

# BASESTATION DEST
dest = file.path("clim/netcdf.base")

# specify climate files individualy or via pattern, USE PROCESSED CLIMATE INPUTS FROM PREVIOUS SCRIPT (clim_1_ncdf_processgridmet.R)
# clim_files = list.files(path = "clim",pattern = "crop_agg_met_", full.names = T)
clim_files = list.files(path = "clim",pattern = "agg_met_daymet", full.names = T)
# clim_files = list.files(path = "clim",pattern = "agg_met_daymet.*test", full.names = T)
# backup since sometimes the clim netcdf can not work to create the grid
# gridmet_metadata = "../data/gridmet/metdata_elevationdata.nc"
# CHECK THE EDITS TO THE BASE STATION FILE AT THE END, FILE NAMES MAY NEED TO BE CORRECTED

# ------------------------------ END INPUTS ------------------------------
# for later use
basin_vect = as.polygons(basin)
basin_vect_latlon = project(basin_vect, "EPSG:4326")
# ------------------------------ GET GRID MAP FROM NETCDF ------------------------------
# for comparison, exploring original data
# tmp = nc_open(clim_files[1])
# latitude <- ncvar_get(tmp, "lat")
# longitude <- ncvar_get(tmp, "lon")

# there are slight differences between getting the grid from the metadata elevation map and the 
# netcdf already clipped using NCO. Using the clipped data from NCO as a default for now, but 
# could easily be better the other way, code should be easy to toggle

# Get CRS from netcdf
nctmp = nc_open(clim_files[1])
globalatts = ncatt_get(nctmp, varid = 0)
lcc_atts = ncatt_get(nctmp, varid = "lambert_conformal_conic")
nc_close(nctmp)

# ---------- Grid from clipped climate netcdf files ----------
grid_from_clim = T
if (grid_from_clim) {
  # read in netcdf as raster, get only 1 day of precip
  nc_grid_lonlat = rast(clim_files[1])[[1]]
  # set values to sequential, chnage name
  values(nc_grid_lonlat) = seq_along(values(nc_grid_lonlat))
  names(nc_grid_lonlat) = "ID"
  # project to utm, mask to get zone/basestation map
  nc_grid = project(nc_grid_lonlat, basin, method="near")
  id_grid = mask(nc_grid, basin)
}

grid_from_elevmeta = F
if (grid_from_elevmeta) {
  gridmeta_proj = project(rast(gridmet_metadata), crs(basin))
  # ----- method using gridmet elevation metadata -----
  # clip/crop the grid
  nc_grid  = crop(gridmeta_proj, basin, extend = T, touches = T, snap = "out")
  # fix name, set values to sequential
  names(nc_grid) = "ID"
  values(nc_grid) = seq_along(values(nc_grid))
  nc_grid_lonlat = project(nc_grid, "EPSG:4326")
  # resample the grid to basin res (from 4km to 90m or whatever)
  basin_extended = extend(basin, nc_grid)
  nc_grid_resample = resample(nc_grid, basin_extended, method = "near")
  # mask then crop to original basin extent
  nc_grid_mask = mask(nc_grid_resample, basin_extended)
  id_grid = crop(nc_grid_mask, basin)
}

if (plots) {
  par(mfrow = c(2, 1))
  # layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE))
  # projected grid
  plot(nc_grid_lonlat)
  plot(basin_vect_latlon, add=T)
  # masked map (zone basestation map)
  plot(id_grid)
  plot(basin_vect, add=T)
}

# --------------- output the zone grid for use in preprocessing ---------------
# zone = mask(x = id_grid_proj, mask = basin)
writeRaster(id_grid, file.path(zone_dest, "zoneDAYMET.tif"), overwrite=T)

# ------------------------------ GENERATE INPUTS FOR NETCDF BASE STATION CREATION  ------------------------------
# Maps need to be based on the netcdf latlong grid resolution + extent

# demproj = project(DEM, "EPSG:4326")
demproj = project(DEM, crs(nc_grid_lonlat)) # can just project based on read in data
dem_nc = resample(demproj, nc_grid_lonlat,method="average")

# ------------------------------ GET VAR NAMES  ------------------------------
varnames = rep(NA, length(clim_files))
for (i in seq_along(clim_files)) {
  tmp = nc_open(clim_files[i])
  vartmp = names(tmp$var)
  if (length(vartmp) > 1) {
    vartmp <- vartmp[!vartmp %in% c("lat", "lon", "lambert_conformal_conic")]
  }
  varnames[i] = vartmp
  nc_close(tmp)
}

# =========================== WRITE BASE STATION ==========================
# ID,lat,lon etc inputs are maps, matrix, single value, or values - where length(values) == length(values(map)) == length(c(t(matrix)))

write_basestation(
  dest = dest, year_start_index = 1900, search_dist = 0.04, 
  netcdf_var_x = "lon", netcdf_var_y = "lat", 
  precip_multiplier = 0.001, rhum_multiplier = 0.01, temp_unit = "K", 
  IDs = nc_grid_lonlat, dem = dem_nc, lat = crds(nc_grid_lonlat, df=T)$y, lon = crds(nc_grid_lonlat, df=T)$x, 
  x = NULL, y = NULL, lai = 3.0, screen_height = 10.0,
  netcdf_tmax_filename = clim_files[grepl("tmmx|tmax", clim_files)], 
  netcdf_var_tmax = varnames[grepl("tmmx|tmax", clim_files)], 
  netcdf_tmin_filename = clim_files[grepl("tmmn|tmin", clim_files)], 
  netcdf_var_tmin = varnames[grepl("tmmn|tmin", clim_files)], 
  netcdf_rain_filename = clim_files[grepl("pr|prcp", clim_files)],
  netcdf_var_rain = varnames[grepl("pr|prcp", clim_files)]
)

write_basestation = function(dest, year_start_index = 1900, search_dist, netcdf_var_x = "lon", netcdf_var_y = "lat", 
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
  any2vect = function(X) {
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
  bs_IDs = any2vect(IDs)
  bs_dem = any2vect(dem)
  bs_lat = any2vect(lat)
  bs_lon = any2vect(lon)
  bs_x = any2vect(x)
  bs_y = any2vect(y)
  bs_lai = any2vect(lai)

  # for DEM, if missing values, set as -9999? or maybe just mean of dem for now
  if (any(is.nan(bs_dem))) {
    cat("DEM has NaNs, setting to mean dem elevation value")
    bs_dem[is.nan(bs_dem)] = mean(bs_dem, na.rm=T)
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
    bs_lai = rep(bs_lai, length(bs_IDs))
  }

  # header info
  bshead = data.frame(
    value = c(length(bs_IDs), search_dist, year_start_index, 0.0, 1.0, precip_multiplier, rhum_multiplier, temp_unit, netcdf_var_x, netcdf_var_y),
    var = c("grid_cells", "location_searching_distance", "year_start_index", "day_offset", "leap_year_include", "precip_multiplier", "rhum_multiplier", "temperature_unit", "netcdf_var_x", "netcdf_var_y")
  )

  # file and var name info
  f = data.frame(
    value = c(netcdf_tmax_filename, netcdf_var_tmax, netcdf_tmin_filename, netcdf_var_tmin, netcdf_rain_filename, netcdf_var_rain),
    var = c("netcdf_tmax_filename", "netcdf_var_tmax", "netcdf_tmin_filename", "netcdf_var_tmin", "netcdf_rain_filename", "netcdf_var_rain")
  )
  if (!is.null(netcdf_huss_filename)) {
    f = rbind(f, data.frame(
      value = c(netcdf_huss_filename, netcdf_var_huss), var = c("netcdf_huss_filename", "netcdf_var_huss"))
    )
  }
  if (!is.null(netcdf_rmax_filename)) {
    f = rbind(f, data.frame(
      value = c(netcdf_rmax_filename, netcdf_var_rmax), var = c("netcdf_rmax_filename", "netcdf_var_rmax"))
    )
  }
  if (!is.null(netcdf_rmin_filename)) {
    f = rbind(f, data.frame(
      value = c(netcdf_rmin_filename, netcdf_var_rmin), var = c("netcdf_rmin_filename", "netcdf_var_rmin"))
    )
  }
  if (!is.null(netcdf_rsds_filename)) {
    f = rbind(f, data.frame(
      value = c(netcdf_rsds_filename, netcdf_var_rsds), var = c("netcdf_rsds_filename", "netcdf_var_rsds"))
    )
  }
  if (!is.null(netcdf_was_filename)) {
    f = rbind(f, data.frame(
      value = c(netcdf_was_filename, netcdf_var_was), var = c("netcdf_was_filename", "netcdf_var_was"))
    )
  }

  bs_stations = rbind(bshead, f)

  for (i in seq_along(IDs)) {
    bs_stations[nrow(bs_stations) + 1, ] = c(bs_IDs[i], "base_station_id")
    if (!is.null(bs_lon) && !is.null(bs_lat)) {
      bs_stations[nrow(bs_stations) + 1, ] = c(bs_lon[i], "lon")
      bs_stations[nrow(bs_stations) + 1, ] = c(bs_lat[i], "lat")
    }
    if (!is.null(bs_x) && !is.null(bs_y)) {
      bs_stations[nrow(bs_stations) + 1, ] = c(bs_x[i], "x")
      bs_stations[nrow(bs_stations) + 1, ] = c(bs_y[i], "y")
    }
    bs_stations[nrow(bs_stations) + 1, ] = c(bs_dem[i], "z_coordinate")
    bs_stations[nrow(bs_stations) + 1, ] = c(bs_lai[i], "effective_lai")
    bs_stations[nrow(bs_stations) + 1, ] = c(screen_height, "screen_height")
  }

  cat("Created basestation file. Basestation header info: \n")
  print(rbind(bshead, f))
  
  write.table(bs, dest, row.names = F, col.names = F, quote = F)
  cat("Wrote basestation file '",dest,"'", sep="")

  return("done")
}


# ------------------------------ OLD BASESTATION VERSION ------------------------------
oldbase = F
if (oldbase) {

  if (!file.exists(map_dest)) {
    dir.create(map_dest)
  }

  writeRaster(dem_nc, filename = file.path(map_dest,"dem.asc"), filetype="AAIGrid", gdal = c("FORCE_CELLSIZE=TRUE"), NAflag=-9999, overwrite=T)
  writeRaster(lai_nc, filename = file.path(map_dest,"lai.asc"), filetype="AAIGrid", gdal = c("FORCE_CELLSIZE=TRUE"), NAflag=-9999, overwrite=T)
  writeRaster(nc_grid_lonlat, filename = file.path(map_dest,"cellid.asc"), filetype="AAIGrid", datatype =  "INT4S", gdal = c("FORCE_CELLSIZE=TRUE"), NAflag=-9999, overwrite=T)
  file.remove(list.files(path = map_dest, pattern = ".prj|.aux.xml", full.names = T))
  
  
  # Get grid info - xy and latlon
  nctmp = nc_open(clim_files[1])
  ync = ncvar_get(nctmp,varid = "y")
  xnc = ncvar_get(nctmp,varid = "x")
  lonnc = ncvar_get(nctmp,varid = "lon")
  latnc = ncvar_get(nctmp,varid = "lat")
  nc_close(nctmp)
  
  # format: ID ID Y X X Y
  xyid_loc = cbind(1:nrow(crds(nc_grid_lonlat)), 1:nrow(crds(nc_grid_lonlat)), crds(nc_grid_lonlat)[,c("y","x")], crds(nc_grid_lonlat))
  daymet = F
  if (daymet) {
    # xyid_loc = cbind(seq_along(c(t(latgrid))), seq_along(c(t(latgrid))), c(t(latgrid)), c(t(longrid)), c(t(longrid)),c(t(latgrid)))
    xyid_loc = cbind(1:nrow(crds(nc_grid_lonlat)), 1:nrow(crds(nc_grid_lonlat)), crds(nc_grid_lonlat)[,c("y","x")]/1000, crds(nc_grid_lonlat)/1000)
    xyid_loc = cbind(1:nrow(crds(nc_grid_lonlat)), 1:nrow(crds(nc_grid_lonlat)), crds(nc_grid_lonlat)[,c("y","x")], crds(nc_grid_lonlat))
  }
  
  write.table(xyid_loc, file.path(map_dest,"xyid_loc.txt"), row.names = F, col.names = F)
  
  # ------------------------------ RUN C BIN TO GET BASE STATION  ------------------------------
  # COMPILE -- IF ERROR TRY RELATIVE PATHS
  system(noquote(paste("bash -c \"", paste0("gcc ", ncbase_src, " -o ", file.path(map_dest,"create_netcdfbase")), "\"", sep = "")))
  
  # RUN -- YOU MAY NEED TO EDIT THIS, command should look like: ./create_netcdfbase cellid.asc lai.asc dem.asc xyid_loc.txt clim/ netcdf.base
  cmd = paste0(file.path(map_dest,"create_netcdfbase"), " ", file.path(map_dest,"cellid.asc"), " ", file.path(map_dest,"lai.asc"), " ",
               file.path(map_dest,"dem.asc"), " ", file.path(map_dest,"xyid_loc.txt"), " ", dirname(clim_files[1]), "/ ", file.path(map_dest,"netcdf.base"))
  system(noquote(paste("bash -c \"",cmd ,"\"", sep = "")))

  # WORKING HERE
  ncbase = read.table(file.path(map_dest,"netcdf.base"))

  ncbase[ncbase[,2]=="location_searching_distance", 1] = 0.5
  ncbase[ncbase[,2]=="location_searching_distance", 1] = 500

  # ncbase[ncbase[,2]=="netcdf_var_x", 1] = "x"
  # ncbase[ncbase[,2]=="netcdf_var_y", 1] = "y"
  # ncbase[ncbase[,2]=="lat", 2] = "y"
  # ncbase[ncbase[,2]=="lon", 2] = "x"
  ncbase[ncbase[,2]=="netcdf_var_x", 1] = "lon"
  ncbase[ncbase[,2]=="netcdf_var_y", 1] = "lat"
  ncbase[ncbase[,2]=="lat", 2] = "lat"
  ncbase[ncbase[,2]=="lon", 2] = "lon"
  # ncbase[ncbase[,2]=="year_start_index", 1] = "1900"
  ncbase[ncbase[,2]=="year_start_index", 1] = "1950"
  ncbase[ncbase[,2]=="netcdf_tmax_filename", 1] = clim_files[grepl("tmmx|tmax", clim_files)]
  ncbase[ncbase[,2]=="netcdf_var_tmax", 1] = varnames[grepl("tmmx|tmax", clim_files)]
  ncbase[ncbase[,2]=="netcdf_tmin_filename", 1] = clim_files[grepl("tmmn|tmin", clim_files)]
  ncbase[ncbase[,2]=="netcdf_var_tmin", 1] = varnames[grepl("tmmn|tmin", clim_files)]
  ncbase[ncbase[,2]=="netcdf_rain_filename", 1] = clim_files[grepl("pr|prcp", clim_files)]
  ncbase[ncbase[,2]=="netcdf_var_rain", 1] = varnames[grepl("pr|prcp", clim_files)]

  # ncbase[ncbase[,2]=="netcdf_huss_filename", 1] = clim_files[grepl("daily_mean_specific_humidity", varnames)]
  # ncbase[ncbase[,2]=="netcdf_var_huss", 1] = varnames[grepl("daily_mean_specific_humidity", varnames)]
  # ncbase[ncbase[,2]=="netcdf_rmax_filename", 1] = clim_files[grepl("rmax", clim_files)]
  # ncbase[ncbase[,2]=="netcdf_var_rmax", 1] = varnames[grepl("rmax", clim_files)]
  # ncbase[ncbase[,2]=="netcdf_rmin_filename", 1] = clim_files[grepl("rmin", clim_files)]
  # ncbase[ncbase[,2]=="netcdf_var_rmin", 1] = varnames[grepl("rmin", clim_files)]
  # ncbase[ncbase[,2]=="netcdf_rsds_filename", 1] = clim_files[grepl("daily_mean_shortwave_radiation_at_surface", varnames)]
  # ncbase[ncbase[,2]=="netcdf_var_rsds", 1] = varnames[grepl("daily_mean_shortwave_radiation_at_surface", varnames)]
  # ncbase[ncbase[,2]=="netcdf_was_filename", 1] = clim_files[grepl("daily_mean_wind_speed", varnames)]
  # ncbase[ncbase[,2]=="netcdf_var_was", 1] = varnames[grepl("daily_mean_wind_speed", varnames)]

  write.table(ncbase, file.path(map_dest,"netcdf.base"), row.names = F, col.names = F, quote = F)

  file.copy(file.path(map_dest,"netcdf.base"), to = "clim/netcdf_daymet.base", overwrite = T)
}
# ------------------------------ OLD GRASS VERSION ------------------------------
grass = F
if (grass) {
  # GRASS COMMANDS
  # 
  # Create location with tif version of cropped netcdf, with extent fixed by vector basin file
  # r.proj location=Wardproj mapset=PERMANENT input=basin
  # g.region n=39.162039 s=39.111069 w=-120.247294 e=-120.156854 rows=1 cols=2 -p
  # g.region res=00:00:02.7 -a -p
  # r.proj --overwrite --verbose location=Wardproj mapset=PERMANENT input=basin
  # g.region n=39.162750 s=39.111000 w=-120.247500 e=-120.156750 rows=1 cols=2 -p
  
  # r.mapcalc expression=xloc=x()                                                   
  # r.mapcalc expression=yloc=y()                                                   
  # r.mapcalc expression=xmap=col()                                                 
  # r.mapcalc expression=ymap=row() 
  
  # r.mapcalc expression=xyid=(ymap-1)*12+xmap
  # r.mapcalc expression=screen_height=2.0
  # r.proj -g location=Wardproj mapset=PERMANENT input=dem30f
  # g.region n=39:10:13.361221N s=39:03:07.027948N w=120:15:27.127436W e=120:08:08.47742W rows=427 cols=337
  # g.region res=00:00:02.7 -a -p
  # r.proj --overwrite --verbose location=Wardproj mapset=PERMANENT input=dem30f
  # g.region rast=xyid res=00:00:02.7
  # r.mapcalc expression=dem2_7sec=dem30f
  # r.mapcalc expression=lai2_7sec=3.0
  # r.mapcalc expression=basin2_7sec=basin
  # g.region rast=xyid res=00:02:30
  # r.resamp.stats input=basin output=test method=sum --overwrite
  # r.mapcalc expression=basinmask=if(test >= 1, 1)
  # g.remove type=raster name=test@PERMANENT -f
  # r.mask raster=basinmask maskcats=1
  # r.mapcalc expression=xyid_msk = xyid
  # r.resamp.stats input=dem2_7sec output=dem2m30s method=average --overwrite
  # r.resamp.stats input=lai2_7sec output=lai2m30s method=average --overwrite
  # r.resamp.stats input=screen_height output=screenheight2m30s method=average --overwrite
  
  # set base="C:/Users/burke/Documents/Carb/Ward/clim/"
  # 
  # r.out.ascii input=xyid_msk output="%base%/cellid_msk.asc"
  # r.out.ascii input=dem2m30s output="%base%/dem_msk.asc"
  # r.out.ascii input=lai2m30s output="%base%/lai_msk.asc"
  # r.out.ascii input=screenheight2m30s output="%base%/screen_msk.asc"
  # r.out.ascii input=xloc output="%base%/xloc.asc"
  # r.out.ascii input=yloc output="%base%/yloc.asc"
  
  # FIX HEADERS
  # r.out.gdal input=xyid_msk output="%base%/cellid.asc" format=AAIGrid type=Int16 createopt=FORCE_CELLSIZE=TRUE nodata=-9999 --overwrite
  # r.out.gdal input=dem2m30s output="%base%/dem.asc" format=AAIGrid type=Float64 createopt=FORCE_CELLSIZE=TRUE nodata=-9999 --overwrite
  # r.out.gdal input=lai2m30s output="%base%/lai.asc" format=AAIGrid type=Float64 createopt=FORCE_CELLSIZE=TRUE nodata=-9999 --overwrite
  # r.out.gdal input=screenheight2m30s output="%base%/screen.asc" format=AAIGrid type=Float64 createopt=FORCE_CELLSIZE=TRUE nodata=-9999 --overwrite
  # r.out.gdal input=xloc output="%base%/xloc.asc" format=AAIGrid type=Float64 createopt=FORCE_CELLSIZE=TRUE nodata=-9999 --overwrite
  # r.out.gdal input=yloc output="%base%/yloc.asc" format=AAIGrid type=Float64 createopt=FORCE_CELLSIZE=TRUE nodata=-9999 --overwrite
  
  # file.remove(list.files(path = "clim/", pattern = ".prj|.aux.xml", full.names = T))
  # 
  # xloc = read.table("clim/xloc.asc", skip = 6)
  # yloc = read.table("clim/yloc.asc", skip = 6)
  # # COMBINE - by rows, eg ids are seq along each row
  # # format: ID ID Y X X Y
  # xyid_loc = data.frame(seq_along(as.vector(unlist(xloc))), seq_along(as.vector(unlist(xloc))),as.vector(unlist(yloc)), 
  #                       as.vector(unlist(xloc)), as.vector(unlist(xloc)), as.vector(unlist(yloc)), fix.empty.names = F)
  # write.table(xyid_loc, "clim/xyid_loc.txt", row.names = F, col.names = F)
  # 
  # # ./create_netcdfbase cellid.asc lai.asc dem.asc xyid_loc.txt clim/ netcdf.base
  # 
  # # CHECK VARS
  # pr_nc = nc_open("clim/crop_agg_met_pr_1979_CurrentYear_CONUS.nc")
  # tmin_nc = nc_open("clim/crop_agg_met_tmmn_1979_CurrentYear_CONUS.nc")
  # tmax_nc = nc_open("clim/crop_agg_met_tmmx_1979_CurrentYear_CONUS.nc")
}
