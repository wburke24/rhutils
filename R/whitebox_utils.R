# Set of functions to semi automate whitebox tools

# ================================================================================
# resample and trim dem, make breach, d8 pointer, d8 accum, gauge snap, basin extent
#' @export
wbox_dem2streams_gauge_basin = function(source_dem, source_gauge, res, stream_threshold = 100, gauge_snap_dist = 90, output_dir, tmp_dir = file.path(output_dir, "wb_tmp"), plots = T, writeplots = T, overwrite = T) {

  # ==================== copy and trim source DEM ====================
  dem_source = trim(rast(source_dem))
  dem_resample = resample(dem_source, rast(ext(dem_source),resolution = res))
  writeRaster(dem_resample, file.path(tmp_dir,"dem_trim.tif"), overwrite=overwrite)

  if (plots) {
    terra::plot(rast(file.path(tmp_dir,"dem_trim.tif")), main = paste0("DEM Resampled to ",res,"m"))
  } 

  # ==================== Fill, d8 direction, accumulation ====================
  ## Breach depressions to ensure continuous flow
  wbt_breach_depressions(dem = file.path(tmp_dir,"dem_trim.tif"), output = file.path(tmp_dir, "dem_brch.tif"))
  # wbt_breach_depressions_least_cost(dem = file.path(tmp_dir,"dem_trim.tif"), output = file.path(tmp_dir, "dem_brch.tif"), dist = 50)
  # wbt_fill_depressions(dem = file.path(tmp_dir,"dem_trim.tif"), output = file.path(tmp_dir, "dem_brch.tif"))
  ## Generate d8 flow pointer (note: other flow directions are available)
  wbt_d8_pointer(dem = file.path(tmp_dir, "dem_brch.tif"), output = file.path(tmp_dir, "dem_brch_ptr_d8.tif"))
  # this is fractional? d8, more equivilent to previous grass method
  # wbt_fd8_pointer(dem = file.path(tmp_dir, "dem_brch.tif"), output = file.path(tmp_dir, "dem_brch_ptr_fd8.tif"))
  ## Generate d8 flow accumulation in units of cells (note: other flow directions are available)
  wbt_d8_flow_accumulation(input = file.path(tmp_dir, "dem_brch.tif"), output = file.path(tmp_dir, "dem_brch_accum_d8.tif"), out_type = "cells")
  # wbt_fd8_flow_accumulation(dem = file.path(tmp_dir, "dem_brch.tif"), output = file.path(tmp_dir, "dem_brch_accum_fd8.tif"), out_type = "cells")

  wbt_extract_streams(flow_accum = file.path(tmp_dir, "dem_brch_accum_d8.tif"), output = file.path(tmp_dir, "streams.tif"), threshold = stream_threshold)
  
  # POUR POINT CANNOT HAVE ATTRIBUTE DATA FOR SOME STUPID REASON
  if(length(names(vect(source_gauge))) > 0) {
    # remove attributes and write a temp file
    cat("Pour point cannot have attributes - writing temporary version for use with snap pour points.\n")
    v = vect(source_gauge)    
    v_geom <- vect(geom(v), crs = crs(v))  # keep CRS explicitly
    writeVector(v_geom, file.path(tmp_dir, "pour_point_temp.shp"), overwrite = TRUE)
    source_gauge = file.path(tmp_dir, "pour_point_temp.shp")
  }
  
  #snapped gauge is first final output
  wbt_jenson_snap_pour_points(pour_pts = source_gauge, 
                              streams = file.path(tmp_dir, "streams.tif"), 
                              output = file.path(output_dir, "gauge_loc_snap.shp"), 
                              snap_dist = gauge_snap_dist)  
    
  tmp = vect(file.path(output_dir, "gauge_loc_snap.shp"))
  if (length(tmp) == 0) {
    stop("Gauge snapping failed, try different distance.")
  }

  if (plots) {
    terra::plot(rast(file.path(tmp_dir, "streams.tif")), main = paste0("Streams & pour point"),col="black")
    terra::plot(vect(source_gauge),add=T ,col="red", pch = 19, cex = 1.5)
    terra::plot(vect(file.path(output_dir, "gauge_loc_snap.shp")),add=T ,col="blue", pch = 17, cex = 1.5)
    legend("topright", legend = c("Source gauge", "Snapped gauge"),col = c("red","blue"), pch = c(19, 17), pt.cex = 1.5, bty = "n")
  }

  wbt_watershed(d8_pntr = file.path(tmp_dir, "dem_brch_ptr_d8.tif"), 
                pour_pts = file.path(output_dir, "gauge_loc_snap.shp"), 
                output = file.path(output_dir, "basin.tif"))
  
  # ==================== trim the breach DEM and streams to the basin for later use ====================
  cat("Masking dem_brch.tif to basin extent.\n")
  dem_brch_mask = mask(rast(file.path(tmp_dir, "dem_brch.tif")), rast(file.path(output_dir, "basin.tif")))
  writeRaster(dem_brch_mask, filename = file.path(output_dir, "dem.tif"), overwrite = T)
  
  cat("Masking streams.tif to basin extent.\n")
  streams_mask = mask(rast(file.path(tmp_dir, "streams.tif")),rast(file.path(output_dir, "basin.tif")))
  writeRaster(streams_mask, filename = file.path(output_dir, "streams.tif"), overwrite = T)
  
  # ==================== Report maps created ====================
  cat("\n========================================\n")
  cat("Maps written to temporary directory ('",tmp_dir,"'):\n\tdem_brch.tif (depression breached DEM, trimmed by basin)\n\tdem_brch_ptr_d8.tif (D8 flow direction)\n\tdem_brch_accum_d8.tif (D8 flow accumulation)\n\tstreams.tif.\n", sep="")
  cat("Maps written to output directory ('",output_dir,"'):\n\tbasin.tif\n\tgauge_loc_snap.shp (gauge location, snapped to stream)\n\tdem.tif (masked to basin extent) \n\tstreams.tif (masked to basin extent)\n", sep="")
  cat("========================================\n")

  if (plots) {
    par(mfrow=c(1,1), mar=c(3,3,3,7))
    terra::plot(rast(file.path(output_dir, "basin.tif")))
    par(mfrow=c(1,1), mar=c(3,3,3,7), new=TRUE)
    terra::plot(rast(file.path(output_dir, "streams.tif")), add=T, col="black")
    par(mfrow=c(1,1), mar=c(3,3,3,7), new=TRUE)
    terra::plot(vect(file.path(output_dir, "gauge_loc_snap.shp")), add=T,col="red")
    par(mfrow=c(1,1), mar=c(3,3,3,7), new=TRUE)
    terra::plot(vect(source_gauge), add=T,col="blue")

    if (writeplots) {
      dev.copy2pdf(file = file.path(output_dir, "basin_unmasked_streamsgauge.pdf"), width = 8, height = 6)
    }
  }

  b = rast(file.path(output_dir, "basin.tif"))
  bsize = sum(b[!is.na(b)]) * res(b)[1] * res(b)[2]

  cat("Basin is ",bsize," in map area units (probably square meters).\nAssuming square meters, in square km, basin is: ",bsize/(1000*1000),".\n")
  cat("With patches as unique grid cells, there would be: ",sum(b[!is.na(b)])," patches.\n")

  maps_out = c("basin" = file.path(output_dir, "basin.tif"), "DEM" = file.path(output_dir, "dem.tif"),"streams" = file.path(output_dir, "streams.tif"), "gauge_snap" = file.path(output_dir, "gauge_loc_snap.shp") )
  return(maps_out)
}

# streams = maps_out[["streams"]]
# basin = maps_out[["basin"]]
# DEM = maps_out[["DEM"]]

# ================================================================================
# trim all maps to basin if needed
#' @export
wbox_trim_to_basin_overwrite = function(maps, basin, output_dir, plots = T, writeplots = T) {
  basin_rast = trim(rast(basin))
  if ("gauge_snap" %in% names(maps)) {
    maps_all = maps
    maps = maps[!names(maps) %in% c("gauge_snap")]
  }

  # trim all maps to basin
  # "basin" - trim to itself
  message(paste0("Trimming basin..."))
  map_trimmed = crop(rast(maps[["basin"]]), basin_rast)
  tmpfile = tempfile(fileext = ".tif")
  writeRaster(map_trimmed, tmpfile, overwrite = TRUE)
  file.rename(tmpfile, maps[["basin"]])
  # "DEM" - trim to basin
  message(paste0("Trimming DEM to basin..."))
  map_trimmed = crop(rast(maps[["DEM"]]), basin_rast)
  tmpfile = tempfile(fileext = ".tif")
  writeRaster(map_trimmed, tmpfile, overwrite = TRUE)
  file.rename(tmpfile, maps[["DEM"]])
  # "streams" - trim to basin
  message(paste0("Trimming streams to basin..."))
  map_trimmed = crop(rast(maps[["streams"]]), basin_rast)
  tmpfile = tempfile(fileext = ".tif")
  writeRaster(map_trimmed, tmpfile, overwrite = TRUE)
  file.rename(tmpfile, maps[["streams"]])
  
  # for (mapname in names(maps)) {
  #   message(paste0("Trimming ", mapname, " to basin..."))
  #   map_trimmed = crop(rast(maps[[mapname]]), basin_rast)
  #   tmpfile = tempfile(fileext = ".tif")
  #   writeRaster(map_trimmed, tmpfile, overwrite = TRUE)
  #   file.rename(tmpfile, maps[[mapname]])
  #   # writeRaster(map_trimmed, filename = maps[[mapname]], overwrite = T)
  # }
  nmaps = length(names(maps))
  rowcol = ceiling(sqrt(nmaps))

  if (plots) {
    # Loop through and plot maps in grid
    par(mfrow=c(rowcol,rowcol), mar=c(3,3,3,3))
    for (i in seq_along(maps)) {
      terra::plot(rast(maps[i]), main = names(maps)[i])
    }
    if (writeplots) {
      dev.copy2pdf(file = file.path(output_dir, "maps_trimmed.pdf"), width = 8, height = 6)
    }
  }

  if (exists("maps_all")) {
    maps_all[names(maps)] = maps
    maps = maps_all
  }

  return(maps)

}

# ================================================================================
# get subbasins
#' @export
wbox_subbasins = function(dem_brch, streams, output_dir, stream_threshold, tmp_dir = file.path(output_dir, "wb_tmp"), plots = T, writeplots = T, overwrite = T) {

  wbt_d8_pointer(dem = dem_brch, output = file.path(tmp_dir, "dem_brch_basin_ptr_d8.tif"))

  wbt_subbasins(d8_pntr = file.path(tmp_dir, "dem_brch_basin_ptr_d8.tif"), streams = streams, output = file.path(output_dir, "subbasins.tif"))
  
  wbox_subbasins_plot(file.path(output_dir, "subbasins.tif"), streams, output_dir, stream_threshold, tmp_dir, plots = plots, writeplots = writeplots, overwrite = overwrite)

  cat("\n========================================\n")
  cat("Map written to output directory ('",output_dir,"'):\n\tsubbasins.tif\n", sep="")
  cat("========================================\n")

  return(c("subbasins" = file.path(output_dir, "subbasins.tif")))
}

# ================================================================================
# slope, aspect, horizon
#' @export
wbox_slope_aspect_horizons = function(dem_brch, plots = T, writeplots = T, overwrite = T) {

  # ==================== Slope and aspect ====================
  wbt_slope(dem = dem_brch, output = file.path(output_dir, "slope.tif"), units = 'degrees')
  # aspect is standard (0 == 360 == NORTH )
  wbt_aspect(dem = dem_brch, output = file.path(output_dir, "aspect.tif"))

  if (plots) {
    par(mfrow = c(2, 2))
    terra::plot(rast(dem_brch), main = "Filled DEM", col = terrain.colors(50))
    # terra::plot(rast(file.path(tmp_dir, "dem_brch_accum_d8.tif")), main = "Flow Accumulation")
    terra::plot(rast(file.path(output_dir, "slope.tif")), main = "Slope", col = colorRampPalette(c("white", "yellow", "orange", "red", "brown"))(50))
    asp_cols <- colorRampPalette(c("red", "yellow", "green", "cyan", "blue", "magenta", "red"))(72)   # 5-degree bins
    terra::plot(rast(file.path(output_dir, "aspect.tif")), main = "Aspect", col=asp_cols)
    if (writeplots) {
      dev.copy2pdf(file = file.path(output_dir, "plot_DEM_slope_aspect.pdf"), width = 8, height = 6)
    }
  }

  # ==================== Horizons + RHESSys Changes ====================
  wbt_horizon_angle(dem = dem_brch, output = file.path(tmp_dir, "horizon_east.tif"), azimuth = 270, max_dist = 100000)
  wbt_horizon_angle(dem = dem_brch, output = file.path(tmp_dir, "horizon_west.tif"), azimuth = 90, max_dist = 100000)
  # sin of horizon in radians
  horizon_east_sin = sin(rast(file.path(tmp_dir, "horizon_east.tif")) * pi/180)
  horizon_west_sin = sin(rast(file.path(tmp_dir, "horizon_west.tif")) * pi/180)
  writeRaster(horizon_east_sin, file.path(output_dir, "e_horizon.tif"), overwrite = T)
  writeRaster(horizon_west_sin, file.path(output_dir, "w_horizon.tif"), overwrite = T)

  if (plots) {
    par(mfrow = c(1, 2))
    terra::plot(rast(file.path(output_dir, "e_horizon.tif")), main = "East Horizon")
    terra::plot(rast(file.path(output_dir, "w_horizon.tif")), main = "West Horizon")
    if (writeplots) {
      dev.copy2pdf(file = file.path(output_dir, "plot_horizons.pdf"), width = 8, height = 6)
    }
  }

  maps_out = c("aspect" = file.path(output_dir, "aspect.tif"), "slope" = file.path(output_dir, "slope.tif"),"east_horizon" = file.path(output_dir, "e_horizon.tif"), "west_horizon" = file.path(output_dir, "w_horizon.tif"))

  return(maps_out)
}

# subbasin vis
#' @export
wbox_subbasins_vis = function(subbasins, streams, min_subbasin_size = 20) {
  r = rast(subbasins)
  # r = terra::project(x = r, "EPSG:4326", method = "near")
  v = as.polygons(r, round = F, values=T)
  v = terra::project(x = v, "EPSG:4326")
  v_sf <- st_as_sf(v)
  v_sf$AREA = st_area(x = v_sf)

  s = rast(streams)
  sv = as.polygons(s, round = F, values=T)
  sv = terra::project(x = sv, "EPSG:4326")
  sv_sf <- st_as_sf(sv)
  sv_sf$AREA = st_area(x = sv_sf)

  # plot(st_geometry(v_sf), col = sf.colors(nrow(v_sf), categorical = TRUE), border = 'grey', axes = TRUE)
  v_sf$subbasins = as.factor(v_sf$subbasins)
  p = rainbow(n = nrow(v_sf))

  l <- leaflet(v_sf) %>%
    addTiles() %>% # Add a base map
    addMapPane("Subbasins", zIndex = 430) %>% # shown below 
    addMapPane("Streams", zIndex = 450) %>% 
    addPolygons(
      fillColor = p[sample(nrow(v_sf))],
      color = "#000000",
      weight = 1,
      fillOpacity = 0.8,
      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = F), # , bringToFront = TRUE
      label = ~subbasins, 
      popup = ~ paste("Region:", subbasins, "<br>Area:", round(AREA, 2)),
      options = pathOptions(pane = "Subbasins") # Show detailed info on click
    ) %>%
    addPolygons(
      data = sv_sf,
      color = "#000000",
      group = "Stream",
      options = pathOptions(pane = "Streams")
    ) %>%
    addLayersControl(
      overlayGroups = c("Stream"),
      options = layersControlOptions(collapsed = FALSE)
    )

  print(l)
  
  tmp = summary(as.factor(values(r, na.rm =T)))
  badsubs = tmp[tmp<min_subbasin_size]
  tmp = matrix(as.numeric(c(names(badsubs),badsubs)),nrow = 2, byrow = T)
  rownames(tmp) = c("Subbasin ID","Cell Count")

  cat("==============================================\n")
  cat("Number of subbasins below size threshold (",min_subbasin_size," cells): ", length(badsubs),"\n" )
  prmatrix(tmp, collab = rep_len("", length(badsubs)))
  cat("==============================================\n")

  cat("\nReturning matrix of subbasin IDs and sizes\n")
  cat("!!WARNING!! - Leaflet map is projected differently (lat lon not utm meters) and slightly distorted, stream overlay is not perfectly accurate - check regular raster plots for accuracy.")

  return(tmp)
}


#' @export
textlocTL = function(srcmap) {
  pct = 0.95
  exts = terra::ext(srcmap)
  x = exts[2] - ((exts[2] - exts[1])*pct)
  y = exts[3] + ((exts[4] - exts[3])*pct)
  return(c(x,y))
}


#' @export
wbox_subbasins_plot = function(subbasins, streams, output_dir, stream_threshold, tmp_dir = file.path(output_dir, "wb_tmp"), plots = T, writeplots = T, overwrite = T) {


  s = rast(subbasins)
  nsub = length(summary(as.factor(values(s, na.rm=T))))
  subsum = summary(as.factor(values(s, na.rm=T)))
  cellarea = mean(values(cellSize(s))) # in meters
  minsub = round((cellarea * min(subsum) * 0.000001), digits = 3)
  maxsub = round((cellarea * max(subsum) * 0.000001), digits = 3)

  colors <- rainbow(length(seq_along(unique(values(s)))))
  par(mfrow = c(1,1))
  terra::plot(s, main = paste0("Subbasins - stream threshold ",stream_threshold), col=colors)
  terra::plot(rast(streams), col="grey",add=T)
  tl = textlocTL(s)
  textlab = paste0("Based on stream threshold ",stream_threshold, "\n",nsub, " subbasins\n", "Min subbasin ", minsub, " km^2,",min(subsum)," patches\nMax subbasin ",maxsub," km^2, ",max(subsum)," patches")
  text(x = tl[1], y = tl[2], labels =textlab , col = "black", cex = 1, adj = c(0,1))

  if (writeplots) {
    dev.copy2pdf(file = file.path(output_dir, paste0("Subbasin_Streams_thresh",stream_threshold,".pdf")), width = 8, height = 6)
  }
}

#' @export
polaris2texture = function(basin, sand, clay, plot_out, writeplots=T) {
  fill.na <- function(x, i=5) {
    if(is.na(x)[i]) {
      return(mean(x, na.rm = T))
    } else {
      return(x[i])
    }
  }

  # mask_map = rast(file.path(output_dir, "basin.tif"))
  clay = rast(clay)
  sand = rast(sand)

  mask_map = rast(basin)

  clay_proj = project(clay, mask_map, method = "bilinear")
  clay_crop = crop(clay_proj, mask_map)
  clay_mask = mask(clay_crop, mask_map)

  sand_proj = project(sand, mask_map, method = "bilinear")
  sand_crop = crop(sand_proj, mask_map)
  sand_mask = mask(sand_crop, mask_map)

  if (any(is.na(values(clay_crop)))) {
    clay_crop = focal(clay_crop, w = 3, fun = fill.na, na.policy = "only")
  }
  if (any(is.na(values(sand_crop)))) {
    sand_crop = focal(sand_crop, w = 3, fun = fill.na, na.policy = "only")
  }

  # silt = "preprocessing/spatial_source/POLARISOut/mean/" # not needed since should be remainder
  texturedata = data.frame(CLAY = unname(values(clay_crop)), SILT = NA, SAND = unname(values(sand_crop)))
  texturedata$ind = seq(1,nrow(texturedata))
  texturedata$SILT = 100 - (texturedata$CLAY + texturedata$SAND)
  # summary(texturedata)

  # this should be the same as the grass function, getting texture from soil components
  texturedata$TextureName = TT.points.in.classes(tri.data  = texturedata, class.sys = "USDA.TT", PiC.type  = "t")

  usdaID = data.frame(name = c("clay","silty-clay", "silty-clay-loam", "sandy-clay", "sandy-clay-loam", "clay-loam",
                              "silt","silt-loam","loam","sand","loamy-sand","sandy-loam"),
                      ID = c(1:12),
                      TextureName = c("Cl","SiCl", "SiClLo","SaCl","SaClLo","ClLo","Si","SiLo","Lo","Sa","LoSa","SaLo"))
  # to do the conversion between names, to make the above df
  # TT.classes.tbl(class.sys = "USDA.TT")
  texturedata = merge(texturedata,usdaID, by = "TextureName", allx = T, sort = F)
  texturedata = texturedata[order(texturedata$ind),]

  soil_texture = clay_crop
  names(soil_texture) = "soil_texture"
  nrow(texturedata) == length(values(soil_texture))
  values(soil_texture) = texturedata$ID

  soil_texture = mask(soil_texture, mask_map)

  if (plots) {
    soil_texture_plot(soil_texture, plot_out, writeplots)
  }

  return(soil_texture)
}

#' @export
soil_texture_plot = function(soil_texture, plot_out, writeplots = T) {
  usdaID = data.frame(name = c("clay","silty-clay", "silty-clay-loam", "sandy-clay", "sandy-clay-loam", "clay-loam", "silt","silt-loam","loam","sand","loamy-sand","sandy-loam"),ID = c(1:12))

  soiltab = unique(usdaID[usdaID$ID %in% unique(values(soil_texture, rm.na=T)),c("name","ID")])
  tmp = summary(factor(values(soil_texture, na.rm=T)))
  tmp = data.frame(ID = names(tmp),count = tmp)
  soiltab = merge(soiltab,tmp)

  rownames(soiltab) = NULL
  text_rep <- capture.output(print(soiltab))
  
  par(mfrow = c(1, 1))
  terra::plot(soil_texture, main = "Soil Textures")
  tl = textlocTL(soil_texture)
  textlab = paste(text_rep, collapse = "\n")
  text(x = tl[1], y = tl[2], labels =textlab , col = "black", cex = 1, adj = c(0,1))

  if (writeplots) {
    dev.copy2pdf(file = plot_out, width = 8, height = 6)
  }
}
