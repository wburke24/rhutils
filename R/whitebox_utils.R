# Set of functions to semi automate whitebox tools

# ================================================================================
# resample and trim dem, make breach, d8 pointer, d8 accum, gauge snap, basin extent
#' @export
wbox_dem2streams_gauge_basin = function(source_dem, source_gauge, res, stream_threshold = 100, gauge_snap_dist = 90, output_dir, tmp_dir = file.path(output_dir, "wb_tmp"), plots = T, writeplots = T, overwrite = T) {
  # copy and trim source DEM -- could clip manually here too
  dem_source = trim(rast(source_dem))
  dem_resample = resample(dem_source, rast(ext(dem_source),resolution = res))
  writeRaster(dem_resample, file.path(tmp_dir,"dem_trim.tif"), overwrite=overwrite)

  if (plots) {
    plot(rast(file.path(tmp_dir,"dem_trim.tif")), main = paste0("DEM Resampled to ",res,"m"))
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
  # maps_out = c("dem_brch" = file.path(tmp_dir, "dem_brch.tif"), "dem_dir" = file.path(tmp_dir, "dem_brch_ptr_d8.tif"), "dem_accum" = file.path(tmp_dir, "dem_brch_accum_d8.tif"))
  # return(maps_out)

  wbt_extract_streams(flow_accum = file.path(tmp_dir, "dem_brch_accum_d8.tif"), output = file.path(tmp_dir, "streams.tif"), threshold = stream_threshold)
  #snapped gauge is first final output
  wbt_jenson_snap_pour_points(pour_pts = source_gauge, 
                              streams = file.path(tmp_dir, "streams.tif"), 
                              output = file.path(output_dir, "gauge_loc_snap.shp"), 
                              snap_dist = gauge_snap_dist)

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
    plot(rast(file.path(output_dir, "basin.tif")))
    par(mfrow=c(1,1), mar=c(3,3,3,7), new=TRUE)
    plot(rast(file.path(output_dir, "streams.tif")), add=T, col="black")
    par(mfrow=c(1,1), mar=c(3,3,3,7), new=TRUE)
    plot(vect(file.path(output_dir, "gauge_loc_snap.shp")), add=T,col="red")
    par(mfrow=c(1,1), mar=c(3,3,3,7), new=TRUE)
    plot(vect(source_gauge), add=T,col="blue")

    if (writeplots) {
      dev.copy2pdf(file = file.path(output_dir, "basin_unmasked_streamsgauge.pdf"), width = 8, height = 6)
    }
  }

  maps_out = c("basin" = file.path(output_dir, "basin.tif"), "DEM" = file.path(tmp_dir, "dem_brch.tif"),"streams" = file.path(output_dir, "streams.tif"), "gauge_snap" = file.path(output_dir, "gauge_loc_snap.shp") )
  return(maps_out)
}

# streams = maps_out[["streams"]]
# basin = maps_out[["basin"]]
# DEM = maps_out[["DEM"]]

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
    plot(rast(dem_brch), main = "Filled DEM")
    # plot(rast(file.path(tmp_dir, "dem_brch_accum_d8.tif")), main = "Flow Accumulation")
    plot(rast(file.path(output_dir, "slope.tif")), main = "Slope")
    plot(rast(file.path(output_dir, "aspect.tif")), main = "Aspect")
    if (writeplots) {
      dev.copy2pdf(file = file.path(output_dir, "plot_DEM_slope_aspect.pdf"), width = 8, height = 6)
    }
  }

  # ==================== Horizons + RHESSys Changes ====================
  wbt_horizon_angle(dem = dem_brch, output = file.path(tmp_dir, "horizon_east.tif"), azimuth = 270, max_dist = 100000)
  wbt_horizon_angle(dem = dem_brch, output = file.path(tmp_dir, "horizon_west.tif"), azimuth = 90, max_dist = 100000)
  # sin of radian horizon
  horizon_east_sin = sin(rast(file.path(tmp_dir, "horizon_east.tif")))
  horizon_west_sin = sin(rast(file.path(tmp_dir, "horizon_west.tif")))
  writeRaster(horizon_east_sin, file.path(output_dir, "e_horizon.tif"), overwrite = T)
  writeRaster(horizon_west_sin, file.path(output_dir, "w_horizon.tif"), overwrite = T)

  if (plots) {
    par(mfrow = c(2, 1))
    plot(rast(file.path(output_dir, "e_horizon.tif")), main = "East Horizon")
    plot(rast(file.path(output_dir, "w_horizon.tif")), main = "West Horizon")
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
wbox_subbasins_plot = function(subbasins, streams, output_dir, stream_threshold, tmp_dir = file.path(output_dir, "wb_tmp"), plots = T, writeplots = T, overwrite = T) {
  textlocTL = function(srcmap) {
    pct = 0.95
    exts = ext(srcmap)
    x = exts[2] - ((exts[2] - exts[1])*pct)
    y = exts[3] + ((exts[4] - exts[3])*pct)
    return(c(x,y))
  }

  s = rast(subbasins)
  nsub = length(summary(as.factor(values(s, na.rm=T))))
  subsum = summary(as.factor(values(s, na.rm=T)))
  cellarea = mean(values(cellSize(s))) # in meters
  minsub = round((cellarea * min(subsum) * 0.000001), digits = 3)
  maxsub = round((cellarea * max(subsum) * 0.000001), digits = 3)

  colors <- rainbow(length(seq_along(unique(values(s)))))
  par(mfrow = c(1,1))
  plot(s, main = paste0("Subbasins - stream threshold ",stream_threshold), col=colors)
  plot(rast(streams), col="grey",add=T)
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
  plot(soil_texture, main = "Soil Textures")
  tl = textlocTL(soil_texture)
  textlab = paste(text_rep, collapse = "\n")
  text(x = tl[1], y = tl[2], labels =textlab , col = "black", cex = 1, adj = c(0,1))

  if (writeplots) {
    dev.copy2pdf(file = plot_out, width = 8, height = 6)
  }
}
