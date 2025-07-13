# 1.2.4_whitebox_TEST.R
# 
# https://whiteboxr.gishub.org/articles/demo.html

# install.packages("whitebox")
# whitebox::install_whitebox()

library(whitebox)
library(terra)
library(rhutils)

wbt_version()
wbt_init()

textlocTL = function(srcmap) {
  pct = 0.95
  exts = ext(srcmap)
  x = exts[2] - ((exts[2] - exts[1])*pct)
  y = exts[3] + ((exts[4] - exts[3])*pct)
  return(c(x,y))
}

# ==================== Existing maps ====================
# fieldsite = vect("preprocessing/spatial_source/fromseka/field site.shp")
# basin_proj = project(basin, crs(fieldsite))
# plot(basin_proj)
# plot(fieldsite,add=T)

# ==================== Inputs ====================
# source DEM
dem_source_path = "preprocessing/spatial_source/ned_dem_basinclip.tif"
# res = 90
res = 30

# source gauge location shapefile, snap dist in map unit (m)
# gauge_source = "preprocessing/spatial_source/basin_gauge.shp"
gauge_source = "preprocessing/whitebox/gauge_subbasin.shp"

gauge_snap_dist = 120

stream_threshold=275
# stream_threshold=150
# stream_threshold=75

# for final and temp output
output_dir = "preprocessing/whitebox"
wb_tmp = file.path(output_dir, "wb_tmp")

plots = T
writeplots = T
testing = F

# check + add folders
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
} 
if (!dir.exists(wb_tmp)) {
  dir.create(wb_tmp)
}


# ==================== starting DEM ====================
# copy and trim source DEM -- could clip manually here too
dem_source = rast(dem_source_path)
writeRaster(trim(dem_source), file.path(wb_tmp,"dem_trim.tif"), overwrite=T)

# ==================== CHANGE RESOLUTION HERE ====================
dem_resample = resample(dem_source, rast(ext(dem_source),resolution = res))

writeRaster(trim(dem_resample), file.path(wb_tmp,"dem_trim.tif"), overwrite=T)

if (plots) {
  plot(rast(file.path(wb_tmp,"dem_trim.tif")))
  # rast(file.path(wb_tmp,"dem_trim.tif"))
}

# ==================== Fill, d8 direction, accumulation ====================
## Breach depressions to ensure continuous flow
wbt_breach_depressions(dem = file.path(wb_tmp,"dem_trim.tif"), output = file.path(wb_tmp, "dem_brch.tif"))
# wbt_breach_depressions_least_cost(dem = file.path(wb_tmp,"dem_trim.tif"), output = file.path(wb_tmp, "dem_brch.tif"), dist = 50)
# wbt_fill_depressions(dem = file.path(wb_tmp,"dem_trim.tif"), output = file.path(wb_tmp, "dem_brch.tif"))

## Generate d8 flow pointer (note: other flow directions are available)
wbt_d8_pointer(dem = file.path(wb_tmp, "dem_brch.tif"), output = file.path(wb_tmp, "dem_brch_ptr_d8.tif"))
# this is fractional? d8, more equivilent to previous grass method
# wbt_fd8_pointer(dem = file.path(wb_tmp, "dem_brch.tif"), output = file.path(wb_tmp, "dem_brch_ptr_fd8.tif"))
## Generate d8 flow accumulation in units of cells (note: other flow directions are available)
wbt_d8_flow_accumulation(input = file.path(wb_tmp, "dem_brch.tif"), output = file.path(wb_tmp, "dem_brch_accum_d8.tif"), out_type = "cells")
# wbt_fd8_flow_accumulation(dem = file.path(wb_tmp, "dem_brch.tif"), output = file.path(wb_tmp, "dem_brch_accum_fd8.tif"), out_type = "cells")

# ==================== Streams ====================
## Generate streams with a stream initiation threshold of based on threshold param above
######## RERUN FROM HERE IF THRESHOLD IS CHANGED ##################
wbt_extract_streams(flow_accum = file.path(wb_tmp, "dem_brch_accum_d8.tif"), output = file.path(wb_tmp, "streams.tif"), threshold = stream_threshold)

# ==================== Basin, basin outlet ====================
wbt_jenson_snap_pour_points(pour_pts = gauge_source, 
                            streams = file.path(wb_tmp, "streams.tif"), 
                            output = file.path(output_dir, "gauge_loc_snap.shp"), 
                            snap_dist = gauge_snap_dist)

wbt_watershed(d8_pntr = file.path(wb_tmp, "dem_brch_ptr_d8.tif"), 
              pour_pts = file.path(output_dir, "gauge_loc_snap.shp"), 
              output = file.path(wb_tmp, "basin.tif"))

if (plots) {
  par(mfrow=c(1,1), mar=c(3,3,3,7))
  plot(rast(file.path(wb_tmp, "basin.tif")))
  par(mfrow=c(1,1), mar=c(3,3,3,7), new=TRUE)
  plot(rast(file.path(wb_tmp, "streams.tif")), add=T, col="black")
  par(mfrow=c(1,1), mar=c(3,3,3,7), new=TRUE)
  plot(vect(file.path(output_dir, "gauge_loc_snap.shp")), add=T,col="red")
  par(mfrow=c(1,1), mar=c(3,3,3,7), new=TRUE)
  plot(vect(gauge_source), add=T,col="blue")

  if (writeplots) {
    dev.copy2pdf(file = file.path(output_dir, "basin_unmasked_streamsgauge.pdf"), width = 8, height = 6)
  }
}

# ==================== Crop and Trim by basin ====================
# crop tRIM FIRST
basin = rast(file.path(wb_tmp, "basin.tif"))
plot(basin)

dem_brch_mask = mask(rast(file.path(wb_tmp, "dem_brch.tif")),basin)
dem_brch_mask = trim(dem_brch_mask)
writeRaster(dem_brch_mask,filename = file.path(output_dir, "dem.tif"), overwrite = T)

streams_mask = mask(rast(file.path(wb_tmp, "streams.tif")),basin)
streams_mask = crop(streams_mask, trim(basin))
writeRaster(streams_mask,filename = file.path(output_dir, "streams.tif"), overwrite = T)

ptr_mask = mask(rast(file.path(wb_tmp, "dem_brch_ptr_d8.tif")),basin)
ptr_mask = trim(ptr_mask)
writeRaster(ptr_mask,filename = file.path(wb_tmp, "dem_brch_ptr_d8.tif"), overwrite = T)

basin = trim(basin)
writeRaster(basin,filename = file.path(output_dir, "basin.tif"), overwrite = T)

# ==================== Subbasins/hillslopes ====================
# HILLSLOPES DOESNT WORK SINCE IT EXCLUDES THE STREAM PIXELS
wbt_subbasins(d8_pntr = file.path(wb_tmp, "dem_brch_ptr_d8.tif"), streams = file.path(output_dir, "streams.tif"), output = file.path(wb_tmp, "subbasins.tif"))

subbasins = rast(file.path(wb_tmp, "subbasins.tif"))
plot(subbasins)
subbasins = subst(subbasins, from = unique(values(subbasins, na.rm=T)),to = seq_along(unique(values(subbasins, na.rm=T))))

nsub = length(summary(as.factor(values(subbasins, na.rm=T))))
subsum = summary(as.factor(values(subbasins, na.rm=T)))
cellarea = mean(values(cellSize(subbasins)))
minsub = round((cellarea * min(subsum) * 0.000001), digits = 3)
maxsub = round((cellarea * max(subsum) * 0.000001), digits = 3)

colors <- rainbow(length(seq_along(unique(values(subbasins)))))
par(mfrow = c(1,1))
plot(subbasins, main = "Subbasins", col=colors)
plot(streams_mask, col="grey",add=T)
tl = textlocTL(subbasins)
textlab = paste0("Based on stream threshold ",stream_threshold, "\n",nsub, " subbasins\n", "Min subbasin ", minsub, " km^2,",min(subsum)," patches\nMax subbasin ",maxsub," km^2, ",max(subsum)," patches")
text(x = tl[1], y = tl[2], labels =textlab , col = "black", cex = 1, adj = c(0,1))

# field = vect("preprocessing/spatial_source/fromseka/field site.shp")
# fieldprj = project(field, subbasins)
# plot(fieldprj, add=T)

if (writeplots) {
  dev.copy2pdf(file = file.path(output_dir, paste0("Subbasin_Streams_thresh",stream_threshold,".pdf")), width = 8, height = 6)
}

# check if there are tiny subbasins
tmp = summary(as.factor(values(subbasins)))
tmp[tmp<20]
length(tmp[tmp<20])

reclasshill = F
if (reclasshill) {
  # reclass badhill into targethill
  badhill = 25
  targethill = 29
  
  subset = subbasins
  subset[subset != badhill] = NA
  tarsub = subbasins
  tarsub[tarsub != targethill] = NA
  
  plot(subbasins, main = "Subbasins", col=colors)
  plot(streams_mask, col="grey",add=T)
  plot(subset, col = "black",add=T)
  plot(tarsub, col="brown",add=T)

  tmp = subbasins
  tmp[tmp == badhill] = targethill

  tmp2  = summary(as.factor(values(tmp)))
  tmp2[tmp2<20]
  
  plot(tmp)
  
  writeRaster(x = tmp, filename = file.path(output_dir, "subbasins.tif"), overwrite = T)
} else {
  writeRaster(x = subbasins, filename = file.path(output_dir, "subbasins.tif"), overwrite = T)
}

if (writeplots) {
  dev.copy2pdf(file = file.path(output_dir, paste0("Subbasin_Streams_thresh",stream_threshold,".pdf")), width = 8, height = 6)
}

# ==================== SUBSET TO SUBBASIN CONTAINING FIELD SITE ====================
# FIND CORRECT SUBBASIN, CREATE POINT FOR NEW GAUGE, RERUN SETUP AT 30 METER SINCE BASIN SHOULD BE SMALL, AND THEN GET SUBBASINS/HILLS FOR NEW SMALLER BASIN
subset_subbasin = F
if (subset_subbasin) {
  field = vect("preprocessing/spatial_source/fromseka/field site.shp")
  fieldprj = project(field, subbasins)

  subbasin_num = zonal(x = subbasins, z = fieldprj)

  basin_site = subbasins
  basin_site[basin_site != as.numeric(subbasin_num)] = NA
  basin_site = trim(basin_site)
  plot(basin_site)



  library(leaflet)
  library(mapedit)

  basin_site_prj = project(basin_site, "EPSG:4326")

  m <- leaflet() %>%
    addTiles() %>%  # Add base map tiles
    addRasterImage(basin_site_prj, opacity = 0.8)  # Add the raster layer
  points <- mapedit::editMap(m)
  gauge_new = vect(points$finished)
  gauge_new = project(gauge_new,basin_site)

 
  plot(basin_site)
  plot(gauge_new,add=T)
  tl = textlocTL(basin_site)
  textlab = paste0("Basin size: ",expanse(basin_site, unit = "km")[[2]],"sq km\nPatch count: ",
  length(values(basin_site, na.rm=T)),"\n")
  text(x = tl[1], y = tl[2], labels =textlab , col = "black", cex = 1, adj = c(0,1))

  if (writeplots) {
    dev.copy2pdf(file = file.path(output_dir, paste0("Subbasin_selection_thresh",stream_threshold,".pdf")), width = 8, height = 6)
  }

  writeVector(gauge_new, filename = file.path(output_dir, "gauge_subbasin.shp"))
}

# ==================== Slope and aspect ====================
wbt_slope(dem = file.path(output_dir, "dem.tif"), output = file.path(output_dir, "slope.tif"), units = 'degrees')
# aspect is standard (0 == 360 == NORTH )
wbt_aspect(dem = file.path(output_dir, "dem.tif"), output = file.path(output_dir, "aspect.tif"))

if (plots) {
  par(mfrow = c(2, 2))
  plot(rast(file.path(wb_tmp, "dem_brch.tif")), main = "Filled DEM")
  plot(rast(file.path(wb_tmp, "dem_brch_accum_d8.tif")), main = "Flow Accumulation")
  plot(rast(file.path(output_dir, "slope.tif")), main = "Slope")
  plot(rast(file.path(output_dir, "aspect.tif")), main = "Aspect")
  if (writeplots) {
    dev.copy2pdf(file = file.path(output_dir, "plots1_dem_acc_slope_aspect.pdf"), width = 8, height = 6)
  }
}

# ==================== Horizons + RHESSys Changes ====================
wbt_horizon_angle(dem = file.path(output_dir, "dem.tif"), output = file.path(wb_tmp, "horizon_east.tif"), azimuth = 270, max_dist = 100000)
wbt_horizon_angle(dem = file.path(output_dir, "dem.tif"), output = file.path(wb_tmp, "horizon_west.tif"), azimuth = 90, max_dist = 100000)
# sin of radian horizon
horizon_east_sin = sin(rast(file.path(wb_tmp, "horizon_east.tif")))
horizon_west_sin = sin(rast(file.path(wb_tmp, "horizon_west.tif")))
writeRaster(horizon_east_sin, file.path(output_dir, "e_horizon.tif"), overwrite = T)
writeRaster(horizon_west_sin, file.path(output_dir, "w_horizon.tif"), overwrite = T)

if (plots) {
  par(mfrow = c(2, 2))
  plot(rast(file.path(output_dir, "e_horizon.tif")), main = "East Horizon")
  plot(rast(file.path(output_dir, "w_horizon.tif")), main = "West Horizon")
  plot(rast( file.path(output_dir, "streams.tif")), main = "Streams")
  plot(rast(file.path(output_dir, "subbasins.tif")), main = "Subbasins")
  if (writeplots) {
    dev.copy2pdf(file = file.path(output_dir, "plots2_horizons_streamfs_subbasins.pdf"), width = 8, height = 6)
  }
}

# ==================== Soils ====================
# echo "import soils layers, originally from R-polaris workflow"
# r.import input=C:\Users\burke\Documents\CARB\Pitman\preprocessing\spatial_source\POLARISOut\mean\clay\0_5\lat3738_lon-120-119.tif output=clay_0_5
# r.import input=C:\Users\burke\Documents\CARB\Pitman\preprocessing\spatial_source\POLARISOut\mean\sand\0_5\lat3738_lon-120-119.tif output=sand_0_5
# r.soils.texture sand=sand_0_5@PERMANENT clay=clay_0_5@PERMANENT scheme=C:\Users\burke\Documents\CARB\data\USDA.dat output=soil_texture
# echo "import the soil texture"
# r.proj input=soil_texture location=soil mapset=rhessys output=soil_texture method=nearest --v --o
# r.out.gdal in=soil_texture output="%base%/soil_texture.tif" format=GTiff --o

library(soiltexture)

mask_map = rast(file.path(output_dir, "basin.tif"))

clay = rast("preprocessing/spatial_source/POLARISOut/mean/clay/0_5/lat3940_lon-120-119.tif")
sand = rast("preprocessing/spatial_source/POLARISOut/mean/sand/0_5/lat3940_lon-120-119.tif")

clay_proj = project(clay, mask_map, method = "bilinear")
clay_crop = crop(clay_proj, mask_map)
clay_mask = mask(clay_crop, mask_map)

sand_proj = project(sand, mask_map, method = "bilinear")
sand_crop = crop(sand_proj, mask_map)
sand_mask = mask(sand_crop, mask_map)

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

# cat(paste(text_rep, collapse = "\n"))

if (plots) {
  tmp = unique(texturedata[texturedata$ID %in% unique(values(soil_texture, rm.na=T)),c("name","ID")])
  rownames(tmp) = NULL
  text_rep <- capture.output(print(tmp))
  
  par(mfrow = c(1, 1))
  plot(soil_texture, main = "Soil Textures")
  tl = textlocTL(soil_texture)
  textlab = paste(text_rep, collapse = "\n")
  text(x = tl[1], y = tl[2], labels =textlab , col = "black", cex = 1, adj = c(0,1))

  if (writeplots) {
    dev.copy2pdf(file = file.path(output_dir, "SoilTextures_baseline.pdf"), width = 8, height = 6)
  }
}

# RECLASS HERE
m = matrix(c(8, 12), 
           ncol = 2, byrow = T)
new_soil_texture = classify(soil_texture, m)

if (plots) {
  tmp = unique(texturedata[texturedata$ID %in% unique(values(new_soil_texture, rm.na=T)),c("name","ID")])
  rownames(tmp) = NULL
  text_rep <- capture.output(print(tmp))

  par(mfrow = c(1, 1))
  plot(new_soil_texture, main = "Reclassed Soil Textures")
  tl = textlocTL(new_soil_texture)
  textlab = paste(text_rep, collapse = "\n")
  text(x = tl[1], y = tl[2], labels =textlab , col = "black", cex = 1, adj = c(0,1))

  if (writeplots) {
    dev.copy2pdf(file = file.path(output_dir, "SoilTextures_reclass.pdf"), width = 8, height = 6)
  }
}

writeRaster(new_soil_texture, file.path(output_dir, "soils.tif"),overwrite=T)

# ==================== TROUBLESHOOTING -- Check Maps ====================
if (testing) {
  template = "preprocessing/template/carb_msr.template"
  map_dir = "preprocessing/whitebox/"
  streams = "streams.tif"
  temp_read = template_read(template = template)
  maps_in = c(unique(temp_read[[5]][,2]), streams)
  # map_paths = file.path(map_dir, maps_in)
  
  # ==================== Check exists ====================
  file_paths = vector(mode = "character")
  for (i in maps_in) { #simple check
    file = list.files(path = map_dir, pattern = paste("^",i,"$",sep = ""),full.names = TRUE)
    file_paths = c(file_paths, file)
  }
  length(maps_in) == length(file_paths)
  
  # ==================== try read ====================
  read_stack = try(terra::rast(x = file_paths))
  names(read_stack) = maps_in
  plot(read_stack)
  
  # ==================== Check projection + source driver ====================
  # Check projections (read_stack will error if proj is different, but arguments might be different) -----
  p = vector(mode = "character",length = length(read_stack[1]))
  d = p
  for (i in 1:length(read_stack[1])) {
    p[i] = terra::crs(read_stack[[i]])
    text = sf::gdal_utils(util = "info", source = file_paths[i], quiet = T)
    pattern <- "Driver: (.+?)(?=\n|$)"
    d[i] <- sub("Driver: ","", unlist(regmatches(text, gregexpr(pattern, text, perl = TRUE))))
  }
  cat("Unique map projections (including args): ",length(unique(p)))
  
  # ==================== NaN and NA handling ====================
  # Handle NaNs - set to NA
  cat("Setting NaNs to NA.\n")
  terra::values(read_stack)[is.nan(terra::values(read_stack))] = NA
  
  cat("Trimming NAs.\n")
  read_stack = terra::trim(read_stack) #get rid of extra background

  read_stack = mask(read_stack,read_stack$basin.tif)
  
  read_stack$rules_LPC_90m.tif[!is.na(read_stack$basin.tif) & is.na(read_stack$rules_LPC_90m.tif) ]
  plot(read_stack$basin.tif)
  plot(!is.na(read_stack$basin.tif) & is.na(read_stack$rules_LPC_90m.tif), col = c("transparent","black"), add=T)
  plot(read_stack$streams.tif, add=T, col = "red")
  
  plot(trim(rast("preprocessing/spatial90m/rules_LPC_90m.tif")), col = "black")
  plot(trim(rast("preprocessing/spatial90m/basin.tif")), add=T)
  plot(trim(rast("preprocessing/spatial90m/dem.tif")), add=T)
  x = trim(rast(dem_source_path))
  plot(x)
  plot(x == min(values(x),na.rm = T), add = T, col =  c("transparent","black"))
  
  # Check for missing data (within world map mask) - no fix, just an error since I think this will break things if left unchecked
  cat("Checking for missing data within bounds of world map.\n")
  if (!is.null("map_info")) {
    wrld_vals = !is.na(terra::values(read_stack[["basin.tif"]]))
    NAs_in_wrld = lapply(as.data.frame(terra::values(read_stack)), function(X) {sum(is.na( X[wrld_vals]))})
    NAs_in_wrld[[which(grepl("streams",names(NAs_in_wrld))) ]] = NULL
    if (any(NAs_in_wrld > 0) ) {
      cat("One or more maps have NAs within the bounds of the world map, see maps and counts of NAs below:\n")
      print(NAs_in_wrld[NAs_in_wrld > 0])
      
    }
  }
  
  
  map_df = as.data.frame(read_stack)
}


# ==================== Testing ====================
testing = F
if (testing) {
  ### TEST ###
  demf = rast("preprocessing/spatial90m/dem_fill_grass.tif")
  par(mfrow = c(1,1))
  plot(trim(demf), main = "Filled DEM (GRASS)")
  
  whor_grass = rast("preprocessing/spatial90m/west_horizon_grasstest.tif")
  ehor_grass = rast("preprocessing/spatial90m/east_horizon_grasstest.tif")
  
  wbt_horizon_angle(dem = "preprocessing/spatial90m/dem_fill_grass.tif", output = file.path(wb_tmp, "horizon_west_grasstest.tif"), azimuth = 270, max_dist = 100000)
  wbt_horizon_angle(dem = "preprocessing/spatial90m/dem_fill_grass.tif", output = file.path(wb_tmp, "horizon_east_grasstest.tif"), azimuth = 90, max_dist = 100000)
  whor_test = rast(file.path(wb_tmp, "horizon_west_grasstest.tif"))
  ehor_test = rast(file.path(wb_tmp, "horizon_east_grasstest.tif"))
  
  whor_grass_deg = (whor_grass) * (180/pi)
  ehor_grass_deg = (ehor_grass) * (180/pi)
  
  par(mfrow = c(2, 2))
  plot(trim(whor_grass_deg), main = "GRASS West Horizon", breaks = seq(-20, 50, length.out = 11))
  plot(trim(ehor_grass_deg), main = "GRASS East Horizon", breaks = seq(-20, 50, length.out = 11))
  plot(trim(whor_test), main = "WhiteBox West Horizon", breaks = seq(-20, 50, length.out = 11))
  plot(trim(ehor_test), main = "WhiteBox East Horizon", breaks = seq(-20, 50, length.out = 11))
}
