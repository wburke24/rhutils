library(terra)
library(stringr)

textlocTL = function(srcmap) {
  pct = 0.95
  exts = ext(srcmap)
  x = exts[2] - ((exts[2] - exts[1])*pct)
  y = exts[3] + ((exts[4] - exts[3])*pct)
  return(c(x,y))
}

# get shapefile
basin = rast("preprocessing/whitebox/basin.tif")
basin_v = as.polygons(basin)
# plot(basin_v)
# writeVector(basin_v,filename = "preprocessing/basin.shp")

# see note on rap data 

# cover
# Annual forbs and grasses, 	Perennial forbs and grasses, 	Shrubs, 	Trees, 	Bare ground
# from earth engine script:
# PFTs are "AFGC" (Annual forb and grass cover), "BG" (bare ground), "LTR" (litter), 
# "PFGC" (perennial forb and grass cover), "SHR" (shrub cover), and "TREE" (tree cover)
# var PFTs = ee.List(['AFG', 'BGR', 'LTR', 'PFG', 'SHR', 'TRE'])

cover_src = list.files("preprocessing/spatial_source/RAP/",pattern = "VegCover",full.names = T)
rap_cover = lapply(cover_src,rast)
rap_cover = lapply(rap_cover,project,basin)
names(rap_cover) = str_extract(cover_src,"(?<=_)\\d+(?=\\.tif)")

rap_cover_tree = rast(unlist(lapply(rap_cover, "[[","TRE")))
rap_cover_annualforbgrass = rast(unlist(lapply(rap_cover, "[[","AFG")))
rap_cover_perrforbgrass = rast(unlist(lapply(rap_cover, "[[","PFG")))
rap_cover_shrub = rast(unlist(lapply(rap_cover, "[[","SHR")))
rap_cover_litter = rast(unlist(lapply(rap_cover, "[[","LTR")))
rap_cover_bare = rast(unlist(lapply(rap_cover, "[[","BGR")))

rap_cover_mean = rast(c(tree = mean(rap_cover_tree), anngrass = mean(rap_cover_annualforbgrass), perrgrass = mean(rap_cover_perrforbgrass), shrub = mean(rap_cover_shrub), litter = mean(rap_cover_litter), bare = mean(rap_cover_bare)))


pdf(file = "preprocessing/RAP_vegcover.pdf", width = 10, height = 10)
plot(rap_cover_mean)
# plot(basin_v, add=T)
plot(rap_cover_tree)
plot(rap_cover_annualforbgrass)
plot(rap_cover_perrforbgrass)
plot(rap_cover_shrub)
plot(rap_cover_litter)
plot(rap_cover_bare)
dev.off()


cover_frac = 1 - (rap_cover_mean$bare/100)
cover_frac = mask(cover_frac, basin)
plot(cover_frac)
writeRaster(cover_frac,"preprocessing/whitebox/cover_frac.tif",overwrite=TRUE)
#biomass - lbs per acre
# from earth engine script:
# PFTs are 'afgAGB' (annual forb and grass aboveground biomass), 
# 'pfgAGB' (perennial forb and grass aboveground biomass), 
# and 'herbaceousAGB' (total herbaceous aboveground biomass) - others combined
# var PFTs = ee.List(['afgAGB', 'pfgAGB', 'herbaceousAGB'])

biomass_src = list.files("preprocessing/spatial_source/RAP/",pattern = "Biomass",full.names = T)
rap_biomass = lapply(biomass_src,rast)
rap_biomass = lapply(rap_biomass,project,basin)
rap_biomass = lapply(rap_biomass, function(X){X* 0.4535924 * (1/4046.85642)})
names(rap_biomass) = str_extract(biomass_src,"(?<=_)\\d+(?=\\.tif)")

#convert to kg per sq meter
# tmp = rap_biomass[[1]]
# tmp * 0.4535924 * (1/4046.85642)
rap_bio_herb = rast(unlist(lapply(rap_biomass, "[[",3)))

# plot(rap_bio_herb)
rap_bio_herbavg = mean(rap_bio_herb)

pdf(file = "preprocessing/RAP_biomass.pdf", width = 10, height = 10)
plot(rap_bio_herb)
plot(rap_bio_herbavg, main = "2010-19 Mean Biomass kg/m2")
plot(basin_v, add=T)
dev.off()

