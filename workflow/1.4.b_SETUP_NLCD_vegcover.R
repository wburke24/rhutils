# ------------------------------ ALTERNATIVE = GET NLCD ------------------------------

# devtools::install_github("ropensci/FedData")
library(FedData)
library(terra)

textlocTL = function(srcmap) {
  pct = 0.95
  exts = ext(srcmap)
  x = exts[2] - ((exts[2] - exts[1])*pct)
  y = exts[3] + ((exts[4] - exts[3])*pct)
  return(c(x,y))
}


mask_map = rast("preprocessing/whitebox/basin.tif")
plot(mask_map)

# maskPolygon <- polygon_from_extent(ext(mask_map), proj4string='+proj=utm +datum=NAD83 +zone=11')

NLCD <- get_nlcd(template=mask_map, label='VEPIIN', year = 2019, force.redo = T, extraction.dir = paste0("preprocessing/spatial_source/NLCD/"))

NLCD_proj = project(as(NLCD, "SpatRaster"), mask_map, method = "near")
NLCD_crop = crop(NLCD_proj, mask_map)
NLCD_mask = mask(NLCD_crop, mask_map)

atttab = levels(NLCD_mask)[[1]]
atttabclip = atttab[atttab$ID %in% unique(values(NLCD_mask, na.rm=T,mat=F)),]
row.names(atttabclip) = NULL
histtable = data.frame(ID = names(summary(as.factor(values(NLCD_mask, na.rm=T)))), Count = summary(as.factor(values(NLCD_mask, na.rm=T))))
histmerge = merge(histtable, atttabclip, by = "ID")

textlab = paste(capture.output(print(histmerge, row.names = F)), collapse = "\n")

plot(NLCD_mask, type = "classes", main = "NLCD baseline classes")
tl = textlocTL(NLCD_mask)
text(x = tl[1], y = tl[2], labels =textlab , col = "black", cex = 1, adj = c(0,1))

dev.copy2pdf(file = file.path("preprocessing/", "NLCD_baseline.pdf"), width = 10, height = 6)

hist(NLCD_mask)
# nlcd_colors()

summary(as.factor(values(NLCD_mask)))

#  ==================== RELCCASS =====================
NLCD_mask_reclass = terra::classify(NLCD_mask, data.frame(c(11, 21, 22, 31, 42, 52, 71, 90, 95), c(31, 31, 31, 31, 7, 50, 50, 7, 50) ))

writeRaster(NLCD_mask_reclass, "preprocessing/spatial90m/NLCD_veg_cover.tif", overwrite = T)

# PLOT for saving
par(mfrow = c(1,2))
plot(LPC_veg_cover_map, type = "classes", main ="LPC Veg Cover")
plot(NLCD_mask_reclass, type = "classes", main = "NLCD Veg Cover")
dev.off()
