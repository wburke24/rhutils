# pctcover2MSRrules
# 
# Uses an input basin boundary map and set of lifeform percent cover maps (tree, shrub, herb, other)
# using these inputs it creates a set of MSR rules and a corresponding map
library(rhutils)
# devtools::install_github("wburke24/rhutils")
library(terra)
library(data.table)
# source("R/fun_MSR_thinning.R")
library(gridExtra)
library(patchwork)

# ------------------------------ INPUTS ------------------------------
plots = T
lpc_data_path = "~/Projects/CARB/data/Landcover/2019/" 
# to get the grid
mask_map_path = "preprocessing/whitebox/basin.tif"

# find the LPC data
lpc_grid = vect("~/Projects/CARB/data/Landcover/CONUS_C2_ARD_grid/conus_c2_ard_grid.shp")
mask_rast = rast(mask_map_path)
mask_rast_prj = project(mask_rast,crs(lpc_grid))
grid_crop = crop(lpc_grid,mask_rast_prj)
grid_crop_vals = values(grid_crop)

lpc_pattern = paste0("h",sprintf("%0*d", 3, grid_crop_vals$h),"v", sprintf("%0*d", 3, grid_crop_vals$v))
# lpc_pattern = "h003v008"

round_value = 10

output_rules_map = "preprocessing/whitebox/rules_LPC_30m.tif"
output_rules_file = "preprocessing/rules/LPC_30m.rules"

output_rules_map = "preprocessing/whitebox/rules_LPC_30m_tree2shrub.tif"
output_rules_file = "preprocessing/rules/LPC_30m_tree2shrub.rules"

output_rules_map = "preprocessing/whitebox/rules_LPC_30m_tree2shrubnobare.tif"
output_rules_file = "preprocessing/rules/LPC_30m_tree2shrubnobare.rules"

output_std_veg_cover = "preprocessing/whitebox/LPC_veg_cover.tif"

# --------------------------------------------------------------------

# ============================== EXTRACT ==============================
# If your basin covers multiple lpc tiles you may need to edit this function
lpc_map_list = lpc_extract(lpc_data_path = lpc_data_path, lpc_pattern = lpc_pattern, mask_map_path = mask_map_path)


if (plots) {
  par(mfrow = c(4,2), cex = 0.4)
  # par(mfrow = c(2,4), cex = 0.4)
  plot(trim(lpc_map_list$tc[[1]]), main = "Tree")
  hist(lpc_map_list$tc[[1]], main = "Tree")
  plot(trim(lpc_map_list$sc[[1]]), main = "Shrub")
  hist(lpc_map_list$sc[[1]], main = "Shrub")
  plot(trim(lpc_map_list$hc[[1]]), main = "Herb")
  hist(lpc_map_list$hc[[1]], main = "Herb")
  plot(trim(lpc_map_list$oc[[1]]), main = "Other/Bare")
  hist(lpc_map_list$oc[[1]], main = "Other/Bare")

  dev.copy2pdf(file = file.path("preprocessing/", "LPC_quad_unbinned.pdf"), width = 6, height = 10)

}

# ============================== CONVERT TO DF ==============================
lpc_df = lpc_as_df(lpc_map_list)
cat("Mean sum variance from 100%: ", mean(abs(lpc_df$sum-100)))

# =================== ADJUST FOR REDROCK - TREES BECOME SHRUBS
lpc_df$Shrub = lpc_df$Shrub + lpc_df$Tree
lpc_df$Tree = 0


# ============================== BIN/ROUND ==============================

# round_value = 10
# round_value = 20
# round_value = 25
# round_value = 30
# round_value = 40

lpc_bin_df = lpc_bin(lpc_df = lpc_df, round_value = round_value)

# ======================= VER 2 - BARE EARTH BECOMES ITS OWN MAP FOR COVER FRAC
bare_map = lpc_bin_df[,c("id","Other_rnd_crt")]
bare_map$cover_frac = 1 - (bare_map$Other_rnd_crt/100)

lpc_bin_df_nobare = lpc_bin_df

lpc_bin_df_nobare$sh_hr = lpc_bin_df_nobare$Shrub_rnd + lpc_bin_df_nobare$Herb_rnd_crt
lpc_bin_df_nobare$Shrub_rnd2 = lpc_bin_df_nobare$Shrub_rnd * 100/lpc_bin_df_nobare$sh_hr
lpc_bin_df_nobare$Herb_rnd_crt2 = lpc_bin_df_nobare$Herb_rnd_crt * 100/lpc_bin_df_nobare$sh_hr

lpc_bin_df_nobare[is.nan(lpc_bin_df_nobare$Shrub_rnd2),"Herb_rnd_crt2"] = 100
lpc_bin_df_nobare[is.nan(lpc_bin_df_nobare$Shrub_rnd2),"Shrub_rnd2"] = 0

lpc_bin_df_nobare$sh_hrsum2 = lpc_bin_df_nobare$Herb_rnd_crt2 + lpc_bin_df_nobare$Shrub_rnd2

lpc_bin_df_nobare[is.nan(lpc_bin_df_nobare$sh_hrsum2),]
unique(lpc_bin_df_nobare$sh_hrsum2)

lpc_bin_df_nobare$Shrub_rnd = lpc_bin_df_nobare$Shrub_rnd2
lpc_bin_df_nobare$Herb_rnd_crt = lpc_bin_df_nobare$Herb_rnd_crt2
lpc_bin_df_nobare$Other_rnd_crt = 0

lpc_bin_df = lpc_bin_df_nobare[,1:5]
lpc_bin_df$Shrub_rnd = ceiling(lpc_bin_df$Shrub_rnd)
lpc_bin_df$Herb_rnd_crt = floor(lpc_bin_df$Herb_rnd_crt)

# join bare earth to raster
Mode <- function(x) {
  ux <- unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x, ux)))]
}
fill.nan <- function(x, i=5) {
  if(is.nan(x)[i]) {
    return(Mode(x))
  } else {
    return(x[i])
  }
}

mask_map = rast(mask_map_path)
vals_map = lpc_map_list$tc[[1]]
values(vals_map)[!is.na(values(vals_map))] <- bare_map$cover_frac

# IF THERES MISSING DATA:
# fill in data missing from lpc (which is present in mask) with mode of surrounding rules
if (sum(is.na(values(vals_map)) & !is.na(values(mask_map))) > 0) {
  cat("Number of non-masked NA pixels:", sum(is.na(values(vals_map)) & !is.na(values(mask_map))),"\n")
  vals_map_filled = focal(vals_map, w = 3, fun = fill.nan, na.policy = "only")
  cat("Missing cells filled via mode of neighbors.\n") 
  cat("Remaining number of non-masked NA pixels:", sum(is.na(values(vals_map_filled)) & !is.na(values(mask_map))),"\n" )
} else {
  vals_map_filled = vals_map
}


dev.off()
pdf(paste0("preprocessing/cover_fraction_LPC.pdf"), width = 12, height = 8)
plot(vals_map_filled, main = "Cover Fraction")
dev.off()


writeRaster(vals_map_filled, "preprocessing/whitebox/cover_frac.tif", overwrite=T)

# ============================== ALTERNATIVE BINNING/FIND N CLASSES ==============================

altbin = F
if (altbin) {
  # lpc_bin_df = lpc_bin(lpc_df = lpc_df, round_value = round_value)

  # quantile binning
  # k means clustering
  # hierarchical clustering
  # self organizing map

  # data <- data.frame(
  #   A = runif(100, 0, 50),
  #   B = runif(100, 0, 30),
  #   C = runif(100, 0, 20),
  #   D = runif(100, 0, 100)
  # )
  # data  = lpc_df

  # set.seed(123)
  # k <- 5
  # km <- kmeans(data[, 1:4], centers = k)

  # # Add cluster information to data
  # data$cluster <- km$cluster

  # # Aggregate and compute averages
  # result <- data %>%
  #   group_by(cluster) %>%
  #   summarize(across(Tree:Other, mean)) %>%
  #   mutate(across(Tree:Other, ~ . / sum(.) * 100)) # Normalize rows to sum to 100%

  # result$sum = apply(result,1,sum)

  # hist(data$cluster)
}

# ============================== MAKE STD (non-thinning) RULES MAP AND TABLE
outlist = make_std_rules(lpc_bin_df)
lpc_rules_df = outlist[[1]] # cover data and rule ID for each patch
unique_rules = outlist[[2]] # unique cover combinations/rules for the basin
#baseline rules
rules_map_std = make_rules_map(lpc_rules_df, mask_map_path, lpc_map_list)
names(rules_map_std) = "RuleID"

# TREES 2 SHRUBS
rast_plot = as.data.frame(rules_map_std, xy=T) %>% ggplot(aes(x=x,y=y,fill = factor(RuleID))) + geom_tile() +
  theme_minimal() + 
  labs(title = paste0("Binned Rules Map ",round_value,"% bins. Trees onverted to shrubs"), fill = "RuleID")
table_grob <- tableGrob((unique_rules))

pdf(paste0("preprocessing/LPC_bin",round_value,"trees2shrubs.pdf"), width = 12, height = 8) # Open a PDF device
grid.arrange(rast_plot, table_grob, ncol = 2)
dev.off()

# NO BARE EARTH
rast_plot = as.data.frame(rules_map_std, xy=T) %>% 
  ggplot(aes(x=x,y=y,fill = factor(RuleID))) + 
  geom_tile() +
  theme_minimal() + 
  labs(title = paste0("Binned Rules Map ",round_value,"% bins. Trees onverted to shrubs, bare earth removed."), fill = "RuleID")
table_grob <- tableGrob((unique_rules))

pdf(paste0("preprocessing/LPC_bin",round_value,"trees2shrubsnobare.pdf"), width = 12, height = 8) # Open a PDF device
grid.arrange(rast_plot, table_grob, ncol = 2)
dev.off()

# NO BARE EARTH -------------- pretty plot
unique_rules2 = unique_rules[,c(5,2,3)]
names(unique_rules2) = c("ID","Shrub %","Herb %")
rownames(unique_rules2) = NULL
rast_plot <- as.data.frame(rules_map_std, xy = T) %>%
  ggplot(aes(x = x, y = y, fill = factor(RuleID))) +
  geom_tile() +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank()
  ) +
  labs(title = paste0("Red Rock Vegetation Cover - Landform Percent Cover"), fill = "ID", y = NULL, x = NULL)


rownames(unique_rules2) = NULL
table_grob <- tableGrob(unique_rules2, rows=NULL)

# p = grid.arrange(rast_plot, table_grob, ncol = 2)
p = grid.arrange(rast_plot, table_grob, ncol = 2, widths = c(2, 1))

ggsave(paste0("preprocessing/LPC_bin",round_value,"trees2shrubsnobare_pretty.jpg"),plot = p, width = 12, height = 8)
ggsave(paste0("preprocessing/LPC_bin",round_value,"trees2shrubsnobare_pretty.pdf"),plot = p, width = 12, height = 8)

ex = ext(rules_map_std)

p2 = rast_plot + annotation_custom(
  grob = table_grob,
  xmin = ex[1]+30, xmax = ex[1]+180,  # Stick to left
  ymin = ex[3]+200, ymax = ex[3]+500   # Stick to bottom
)
ggsave(paste0("preprocessing/LPC_bin",round_value,"trees2shrubsnobare_pretty2.jpg"),plot = p2, width = 10, height = 10)
ggsave(paste0("preprocessing/LPC_bin",round_value,"trees2shrubsnobare_pretty2.pdf"),plot = p2, width = 10, height = 10)

# pdf(paste0("preprocessing/LPC_bin",round_value,"trees2shrubsnobare_pretty.pdf"), width = 12, height = 8) # Open a PDF device
# grid.arrange(rast_plot, table_grob, ncol = 2)
# dev.off()
# ---------------------------------------------------

if (plots) {
  # par(mfrow = c(1,1))
  rast_plot = as.data.frame(rules_map_std, xy=T) %>% ggplot(aes(x=x,y=y,fill = factor(RuleID))) + geom_tile() +
    theme_minimal() + 
    labs(title = paste0("Binned Rules Map ",round_value,"% bins."), fill = "RuleID")
  table_grob <- tableGrob((unique_rules))

  pdf(paste0("preprocessing/LPC_bin",round_value,".pdf"), width = 12, height = 8) # Open a PDF device
  grid.arrange(rast_plot, table_grob, ncol = 2)
  dev.off()

  # jpeg(paste0("preprocessing/LPC_bin",round_value,".jpg"), width = 1200, height = 800, quality = 150, pointsize = 14) # Open a PDF device
  # grid.arrange(rast_plot, table_grob, ncol = 2)
  # dev.off()

}

writeRaster(rules_map_std, output_rules_map, overwrite = T)

# ============================== LPC2std_veg_cover ==============================
# get non-msr veg cover - mode of lpc classes
LPC_std_vegcover = LPC2std_veg_cover(lpc_df, mask_map_path, lpc_map_list, vegid_tree = 41, vegid_shrub = 52, vegid_herb = 71, vegid_other = 31)
if (plots) {
  plot(LPC_std_vegcover, main = "LPC-based mode of veg cover")
}
writeRaster(LPC_std_vegcover, output_std_veg_cover, overwrite = T)

# ============================== WRITE RULES FILE ==============================
write_rules_file(
  unique_rules,
  output_rules_file,
  vegid_tree = 42,
  vegid_shrub = 52,
  vegid_herb = 71,
  vegid_other = 31, #bare earth
  strata_ct = NULL
)


# basin = vect("preprocessing/spatial_source/fromseka/basin.shp")
# sub = vect("preprocessing/spatial_source/fromseka/subbasin.shp")

# par(mfrow = c(1,1))
# plot(basin)
# plot(sub)
