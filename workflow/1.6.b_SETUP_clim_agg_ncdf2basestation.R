# aggregate ncdf to a single basestation, based on basin extent

library(ncdf4)
library(terra)
library(stringr)

plot = T
basin = rast("preprocessing/whitebox/basin.tif")
# ext(basin)

# clim_files = list.files(path = "../data/gridmet/",pattern = "agg_met_", full.names = T)
# clim_files = list.files(path = "clim/",pattern = "agg_met_gridmet", full.names = T)
clim_files = list.files(path = "clim/",pattern = "agg_met_gridmet", full.names = T)

# destination folder for cropped + edited ncdf clim files
clim_dest = "clim"

clim_file_vars = gsub("_\\d+.*","", gsub(pattern = ".*agg_met_gridmet_", "", clim_files))

# ------------------------------ Average NETCDF CLIMATE DATA  ------------------------------

# clip the netcdf
clim_list = list()
for (i in seq_along(clim_files)) {
  cat("Reading and clipping ",clim_files[i]," ...\n")
  clim = rast(clim_files[i])
  clim_prj = project(clim, basin, method="near")
  clim_crop = crop(clim_prj, basin)
  clim_mask = mask(clim_crop, basin)

  clim_mean = mean(clim_mask, na.rm=T)
  clim_mean = global(clim_mask, fun = "mean", na.rm = TRUE)
  clim_var = gsub(pattern = "=\\d+",replacement = "",names(clim)[1])
  clim_units = unique(units(clim))
  clim_list[[paste(clim_file_vars[i],clim_var,clim_units,sep="_")]] = clim_mean$mean
}

# write the tmin, tmax, precip, for now
day1 = str_extract(names(clim)[1], "\\d+")
startdate = as.Date("1900-01-01") + as.numeric(day1) - 1
rhstart = paste0(gsub("-"," ",startdate)," 01")

options(scipen = "999")

rainout = c(rhstart,clim_list$pr_precipitation_amount_day_mm/1000)
tminout = c(rhstart,clim_list$tmmn_daily_minimum_temperature_day_K - 273.15)
tmaxout = c(rhstart,clim_list$tmmx_daily_maximum_temperature_day_K - 273.15)

writeLines(text = rainout, con = "clim/RedRock.rain")
writeLines(text = tminout, con = "clim/RedRock.tmin")
writeLines(text = tmaxout, con = "clim/RedRock.tmax")



clim = read_clim("clim/RedRock")

library(tidyverse)

climplot = clim %>% group_by(year) %>% summarise(annual_precip = sum(rain)) %>% 
  ggplot() +
  geom_line(aes(x = year, y = annual_precip*1000)) +
  ylab("Annual Precip (mm)") +
  labs(title = "Annual Precip, Basestation Aggregated from Gridmet")

ggsave("plots/Precip_annual.jpeg",climplot, width = 8, height = 6)
