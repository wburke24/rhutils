library(RHESSysIOinR)
library(rhutils)

# -------------------- Project/Run Name --------------------
name = "RedRock_test"
# -------------------- Input RHESSys --------------------
clim = "clim/RedRock"

#dates = read_clim(clim,dates_out = T)
dates = c("2000 1 1 1", "2010 1 1 1")

input_rhessys = IOin_rhessys_input(
  version = "../bin/rhessys7.5",
  tec_file = paste0("tecfiles/",name,".tec"),
  world_file = "worldfiles/RedRockMSR30m_tree2shrubnobare.world",
  world_hdr_prefix = paste0("hdr_",name),
  flowtable = "flowtables/RedRockMSR30m_tree2shrubnobare.flow",
  start = dates[1],
  end = dates[2],
  output_folder = "output/",
  output_prefix = name,
  commandline_options = "-g -vmort_off"
)

# -------------------- Input Headers --------------------
input_hdr = IOin_hdr(
  basin = "defs/basin.def",
  hillslope = "defs/hill.def",
  zone = "defs/zone.def",
  soil = c("defs/soil_sandyloam.def", "defs/soil_loamysand.def"),
  landuse = "defs/lu.def",
  stratum = c("defs/stratum_evergreen.def", "defs/stratum_shrub.def", 
              "defs/stratum_grass.def", "defs/stratum_nonveg.def"),
  basestations = paste0(clim, ".base")
)

# --------------------  Def File Parameters --------------------

#input_def_pars = IOin_def_pars_simple(input_def_pars)
input_def_pars = NULL

# -------------------- Make Tec File --------------------
input_tec_data = IOin_tec_std(start = "2000 10 1 1", end = dates[2], output_state = F)

# -------------------- Output filter --------------------
source("R/output_aliases.R")

outfilter = build_output_filter(
  timestep = "daily",
  output_format = "csv",
  output_path = "output",
  output_filename = paste0(name,"_basin"),
  spatial_level = "basin",
  spatial_ID = 1,
  variables = output_vars_minimal
)

output_filter = IOin_output_filters(outfilter, file_name = paste0("./output/filters/filter",name,".yml") )

# -------------------- Run --------------------

run_rhessys_single(
  input_rhessys = input_rhessys,
  hdr_files = input_hdr,
  def_pars = input_def_pars,
  tec_data = input_tec_data,
  output_filter = output_filter,
  return_cmd = F,
  write_run_metadata = F
)

out_dir = collect_output()

plotpdf_allvars(out_dir, "msr_run")

# -------------------- WATER BALANCE --------------------
# library(tidyverse)
# library(cowplot)

# DT = get_basin_daily(out_dir)
# bdwatbal = watbal_basin_of(bd = DT)

# theme_set(theme_cowplot())

# water_balance_daily = bdwatbal %>%
#   ggplot() + aes(x = date, y = watbal) + geom_line() + ggtitle("Daily Water Balance") + labs(caption = "For checking data only")

# water_balance_annual = bdwatbal %>% group_by(year) %>% summarize(watbal = sum(watbal)) %>%
#   ggplot() + aes(x = year, y = watbal) + geom_line() + ggtitle("Annual Water Balance") + labs(caption = "For checking data only")

# water_balance_annual


