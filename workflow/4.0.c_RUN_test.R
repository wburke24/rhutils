library(RHESSysIOinR)
library(rhutils)

# -------------------- Project/Run Name --------------------
name = "RedRock_test"
# -------------------- Input RHESSys --------------------
clim = "clim/RedRock"

#dates = read_clim(clim,dates_out = T)
# dates = c("2000 01 1 1", "2000 03 1 1")
# dates = c("1979 1 1 1", "2024 09 29 24")
dates = c("1979 9 1 1", "1982 09 29 24")

input_rhessys = IOin_rhessys_input(
  version = "bin/rhessys7.5",
  tec_file = paste0("tecfiles/",name,".tec"),
  world_file = "worldfiles/RedRockMSR30m_treegrass_LPC_v2_spun_grown_basedefs_35addspin.world",
  world_hdr_prefix = paste0("hdr_",name),
  world_hdr_path = "hdr_files",
  flowtable = "flowtables/RedRockMSR30m_treegrass_LPC_v2.flow",
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
  zone = "defs/zone.def", #"defs/zone_Redrock.def"
  soil = c("defs/soil_sandyloam__Redrock_sagebrush.def", "defs/soil_loamysand__Redrock_sagebrush.def"),
  landuse = "defs/lu.def",
  # stratum = c("defs/veg_sagebrush_uplandl_hybrid_Redrock.def", "defs/goodcal_cheat_hybrid.def"),
  stratum = c("defs/veg_sagebrush_uplandl_hybrid_9.def", "defs/goodcal_cheat_hybrid.def"),
  basestations = paste0(clim, ".base")
)

# --------------------  Def File Parameters --------------------
#input_def_pars = IOin_def_pars_simple(input_def_pars)
# input_def_pars = NULL
load("robj/Redrock_ETcal_select_def_pars.RData")
input_def_pars = rhutils::defpar_rm_dup(select_def_pars)

# -------------------- Make Tec File --------------------
input_tec_data = IOin_tec_std(start = "1979 10 1 1", end = dates[2], output_state = F)

# -------------------- Output filter --------------------
source("~/Repos/rhutils/workflow/output_aliases.R")

outfilter = build_output_filter(
  timestep = "daily",
  output_format = "csv",
  output_path = "output",
  output_filename = paste0(name,"_basin"),
  spatial_level = "basin",
  spatial_ID = 1,
  variables = output_vars_set1
)
outfilter2 = build_output_filter(
  timestep = "daily",
  output_format = "csv",
  output_path = "output",
  output_filename = paste0(name,"_stratum"),
  spatial_level = "stratum",
  spatial_ID = 1,
  variables = c("stratum.cs.totalc", "stratum.cs.live_stemc", "stratum.cs.dead_stemc", "stratum.cs.live_crootc", "stratum.cs.dead_crootc", "stratum.cs.leafc")
)

output_filter = IOin_output_filters(outfilter, outfilter2, file_name = paste0("./output/filters/filter",name,".yml") )

# -------------------- HANDLE RUN IDS --------------------
runid = GetRunID(increment = T)
output_filter = AddRunIDtoOutputFilters(output_filter,runid)
save(input_rhessys, input_hdr, input_def_pars, input_tec_data, output_filter, file = paste0("robj/", name,"_RunID",runid, ".RData"))

# -------------------- Run --------------------
run_rhessys_single(
  input_rhessys = input_rhessys,
  hdr_files = input_hdr,
  def_pars = input_def_pars,
  tec_data = input_tec_data,
  output_filter = output_filter,
  return_cmd = F
)
beepr::beep(2)

out_dir = collect_output(basename = paste0(name,"_RunID", GetRunID(),"_"))

plotpdf_allvars(out_dir = out_dir,out_name = paste0("grow_veg_RunID_",GetRunID()), step = "monthly", pdfwidth = 10, hide_legend = F)


# out_dir = collect_output()

# plotpdf_allvars(out_dir, "msr_run")

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


