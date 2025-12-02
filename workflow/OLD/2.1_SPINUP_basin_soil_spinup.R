
library(RHESSysIOinR)
library(rhutils)

# Soil spinup

resetveg = F
if (resetveg) {
  world_path = "worldfiles/Ward_msr90m_patchsoilspin.world"
  world = read_world(worldfile = world_path)
  veg_vars =
    c("cs.cpool", "cs.leafc", "cs.dead_leafc", "cs.leafc_store", "cs.leafc_transfer",
      "cs.live_stemc", "cs.livestemc_store", "cs.livestemc_transfer", "cs.dead_stemc",
      "cs.deadstemc_store", "cs.deadstemc_transfer", "cs.live_crootc", "cs.livecrootc_store",
      "cs.livecrootc_transfer", "cs.dead_crootc", "cs.deadcrootc_store", "cs.deadcrootc_transfer",
      "cs.frootc", "cs.frootc_store", "cs.frootc_transfer", "cs.cwdc", 
      "epv.prev_leafcalloc", "ns.npool", "ns.leafn", "ns.dead_leafn",
      "ns.leafn_store", "ns.leafn_transfer", "ns.live_stemn", "ns.livestemn_store",
      "ns.livestemn_transfer", "ns.dead_stemn", "ns.deadstemn_store", "ns.deadstemn_transfer",
      "ns.live_crootn", "ns.livecrootn_store", "ns.livecrootn_transfer", "ns.dead_crootn",
      "ns.deadcrootn_store", "ns.deadcrootn_transfer", "ns.frootn", "ns.frootn_store",
      "ns.frootn_transfer", "ns.cwdn", "ns.retransn")
  world$values[world$vars %in% veg_vars] = "0.0"
  write_world(world, gsub(".world", "_reset.world", world_path))
}

# -------------------- Project/Run Name --------------------
name = "Redrock_basin_soilspin"
# -------------------- Input RHESSys --------------------
#clim = "clim/netcdf"
clim = "clim/RedRock"
 
# dates = read_clim(clim,dates_out = T)
# clim_repeat(clim, "clim/ward_1000y", 1000, "years")

#dates = c("1979 1 1 1", "1999 9 30 24")
dates = c("1979 1 1 1", "2379 9 30 24")

input_rhessys = IOin_rhessys_input(
  version = "../bin/rhessys7.5",
  tec_file = paste0("tecfiles/",name,".tec"),
  world_file = "worldfiles/RedRockMSR30m_treegrass_LPC_spun_nutrients.world",
  world_hdr_prefix = paste0("hdr_",name),
  world_hdr_path = "hdr_files",
  flowtable = "flowtables/RedRockMSR30m_treegrass_LPC.flow",
  start = dates[1],
  end = dates[2],
  output_folder = "output/",
  output_prefix = name,
  commandline_options = "-g -vmort_off -climrepeat"
)

# -------------------- Input Headers --------------------
input_hdr = IOin_hdr(
  basin = "defs/basin.def",
  hillslope = "defs/hill.def",
  zone = "defs/zone.def", #"defs/zone_Redrock.def"
  soil = c("defs/soil_sandyloam__Redrock_sagebrush.def", "defs/soil_loamysand__Redrock_sagebrush.def"), 
  landuse = "defs/lu.def",
  # stratum = c("defs/stratum_shrub.def", "defs/stratum_grass.def"),
  stratum = c("defs/veg_sagebrush_uplandl_hybrid_Redrock.def", "defs/veg_grass.def"), # grass 
  # stratum = c("defs/veg_p301_shrub.def", "defs/veg_grass.def"),
  basestations = paste0(clim, ".base")
)

# --------------------  Def File Parameters --------------------

# load("r_obj/cal_defpars.rdata")
# input_def_pars = IOin_def_pars_simple(input_def_pars)
input_def_pars = NULL

# -------------------- Make Tec File --------------------
input_tec_data = IOin_tec_std(start = "1979 10 1 1", end = dates[2], output_state = T)

# -------------------- Output filter --------------------
source("../R/output_aliases.R")

outfilter = build_output_filter(
  timestep = "daily",
  output_format = "csv",
  output_path = "output",
  output_filename = "basinsoilspin_basin",
  spatial_level = "basin",
  spatial_ID = 1,
  variables = c("patch.soil_cs.totalc", "patch.soil_ns.totaln", "patch.lai", "stratum.cs.totalc", "stratum.cs.leafc", "stratum.cs.live_stemc")
)
output_filter = IOin_output_filters(outfilter, file_name = "./output/filters/filter.yml")

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
beepr::beep(3)

out_dir = collect_output(basename = paste0(name,"_RunID", GetRunID(),"_"))


plotpdf_allvars(out_dir = out_dir,out_name = paste0("spinsoils_RunID_",GetRunID()), step = "yearly", pdfwidth = 10, hide_legend = F)

# sensout = pars_sens(out_dir = out_dir, input_def_pars = input_def_pars)
# pars_sens_output_tables(pars_sens_out = sensout, output_path = paste0("plots/pars_sens_RunID_",GetRunID(),".pdf" ) )




# statefile = worldstate(input_rhessys$world_file)
# # name = world_name(input_rhessys, "_soilspin")

# file.rename(statefile, )


# -------------------- Reset --------------------

world_path = name
world = read_world(worldfile = world_path)

veg_vars =
  c(
    "cs.cpool", "cs.leafc", "cs.dead_leafc", "cs.leafc_store", "cs.leafc_transfer", "cs.live_stemc", "cs.livestemc_store", "cs.livestemc_transfer", "cs.dead_stemc",
    "cs.deadstemc_store", "cs.deadstemc_transfer", "cs.live_crootc", "cs.livecrootc_store", "cs.livecrootc_transfer", "cs.dead_crootc", "cs.deadcrootc_store", 
    "cs.deadcrootc_transfer", "cs.frootc", "cs.frootc_store", "cs.frootc_transfer", "cs.cwdc","epv.prev_leafcalloc", "ns.npool", "ns.leafn", "ns.dead_leafn", "ns.leafn_store", 
    "ns.leafn_transfer", "ns.live_stemn", "ns.livestemn_store", "ns.livestemn_transfer", "ns.dead_stemn", "ns.deadstemn_store", "ns.deadstemn_transfer", 
    "ns.live_crootn", "ns.livecrootn_store", "ns.livecrootn_transfer", "ns.dead_crootn", "ns.deadcrootn_store", "ns.deadcrootn_transfer", "ns.frootn", 
    "ns.frootn_store", "ns.frootn_transfer", "ns.cwdn", "ns.retransn"
  )

world$values[world$vars %in% veg_vars] = "0.0"

write_world(world, gsub(".world", "_reset.world", world_path))




