# soil_spin
library(RHESSysIOinR)
library(rhutils)
# library(data.table)
# source("../R/fun_spinup.R")

# only using world ID 8 in this case

# ---------- Input world ----------
# world_path = "worldfiles/RedRockMSR30m_tree2shrubnobare.world"
# flow_path = "flowtables/RedRockMSR30m_tree2shrubnobare.flow"


# -------------------- RUN FOR SOIL NUTRIENT SPINUP --------------------
output_world_paths = "worldfiles/soil_spin/RedRockMSR30m_tree2shrubnobare_ruleID8.world"
output_flow_paths = "flowtables/soil_spin/RedRockMSR30m_tree2shrubnobare_ruleID8.flow"
ID = "8"
# IDs = c("1", "2", "3", "4", "5", "7", "6", "8")


# -------------------- Project/Run Name --------------------
name = paste0("SoilSpin_SENS_ID",ID)
# -------------------- Input RHESSys --------------------
clim = "clim/RedRock"

#dates = c("1979 1 1 1", "2020 9 30 24")
dates = c("1979 1 1 1", "2479 9 30 24")

input_rhessys = IOin_rhessys_input(
  version = "../bin/rhessys7.5",
  tec_file = paste0("tecfiles/",name,".tec"),
  world_file = output_world_paths,
  world_hdr_prefix = paste0("hdr_",name),
  world_hdr_path = "hdr_files",
  flowtable = output_flow_paths,
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
  zone = "defs/zone.def",
  soil = c("defs/soil_sandyloam.def", "defs/soil_loamysand.def"),
  landuse = "defs/lu.def",
  stratum = c("defs/stratum_shrub.def", "defs/stratum_grass.def"),
  # stratum = c("defs/veg_p301_shrub.def", "defs/stratum_grass.def"), # grass 
  # stratum = c("defs/veg_p301_shrub.def", "defs/veg_grass.def"),
  basestations = paste0(clim, ".base")
)

# --------------------  Def File Parameters --------------------
parameters = c("epc.root_distrib_parm", "epc.min_percent_leafg", "epc.branch_turnover", "epc.froot_turnover", "epc.leaf_turnover", "epc.livewood_turnover", "epc.gl_c", "epc.gl_smax", "epc.proj_sla")
defaults = "~/Repos/RHESSys-develop/rhessys/init/construct_stratum_defaults.c"

# get_def_par(input_hdr$stratum_def[1])
# read_def(input_hdr$stratum_def[1])

# shrub
sh = make_par_list_from_def_file(input_hdr$stratum_def[1], parameters, defaults)

s1 = list(list(input_hdr$soil_def[1], "m", "0.25"))
s2 = list(list(input_hdr$soil_def[2], "m", "0.25"))

pars_list = c(sh,s1,s2)

input_def_pars = IOin_def_pars_simple(pars_list, n = 50, pct_range = 1)

# pars_list = list(
  # list(input_hdr$stratum_def[1], "epc.root_distrib_parm", c(5, 19)),
  # list(input_hdr$stratum_def[1], "epc.root_growth_direction", 0.7),
  # list(input_hdr$stratum_def[1], "epc.height_to_stem_coef", c(1, 4.5)),
  # list(input_hdr$stratum_def[1], "epc.gl_c", c(0.0006, 0.00006)),
  # list(input_hdr$stratum_def[1], "epc.gl_smax", 0.006),
  # list(input_hdr$stratum_def[1], "epc.froot_turnover", 0.27),
  # list(input_hdr$stratum_def[1], "epc.leaf_turnover", c(0.3, 0.75)), # leaf turnover is high, trying options
  # list(input_hdr$stratum_def[1], "epc.proj_sla", 9) # i think should be at least 9, depends on seasonality/how evergreen the shrubs are
# )
# pars_list = def_par_allcomb(pars_list)
# input_def_pars = IOin_def_pars_simple(pars_list)
# input_def_pars = NULL

# -------------------- Make Tec File --------------------
input_tec_data = IOin_tec_std(start = "1979 10 1 1", end = dates[2], output_state = F)

# -------------------- Output filter --------------------
source("R/output_aliases.R")

outfilter = build_output_filter(
  timestep = "daily",
  output_format = "csv",
  output_path = "output",
  output_filename = paste0("soilspin_SENS_basin_ID",ID),
  spatial_level = "basin",
  spatial_ID = 1,
  variables = c("patch.soil_cs.totalc", "patch.soil_ns.totaln", "stratum.cs.net_psn", "patch.lai", "patch.totalc","patch.evaporation", "patch.streamflow", "patch.snowpack.water_equivalent_depth", "hill.base_flow","patch.rz_storage","patch.unsat_storage", "patch.rootzone.depth", "patch.gw_drainage", "stratum.cs.totalc", "stratum.cs.leafc", "stratum.cs.cpool", "stratum.cs.live_stemc", "stratum.cs.dead_stemc", "stratum.cs.live_crootc", "stratum.cs.dead_crootc", "stratum.cs.frootc", "patch.litter_cs.totalc", "patch.litter_cs.totalc","patch.litter_cs.litr1c_bg","patch.litter_cs.litr2c_bg","patch.litter_cs.litr3c_bg","patch.litter_cs.litr4c_bg")
)
output_filter = IOin_output_filters(outfilter, file_name = paste0("./output/filters/filter_ID",ID,".yml"))

# -------------------- HANDLE RUN IDS --------------------
runid = GetRunID(increment = T)

save(input_rhessys, input_hdr, input_def_pars, input_tec_data, output_filter, file = paste0("robj/", name,"_RunID",runid, ".RData"))
# change output files to have an ID?
output_filter = AddRunIDtoOutputFilters(output_filter,runid)

# -------------------- RUN --------------------
run_rhessys_multi(
  input_rhessys = input_rhessys,
  hdr_files = input_hdr,
  def_pars = input_def_pars,
  tec_data = input_tec_data,
  output_filter = output_filter,
  return_cmd = F,
  write_log = T
)

out_dir = collect_output()
plotpdf_allvars(out_dir, "spinsoils_ID8_SENS", step = "yearly", pdfwidth = 10)


# -------------------- MANUAL PARALLEL RUNS --------------------
manualpara = F
if (manualpara) {
  library(parallel)
  n_cores = parallel::detectCores() - 1
  start = Sys.time()
  cl = parallel::makeCluster(n_cores)
  parallel::clusterExport(cl = cl, varlist = c("run_cmds"), envir = environment())
  parallel::parLapply(cl = cl, X = seq_along(run_cmds), fun = function(X, run_cmds) { system(run_cmds[[X]])}, run_cmds = run_cmds)
  # stop the cluster
  parallel::stopCluster(cl)
  end = Sys.time()
  end - start
  out_dir = collect_output()
  plotpdf_allvars(out_dir, "spinsoils_ID8_SENS")
}
