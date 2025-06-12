# soil_spin
library(RHESSysIOinR)
library(rhutils)
source("R/output_aliases.R")
# library(data.table)
# source("../R/fun_spinup.R")

# only using world ID 8 in this case

# ---------- Input world ----------
# world_path = "worldfiles/RedRockMSR30m_tree2shrubnobare.world"
# flow_path = "flowtables/RedRockMSR30m_tree2shrubnobare.flow"


# -------------------- RUN FOR SOIL NUTRIENT SPINUP --------------------
# SHRUB only: 
output_world_paths = "worldfiles/soil_spin/RedRockMSR30m_tree2shrubnobare_ruleID8.world"
output_world_paths = "worldfiles/soil_spin/RedRockMSR30m_tree2shrubnobare_ruleID8_1kyr.world"
output_flow_paths = "flowtables/soil_spin/RedRockMSR30m_tree2shrubnobare_ruleID8.flow"
ID = "8"

# HERB only: 
# output_world_paths = "worldfiles/soil_spin/RedRockMSR30m_tree2shrubnobare_ruleID4.world"
# output_flow_paths = "flowtables/soil_spin/RedRockMSR30m_tree2shrubnobare_ruleID4.flow"
# ID = "4"

# IDs = c("1", "2", "3", "4", "5", "7", "6", "8")

# tmp = compare_params("defs/veg_sagebrush_riparian_243.def","defs/veg_sagebrush_uplandl_hybrid_9.def", "~/Repos/RHESSys-develop/rhessys/init/construct_stratum_defaults.c")
# tmp[tmp$different_pars,]

# -------------------- Project/Run Name --------------------
name = paste0("SoilSpin_SENS_ID",ID) # ,"-", gsub("defs/","", input_hdr$stratum_def)

# -------------------- climate --------------------
clim = "clim/RedRock"
# clim = "clim/Redrock150pctpcp"

altclim = F
if (altclim){
  clim 1.5X
  climdf = read_clim(clim)
  pcp = climdf$rain
  pcp = pcp*1.5
  file.copy("clim/RedRock.rain",to = "clim/Redrock150pctpcp.rain")
  file.copy("clim/RedRock.tmax",to = "clim/Redrock150pctpcp.tmax")
  file.copy("clim/RedRock.tmin",to = "clim/Redrock150pctpcp.tmin")
  file.copy("clim/RedRock.base",to = "clim/Redrock150pctpcp.base")

  datehead = readLines("clim/RedRock.rain",n=1)
  write(datehead, "clim/Redrock150pctpcp.rain")
  data.table::fwrite(list(pcp), "clim/Redrock150pctpcp.rain", append = T)
}


# -------------------- Input RHESSys --------------------
#dates = c("1979 1 1 1", "2020 9 30 24")
dates = c("1979 1 1 1", "2179 9 30 24")

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
  # stratum = c("defs/stratum_shrub.def", "defs/stratum_grass.def"),
  # stratum = c("defs/veg_p301_shrub.def", "defs/stratum_grass.def"), # grass 
  # stratum = c("defs/stratum_shrub.def"),
  # stratum = c("defs/veg_p301_shrub.def"),
  stratum = c("defs/veg_sagebrush_riparian_243.def"),
  # stratum = c("defs/veg_sagebrush_uplandl_hybrid_9.def"),
  # stratum = c("defs/veg_mtnmahogany.def"),
  basestations = paste0(clim, ".base")
)

# --------------------  Def File Parameters --------------------
source("~/Projects/defs/param_lists.R")

getpars = F
if (getpars) {
  defaults = "~/Repos/RHESSys-develop/rhessys/init/construct_stratum_defaults.c"
  default_pars = parse_rh_constr_func(defaults)
  # default_pars$Name

  # --------------------  CHECK DEF PARS --------------------
  # get_def_par(input_hdr$stratum_def[1])
  # read_def(input_hdr$stratum_def[1])
  parcheck = check_params(defaults, input_hdr$stratum_def[1])
  parcheck[grepl("turnover",parcheck$Name),]
  parcheck[grepl("vpd",parcheck$Name),]
  parcheck[grepl("root",parcheck$Name),]
}

# ------------------- sagebrush -------------------
# stomatal regulation
pl1 = list(
  list(Def_file = input_hdr$stratum_def[1], Variable = "epc.vpd_open", Value = c(0,100)), # 0.000000
  list(Def_file = input_hdr$stratum_def[1], Variable = "epc.vpd_close", Value = c(2500, 3500)) # 3000
)
# turnovers
pl2 = list(
  list(Def_file = input_hdr$stratum_def[1], Variable = "epc.root_distrib_parm", Value = c(15,150)), # "4.000000"
  list(Def_file = input_hdr$stratum_def[1], Variable = "epc.root_growth_direction", Value = c(0.6,1.1) ), #"1.1"
  list(Def_file = input_hdr$stratum_def[1], Variable = "epc.froot_turnover", Value = c(0.1, 0.27) ), #"0.270000"
  list(Def_file = input_hdr$stratum_def[1], Variable = "epc.leaf_turnover", Value = c(0.1, 0.3) ), #"0.400000"
  list(Def_file = input_hdr$stratum_def[1], Variable = "epc.livewood_turnover", Value = c(0.05, 0.3) ), #"0.100000" 
  list(Def_file = input_hdr$stratum_def[1], Variable = "epc.branch_turnover", Value = c(0.005,0.02) ) #"0.0200"
)

pl_soils = list(
  list(input_hdr$soil_def[1], "m", c(0.15,0.4)),
  list(input_hdr$soil_def[2], "m", c(0.15,0.4))
)

# all combintaions
# pars_list = def_par_allcomb(c(pl1,pl2))

# random sample
pars_list = lapply(c(pl1,pl2,pl_soils), runif_sample, 50)


# -------------------- Make par list --------------------
# param list: p_all
parameters = p_all[!p_all %in% sapply(pars_list, "[[",2)]

# shrub
sh = make_par_list_from_def_file(input_hdr$stratum_def[1], parameters, defaults)

input_def_pars = IOin_def_pars_simple(sh, n = 25, pct_range = 0.95, rm_dup = T)

input_def_pars = c(input_def_pars, pars_list)

# pars_list = def_par_allcomb(pars_list)
input_def_pars = IOin_def_pars_simple(pars_list)
# input_def_pars = NULL

# -------------------- Make Tec File --------------------
input_tec_data = IOin_tec_std(start = "1979 10 1 1", end = dates[2], output_state = T)

# -------------------- Output filter --------------------
# outvars = c("patch.soil_cs.totalc", "patch.soil_ns.totaln", "stratum.cs.net_psn", "patch.lai", "patch.totalc","patch.evaporation", "patch.streamflow", "patch.snowpack.water_equivalent_depth", "hill.base_flow","patch.rz_storage","patch.unsat_storage", "patch.rootzone.depth", "patch.gw_drainage", "stratum.cs.totalc", "stratum.cs.leafc", "stratum.cs.cpool", "stratum.cs.live_stemc", "stratum.cs.dead_stemc", "stratum.cs.live_crootc", "stratum.cs.dead_crootc", "stratum.cs.frootc", "patch.litter_cs.totalc", "patch.litter_cs.totalc","patch.litter_cs.litr1c_bg","patch.litter_cs.litr2c_bg","patch.litter_cs.litr3c_bg","patch.litter_cs.litr4c_bg")

# short ver
# outvars = c("patch.soil_cs.totalc", "patch.soil_ns.totaln", "patch.lai", "patch.totalc","patch.evaporation", "patch.rz_storage","patch.unsat_storage", "patch.rootzone.depth", "stratum.cs.totalc", "stratum.cs.leafc", "stratum.cs.cpool", "stratum.cs.live_stemc", "stratum.cs.dead_stemc", "stratum.cs.live_crootc", "stratum.cs.dead_crootc", "stratum.cs.frootc", "patch.litter_cs.totalc", "patch.litter_cs.totalc")

outvars = c("patch.soil_cs.totalc", "patch.soil_ns.totaln", "patch.lai", "patch.rootzone.depth", "stratum.cs.totalc", "stratum.cs.leafc", "stratum.cs.cpool", "stratum.cs.live_stemc", "patch.evaporation","patch.evaporation_surf","patch.transpiration_unsat_zone","patch.transpiration_sat_zone")

outfilter = build_output_filter(
  timestep = "daily",
  output_format = "csv",
  output_path = "output",
  output_filename = paste0(name,"_basin"),
  spatial_level = "basin",
  spatial_ID = 1,
  variables = outvars
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
beepr::beep(3)
out_dir = collect_output()
plotpdf_allvars(out_dir, out_name = paste0("spinsoils_ID8_SENS_",ExtractRunID(out_dir)), step = "yearly", pdfwidth = 10, hide_legend = T)

ExtractRunID = function(out_dir) {
  allfiles = list.files(out_dir)
  allrun = stringr::str_extract(allfiles,"RunID\\d+")
  run = unique(allrun[!is.na(allrun)])
  if (length(run) > 1) {
    stop(paste0("More than 1 run ID found:",run))
  }
  return(run)
}

ExtractRunID(out_dir)



# run_rhessys_single(
#   input_rhessys = input_rhessys,
#   hdr_files = input_hdr,
#   def_pars = input_def_pars,
#   tec_data = input_tec_data,
#   output_filter = output_filter,
#   return_cmd = F,
#   write_log = T
# )




defopts = c("defs/veg_p301_shrub.def","defs/stratum_shrub.def", "defs/veg_sagebrush_riparian_243.def","defs/veg_sagebrush_uplandl_hybrid_9.def","defs/veg_mtnmahogany.def")

for (i in seq_along(defopts)) {
  input_hdr$stratum_def = defopts[i]
  output_filter$filter$output$filename = paste0(gsub("defs/","",defopts[i]),"_pcp150pct_basin" )

  run_rhessys_single(
    input_rhessys = input_rhessys,
    hdr_files = input_hdr,
    def_pars = input_def_pars,
    tec_data = input_tec_data,
    output_filter = output_filter,
    return_cmd = F,
    write_log = T
  )
}

beepr::beep(3)
out_dir = collect_output()
plotpdf_allvars(out_dir, "spinsoils_ID8_SENS", step = "yearly", pdfwidth = 10, hide_legend = F)


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
