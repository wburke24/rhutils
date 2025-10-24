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
# output_world_paths = "worldfiles/soil_spin/RedRockMSR30m_tree2shrubnobare_ruleID8.world"
# output_world_paths = "worldfiles/soil_spin/RedRockMSR30m_tree2shrubnobare_ruleID8_1kyr.world"
# output_world_paths = "worldfiles/soil_spin/RedRockMSR30m_tree2shrubnobare_ruleID8_1700yr.world"
# output_world_paths = "worldfiles/soil_spin/RedRockMSR30m_tree2shrubnobare_ruleID8_1700yr_fullcover.world"
# output_world_paths = "worldfiles/soil_spin/RedRockMSR30m_tree2shrubnobare_ruleID8_1700yr_35cover.world"
# output_flow_paths = "flowtables/soil_spin/RedRockMSR30m_tree2shrubnobare_ruleID8.flow"
# ID = "8"

output_world_paths = "worldfiles/soil_spin/RedRockMSR30m_treegrass_LPC_ruleID4.world"
output_flow_paths = "flowtables/soil_spin/RedRockMSR30m_treegrass_LPC_ruleID4.flow"

ID = "4"

# HERB only: 
# output_world_paths = "worldfiles/soil_spin/RedRockMSR30m_tree2shrubnobare_ruleID4.world"
# output_flow_paths = "flowtables/soil_spin/RedRockMSR30m_tree2shrubnobare_ruleID4.flow"
# ID = "4"

# IDs = c("1", "2", "3", "4", "5", "7", "6", "8")

# tmp = compare_params("defs/veg_sagebrush_riparian_243.def","defs/veg_sagebrush_uplandl_hybrid_9.def", "~/Repos/RHESSys-develop/rhessys/init/construct_stratum_defaults.c")
# tmp[tmp$different_pars,]

# -------------------- Project/Run Name --------------------
name = paste0("SoilSpin_SENS_ID",ID) # ,"-", gsub("defs/","", input_hdr$stratum_def)
# name = paste0("SoilSpin_SENS_ID",ID,"35coverdickenson")

# -------------------- climate --------------------
clim = "clim/RedRock"
# clim = "clim/Redrock150pctpcp"

altclim = F
if (altclim) {
  # clim 1.5
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
dates = c("1979 1 1 1", "2379 9 30 24")
# dates = c("1979 1 1 1", "2700 9 30 24")

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
  # soil = c("defs/soil_sandyloam.def", "defs/soil_loamysand.def"),
  soil = c("defs/soil_sandyloam__Redrock_sagebrush.def", "defs/soil_loamysand__Redrock_sagebrush.def"), 
  landuse = "defs/lu.def",
  # stratum = c("defs/stratum_grass.def"),
  stratum = c("defs/veg_grass.def"), # for now using this
  # stratum = c("defs/veg_p301_shrub.def", "defs/stratum_grass.def"), # grass 
  # stratum = c("defs/stratum_shrub.def"),
  # stratum = c("defs/veg_p301_shrub.def"),
  # stratum = c("defs/veg_sagebrush_riparian_243.def"),
  # stratum = c("defs/veg_sagebrush_uplandl_hybrid_9.def"),
  # stratum = c("defs/veg_mtnmahogany.def"),
  basestations = paste0(clim, ".base")
)

# write_fire_grids("fire_test",template = "preprocessing/template/RedRockMSR1strata_coverfrac.template", map_dir = "preprocessing/whitebox/")

# --------------------  Def File Parameters --------------------
source("~/Projects/defs/param_lists.R")
# ========== NUMBER OF RUNS ==========
n = 100

getpars = F
if (getpars) {
  
  # --------------------  CHECK VEGETATION PARS -------------------
  defaults = "~/Repos/RHESSys-develop/rhessys/init/construct_stratum_defaults.c"
  default_pars = parse_rh_constr_func(defaults)
  parcheck = check_params(defaults, input_hdr$stratum_def[1])
  parcheck[grepl("turnover",parcheck$Name),]
  parcheck[grepl("vpd",parcheck$Name),]
  parcheck[grepl("root",parcheck$Name),]
  parcheck[grepl("sla",parcheck$Name),]
  parcheck[grepl("cpool",parcheck$Name),]

  # --------------------  CHECK SOIL PARS -------------------
  defaultssoil = "~/Repos/RHESSys-develop/rhessys/init/construct_soil_defaults.c"
  default_pars_soil = parse_rh_constr_func(defaultssoil)
  soil_pars = c("m", "m_v", "Ksat_0", "Ksat_0_v", "psi_air_entry", "pore_size_index", "porosity_0","sat_to_gw_coeff")
  parchecksoils = check_params(defaultssoil, input_hdr$soil_def[1])

  parchecksoils[parchecksoils$Name %in% soil_pars,]

  parchecksoils2 = check_params(defaultssoil, input_hdr$soil_def[2])
  parchecksoils2[grepl("depth",parchecksoils2$Name),]
 
}

getparsfromid = F
if (getparsfromid) {
  load("robj/SoilSpin_SENS_ID8_RunID65.RData")
  input_def_pars = defpar_extract_byrunnum(pars_list = input_def_pars, runnum = c(424))
  input_def_pars = lapply(input_def_pars, function(X){if(all(is.numeric(X[[3]]))){ X[[3]] = signif(X[[3]],4)}; return(X) })

  write_updated_def_files(input_def_pars, input_hdr,"Redrock")
  # input_def_pars = lapply(pars_list, runif_sample, n)

}

# ------------------- soil pars -------------------
pars_list = list(
  # soils
  list(input_hdr$soil_def[1], Variable = "soil_depth", Value = c(1,3)), 
  list(input_hdr$soil_def[2], Variable = "soil_depth", Value = c(1,3)), 

  list(input_hdr$soil_def[1], Variable = "m", Value = c(0.01, 0.3)), #0.1953 default is 0.12
  list(input_hdr$soil_def[2], Variable = "m", Value = c(0.01, 0.3)), #0.2485
  list(input_hdr$soil_def[1], Variable = "m_v", Value = c(0.05, 0.3)), #
  list(input_hdr$soil_def[2], Variable = "m_v", Value = c(0.05, 0.3)), #

  list(input_hdr$soil_def[1], Variable = "Ksat_0", Value = c(3, 500)), #
  list(input_hdr$soil_def[2], Variable = "Ksat_0", Value = c(3, 500)), #
  list(input_hdr$soil_def[1], Variable = "Ksat_0_v", Value = c(3, 500)), #
  list(input_hdr$soil_def[2], Variable = "Ksat_0_v", Value = c(3, 500)), #
  
  list(input_hdr$soil_def[1], Variable = "pore_size_index", Value = c(0.1, 0.3)), # 0.204
  list(input_hdr$soil_def[2], Variable = "pore_size_index", Value = c(0.1, 0.3)), #
  list(input_hdr$soil_def[1], Variable = "porosity_0", Value = c(0.3, 0.55)), # 0.435
  list(input_hdr$soil_def[2], Variable = "porosity_0", Value = c(0.3, 0.55)), #
  list(input_hdr$soil_def[1], Variable = "psi_air_entry", Value = c(0.1, 0.3)), #0.218
  list(input_hdr$soil_def[2], Variable = "psi_air_entry", Value = c(0.1, 0.3)), #
  
  list(input_hdr$soil_def[1], Variable = "sat_to_gw_coeff", Value = c(0)), #1
  list(input_hdr$soil_def[2], Variable = "sat_to_gw_coeff", Value = c(0)) #
)
soil_pars = lapply(pars_list, runif_sample, n)
# soil_pars = NULL
# -------------------- Get list of all veg pars to vary --------------------
# param list: p_all

parameters = p_all[!p_all %in% sapply(pars_list, "[[",2)]
parameters = p_all
# -------------------- Get baseline veg pars from def file --------------------
pars_list_baseline = make_par_list_from_def_file(input_hdr$stratum_def[1], parameters, 
  defaults = "~/Repos/RHESSys-develop/rhessys/init/construct_stratum_defaults.c")

veg_pars = IOin_def_pars_simple(pars_list_baseline, n = n, pct_range = 0.25, rm_dup = T)

veg_pars = NULL

# -------------------- Make par list --------------------
# pars_list = def_par_allcomb(pars_list)
# input_def_pars = IOin_def_pars_simple(pars_list)
# input_def_pars = NULL

# -------------------- *** CHANGE DEF PAR HDR, KEEPS PARS --------------------
# olddef = "defs/veg_sagebrush_riparian_243.def"
# newdef =  input_hdr$stratum_def
# input_def_pars = lapply(input_def_pars, function(X){if (X$Def_file == olddef) {X$Def_file = newdef}; return(X)})



# ------------------- GRASS PARS -------------------
# input_def_pars = lapply(pars_list, runif_sample, n)

# input_def_pars[[which(sapply(input_def_pars,"[[",2) == "epc.allocation_flag")]]$Value = rep("DICKENSON", n) # COMBINED WARING DICKENSON
# input_def_pars = IOin_def_pars_simple(input_def_pars,rm_dup = T)

# ------------------- COMBINE PARS -------------------
input_def_pars = c(soil_pars, veg_pars)

# input_def_pars = NULL

# -------------------- Make Tec File --------------------
input_tec_data = IOin_tec_std(start = "1979 10 1 1", end = dates[2], output_state = F)

# -------------------- Output filter --------------------
# outvars = c("patch.soil_cs.totalc", "patch.soil_ns.totaln", "stratum.cs.net_psn", "patch.lai", "patch.totalc","patch.evaporation", "patch.streamflow", "patch.snowpack.water_equivalent_depth", "hill.base_flow","patch.rz_storage","patch.unsat_storage", "patch.rootzone.depth", "patch.gw_drainage", "stratum.cs.totalc", "stratum.cs.leafc", "stratum.cs.cpool", "stratum.cs.live_stemc", "stratum.cs.dead_stemc", "stratum.cs.live_crootc", "stratum.cs.dead_crootc", "stratum.cs.frootc", "patch.litter_cs.totalc", "patch.litter_cs.totalc","patch.litter_cs.litr1c_bg","patch.litter_cs.litr2c_bg","patch.litter_cs.litr3c_bg","patch.litter_cs.litr4c_bg")

# short ver
# outvars = c("patch.soil_cs.totalc", "patch.soil_ns.totaln", "patch.lai", "patch.totalc","patch.evaporation", "patch.rz_storage","patch.unsat_storage", "patch.rootzone.depth", "stratum.cs.totalc", "stratum.cs.leafc", "stratum.cs.cpool", "stratum.cs.live_stemc", "stratum.cs.dead_stemc", "stratum.cs.live_crootc", "stratum.cs.dead_crootc", "stratum.cs.frootc", "patch.litter_cs.totalc", "patch.litter_cs.totalc")

outvars = c("patch.soil_cs.totalc", "patch.soil_ns.totaln", "patch.lai", "patch.rootzone.depth", "patch.rz_storage","patch.unsat_storage", "stratum.cs.totalc", "stratum.cs.leafc", "stratum.cs.cpool", "stratum.cs.live_stemc", "stratum.cs.dead_stemc", "patch.evaporation","patch.evaporation_surf","patch.transpiration_unsat_zone","patch.transpiration_sat_zone","stratum.epv.height")

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
output_filter = AddRunIDtoOutputFilters(output_filter,runid)
save(input_rhessys, input_hdr, input_def_pars, input_tec_data, output_filter, file = paste0("robj/", name,"_RunID",runid, ".RData"))

write_param_table(input_def_pars)

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

out_dir = collect_output(basename = paste0(name,"_RunID", GetRunID(),"_"))
plotpdf_allvars(out_dir = out_dir,out_name = paste0("spinsoils_pfamID",ID,"_RunID_",GetRunID()), step = "yearly", pdfwidth = 10, hide_legend = T)
sensout = pars_sens(out_dir = out_dir, input_def_pars = input_def_pars)
pars_sens_output_tables(pars_sens_out = sensout, output_path = paste0("plots/pars_sens_RunID_",GetRunID(),".pdf" ), pdfheight = 18)

# load("robj/SoilSpin_SENS_ID8_RunID62.RData")
# out_dir = "output/SoilSpin_SENS_ID8_RunID62_2025-07-08--18-23-53/"

run_rhessys_single(
  input_rhessys = input_rhessys,
  hdr_files = input_hdr,
  def_pars = input_def_pars,
  tec_data = input_tec_data,
  output_filter = output_filter,
  return_cmd = F,
  write_log = T
)
beepr::beep(3)

# -------------------- run over def par files --------------------
defparruns = F
if (defparruns) {
  # defopts = c("defs/veg_p301_shrub.def","defs/stratum_shrub.def", "defs/veg_sagebrush_riparian_243.def","defs/veg_sagebrush_uplandl_hybrid_9.def","defs/veg_mtnmahogany.def")

  # for (i in seq_along(defopts)) {
  #   input_hdr$stratum_def = defopts[i]
  #   output_filter$filter$output$filename = paste0(gsub("defs/","",defopts[i]),"_pcp150pct_basin" )

  #   run_rhessys_single(
  #     input_rhessys = input_rhessys,
  #     hdr_files = input_hdr,
  #     def_pars = input_def_pars,
  #     tec_data = input_tec_data,
  #     output_filter = output_filter,
  #     return_cmd = F,
  #     write_log = T
  #   )
  # }

  # beepr::beep(3)
  # out_dir = collect_output()
  # plotpdf_allvars(out_dir, "spinsoils_ID8_SENS", step = "yearly", pdfwidth = 10, hide_legend = F)

}

# -------------------- troubleshoot plots par sens --------------------
plotpars_troubleshoot = F
if (plotpars_troubleshoot) {
  # plotpdf_allvars(out_dir = out_dir,out_name = paste0("spinsoils_pfamID8_RunID_",GetRunID()), step = "yearly", pdfwidth = 10 )

  # out_dir = out_dir
  # out_name = paste0("spinsoils_pfamID8_RunID_",GetRunID())
  # step = "yearly"
  # pdfwidth = 10
  # pdfheight = 7
  # hide_legend = T
  # summary_plots = T 

  # load("robj/SoilSpin_SENS_ID8_RunID62.RData")
  # out_dir = "output/SoilSpin_SENS_ID8_RunID62_2025-07-08--18-23-53/"

  # load("robj/SoilSpin_SENS_ID835coverdickenson_RunID53.RData")
  # out_dir = "output/SoilSpin_SENS_ID835covercombined_RunID53_2025-07-02--21-28-08/"


  # sensout = pars_sens(out_dir = out_dir, input_def_pars = input_def_pars)
  # pars_sens_output_tables(pars_sens_out = sensout)
}

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
