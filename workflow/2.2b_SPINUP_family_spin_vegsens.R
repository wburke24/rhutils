# soil_spin
library(RHESSysIOinR)
library(rhutils)
library(data.table)
library(stringr)
source("../R/output_aliases.R")
# source("../R/fun_spinup.R")

# MODE OF RULES RASTER IS 6 - MOST COMMON RULE ID
ID = 6

# -------------------- INPUTS --------------------
output_world_path = "worldfiles/soil_spin/Pitman_v2_msr_nc_90m_init_ruleID6.world"
output_flow_path = "flowtables/soil_spin/Pitman_v2_msr_nc_90m_init_ruleID6.flow"
# =====
# run_cmds = list()
# -------------------- Project/Run Name --------------------
name = paste0("SoilSpin_pfamRULEID",ID)
# -------------------- Input RHESSys --------------------
clim = "clim/pitman"

dates = c("1979 1 1 1", "2279 9 30 24")

input_rhessys = IOin_rhessys_input(
  version = "../bin/rhessys7.5",
  tec_file = paste0("tecfiles/",name,".tec"),
  world_file = output_world_path,
  world_hdr_prefix = paste0("hdr_",name),
  world_hdr_path = "hdr_files",
  flowtable = output_flow_path,
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
  soil = c("defs/soil_sandyloam.def", "defs/soil_loam.def"),
  landuse = "defs/lu.def",
  stratum = c("defs/stratum_evergreen.def", "defs/stratum_shrub.def", 
              "defs/stratum_grass.def", "defs/stratum_nonveg.def"),
  basestations = paste0(clim, ".base")
)

# --------------------  Def File Parameters MULTIPLIERS FROM FILE --------------------

# # param from def file
# get_def_par = function(def_file, parameter = NULL) {
#   tmp = read_def(def_file)
#   if (is.null(parameter)) {
#     cat("No parameter indicated, parameters in def file:\n")
#     print(tmp$names)
#     return(tmp$names)
#   }
#   if (sum(tmp$names == parameter) == 0) {
#     cat("No parameter in file matching '",parameter,"'. Parameters in def file:\n")
#     print(tmp$names)
#     return(NULL)
#   }
#   parval = tmp[tmp$names == parameter,"pars"]
#   return(parval)
# }
# # parval = get_def_par(def_file, parameters[4])
# # list of def file, param, values, from current def file
# make_par_list_from_def_file = function(def_file, parameters, defaults = NULL) {
#   # defaults = "~/Repos/RHESSys-develop/rhessys/init/construct_stratum_defaults.c"
#   def = read_def(def_file)

#   if (!all(parameters %in% def$names)) {
#     cat("Not all parameters found in def file, using defaults if supplied.\n")
#     parameters_in_def = parameters[parameters %in% def$names]
#     par_vals = mapply(get_def_par, def_file, parameters_in_def, USE.NAMES = F)
#     param_df = data.frame(Variable = parameters_in_def, Value = par_vals)

#     if (!is.null(defaults)) {
#       missing_pars = parameters[!parameters %in% def$names]
#       allpars = check_params(rh_file = defaults, def_file = def_file)
#       if (!all(missing_pars %in% allpars$Name)) {
#         cat("Not all missing parameters found in defaults - likely invalid.\n")
#         missing_pars = missing_pars[missing_pars %in% allpars$Name]
#       }
#       tmp = allpars[allpars$Name %in% missing_pars, ]
#       missing_par_df = data.frame(Variable = tmp$Name, Value = tmp$DefaultValue)
#       param_df = rbind(param_df, missing_par_df)
#     } else {
#       cat("No defaults supplied, using only parameters found in def_file.\n")
#       # parameters = parameters[parameters %in% def$names]
#     }
#   } else {
#     par_vals = mapply(get_def_par, def_file, parameters, USE.NAMES = F)
#     param_df = data.frame(Variable = parameters, Value = par_vals)
#   }
#   # make into a list for input into ioinR
#   param_list = apply(param_df, 1, FUN = function(X,Y){list(Def_file = Y, Variable = unname(X[1]), Value = unname(X[2]))}, def_file )
#   return(param_list)
# }

parameters = c( "epc.root_distrib_parm", "epc.min_percent_leafg", "epc.branch_turnover", "epc.froot_turnover", "epc.leaf_turnover", "epc.livewood_turnover")
defaults = "~/Repos/RHESSys-develop/rhessys/init/construct_stratum_defaults.c"

# evergreen
ev = make_par_list_from_def_file(input_hdr$stratum_def[1], parameters, defaults)
# shrub
sh = make_par_list_from_def_file(input_hdr$stratum_def[2], parameters, defaults)

s1 = list(list(input_hdr$soil_def[1], "m", "0.25"))
s2 = list(list(input_hdr$soil_def[2], "m", "0.25"))

pars_list = c(ev,sh,s1,s2)

input_def_pars = IOin_def_pars_simple(pars_list, n = 50, pct_range = 0.5)

# --------------------  Def File Parameters --------------------
# pars_list = list(
#   #veg
#   # ----- conifer -----
#   #list(input_hdr$stratum_def[1], "epc.root_distrib_parm", 10),
#   list(input_hdr$stratum_def[1], "epc.root_distrib_parm", c(1, 150)),
#   list(input_hdr$stratum_def[1], "epc.root_growth_direction", 0.9),
#   list(input_hdr$stratum_def[1], "epc.height_to_stem_coef", 2),
#   #list(input_hdr$stratum_def[1], "epc.max_root_depth", 10), #default is 9999
#   list(input_hdr$stratum_def[1], "epc.storage_transfer_prop", 0.7),
#   list(input_hdr$stratum_def[1], "epc.min_percent_leafg", c(0.001,0.5)),
#   #list(input_hdr$stratum_def[1], "epc.min_percent_leafg", 0.005),
#   #list(input_hdr$stratum_def[1], "epc.waring_pa", c(0.6)),
#   #list(input_hdr$stratum_def[1], "epc.branch_turnover", 0.01),
#   list(input_hdr$stratum_def[1], "epc.branch_turnover", c(0.005,0.3 )),
#   list(input_hdr$stratum_def[1], "epc.froot_turnover", 0.2),
#   #list(input_hdr$stratum_def[1], "epc.leaf_turnover",  0.4),
#   list(input_hdr$stratum_def[1], "epc.leaf_turnover",  c(0.1, 0.8)),
#   list(input_hdr$stratum_def[1], "epc.livewood_turnover",  c(0.005,0.3 )),

#   # SHRUB
#   # list("defs/stratum_shrub.def", "epc.allocation_flag", "combined"),
#   list("defs/stratum_shrub.def", "epc.livewood_turnover", 0.27),
#   list("defs/stratum_shrub.def", "epc.leaf_turnover", 0.33),
#   list("defs/stratum_shrub.def", "epc.froot_turnover", 0.45), # c(0.27, 0.6)
#   list("defs/stratum_shrub.def", "epc.alloc_livewoodc_woodc", 0.95)
# )
# input_def_pars = IOin_def_pars_simple(pars_list)
# input_def_pars = NULL

# -------------------- Make Tec File --------------------
input_tec_data = IOin_tec_std(start = "1979 10 1 1", end = dates[2], output_state = F)

# -------------------- Output filter --------------------
outfilter = build_output_filter(
  timestep = "daily",
  output_format = "csv",
  output_path = "output",
  output_filename = paste0("soilspin_basin_RULEID",ID),
  spatial_level = "basin",
  spatial_ID = 1,
  variables = c("patch.soil_cs.totalc", "patch.soil_ns.totaln", "stratum.cs.net_psn", "patch.lai", "patch.totalc","patch.evaporation", "patch.streamflow", "patch.snowpack.water_equivalent_depth", "hill.base_flow","patch.rz_storage","patch.unsat_storage", "patch.rootzone.depth", "patch.gw_drainage", "stratum.cs.totalc", "stratum.cs.leafc", "stratum.cs.cpool", "stratum.cs.live_stemc", "stratum.cs.dead_stemc", "stratum.cs.live_crootc", "stratum.cs.dead_crootc", "stratum.cs.frootc", "patch.litter_cs.totalc", "patch.litter_cs.totalc")
)
output_filter = IOin_output_filters(outfilter, file_name = paste0("./output/filters/filter_RULEID",ID,".yml"))

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
plotpdf_allvars(out_dir, "pfam_spinsens", pdfwidth = 12)
plotpdf_allvars(out_dir, "pfam_spinsens", pdfwidth = 12, step = "yearly", hide_legend = T)

# -------------------- RUN HPC --------------------
hpc = F
if (hpc) {
  # TURN ON VPN FIRST
  runscript = rhessysIO2pronghorn(rhout, name, rh_bin_replace = "/RHESSys/rhessys7.5", 
                                  dest = "/data/gpfs/assoc/firelab/william.burke/Pitman/", usr = "wburke@pronghorn.rc.unr.edu:",
                                  input_rhessys, input_hdr, input_def_pars, input_tec_data, output_filter,
                                  transfer_method = "scp")
  
  # THIS ASSUMES WORLDFILE, FLOWTABLE, AND CLIMATE FILES ARE ALREADY MOVED
  hpccmd <- paste0(
    "wsl ssh -i ~/rsa_key wburke@pronghorn.rc.unr.edu ",
    "\"cd /data/gpfs/assoc/firelab/william.burke/Pitman/ && ./scripts/run_dyn.sh ", runscript, "\""
  )
  system(hpccmd)
  # "Submitted batch job XXYYZZ" IS INDICATION OF SUCCESS
  
}
# -------------------- GET HPC --------------------
gethpc = F
if (gethpc) {
  library(rhutils)
  # runid = GetRunID()
  gethpcout <- paste0(
    "wsl ssh -i ~/rsa_key wburke@pronghorn.rc.unr.edu ",
    "\"cd /data/gpfs/assoc/firelab/william.burke/Pitman/ && ./scripts/collect_output.sh\"")
  cmdout = system(gethpcout,intern = T)
  # cat(cmdout) ; if (attr(cmdout,"status") %in% c(126, 127)) {stop("THIS FAILED")}
  # "Submitted batch job XXYYZZ" IS INDICATION OF SUCCESS
  hpcoutdir = gsub("Output CSVs moved to ","", cmdout[startsWith(cmdout, "Output CSVs moved to ")])
  rsync_cmd = paste0("wsl rsync -avz -e \"ssh -i ~/rsa_key\" wburke@pronghorn.rc.unr.edu:", hpcoutdir," ./output/")
  system(rsync_cmd)
  out_dir = paste0("output/",basename(hpcoutdir))
  plotpdf_allvars(out_dir, "spinsoils", pdfwidth = 12)
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
  plotpdf_allvars(out_dir, "spinsoils", pdfwidth = 12)
}


# collect_params(dir = "./")
# out_dir = collect_csvs(dir = "output", dir_base = "spinsoils")



# statefile = worldstate(input_rhessys$world_file)
# name = world_name(input_rhessys, "_soilspin")
# 
# file.rename(statefile, name)


