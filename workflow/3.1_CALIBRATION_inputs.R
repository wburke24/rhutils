library(RHESSysIOinR)
library(rhutils)
source("R/0_global_vars.R")
source("../R/output_aliases.R")

# MSR RHESSYS CALIBRATION INPUTS

# -------------------- Project/Run Name --------------------
#name = "Ward_msr_cal"
script = "msr_smcal"
prj = "Pitman"
name = paste0(prj,"_",script)

# -------------------- Input RHESSys --------------------
clim = "clim/pitman_netcdfgridmet_agg"

#dates = read_clim(clim, dates_out = T)
dates = c("1990 1 1 1", "2020 9 30 24")
#dates = c("1979 1 1 1", "2000 9 30 24")

input_rhessys = IOin_rhessys_input(
  version = "../bin/rhessys7.4",
  tec_file = "tecfiles/pitman.tec",
  world_file = "worldfiles/Pitman_MSR_soilspin_300y.world",
  world_hdr_prefix = name,
  flowtable = paste0("flowtables/",site,"_msr90m.flow"),
  start = dates[1],
  end = dates[2],
  output_folder = "output/",
  output_prefix = name,
  commandline_options = "-g -vmort_off -climrepeat",
  world_hdr_path = "hdr_Pitman"
)

# -------------------- Input Headers --------------------
input_hdr = IOin_hdr(
  basin = "defs/basin.def", hillslope = "defs/hill.def", zone = paste0("defs/zone_",site,".def"),
  soil = c(paste0("defs/soil_sandy-loam_",site,".def"), paste0("defs/soil_loam_",site,".def")),
  landuse = paste0("defs/lu_",site,".def"),
  stratum = c("defs/stratum_evergreen.def", "defs/stratum_evergreen_thin.def", 
              paste0("defs/stratum_shrub.def"), paste0("defs/stratum_shrub_thin.def"), 
              "defs/stratum_grass.def", "defs/stratum_nonveg.def"),
  basestations = paste0(clim, ".base")
)


# --------------------  Def File Parameters --------------------

pars_list = list(
  # list(input_hdr$stratum_def[1], "epc.livewood_turnover", c(0.005, 0.02, 0.04, 0.1, 0.01, 0.01, 0.01, 0.01) ), #0.013 was 0.01
  # list(input_hdr$stratum_def[2], "epc.livewood_turnover", c(0.7, 0.7, 0.7, 0.7, 0.2, 0.35, 0.5, 0.6)),
  # ----- soils -----
  # list(input_hdr$soil_def[1], "gsurf_intercept", 0.01),  # 0.001
  # list(input_hdr$soil_def[1], "active_zone_z", 10.0),       # 10
  # list(input_hdr$soil_def[1], "DOM_decay_rate", 0.05),     # 0
  # list(input_hdr$soil_def[1], "N_decay", 0.12),            # 1.2
  # # list(input_hdr$soil_def[1], "NO3_adsorption_rate", 0.0), # 0.0000005
  # 
  # # list(input_hdr$soil_def[1], "sat_to_gw_coeff", 0.013), # c(0.0, 0.4) 0.01325159
  # list(input_hdr$soil_def[1], "psi_air_entry", c(0.5)), # 0.18 c(0.1, 0.4) 0.218 0.18217991
  list(input_hdr$soil_def[1], "m", c(0.3,0.45)), # c(0.04, 0.6) 0.2 0.68355381
  # list(input_hdr$soil_def[1], "m_v", c(0.15,0.25,0.35,0.45,0.55)), # c(0.04, 0.6)
  # list(input_hdr$soil_def[1], "Ksat_0", c(90)), # c(50, 500) 250 228.94942248
  # list(input_hdr$soil_def[1], "Ksat_0_v", 250), # c(50, 500)
  # list(input_hdr$soil_def[1], "sat_to_gw_coeff", c(0.2)), # 1
  # list(input_hdr$soil_def[1], "snow_melt_Tcoef", c(0.0001)),    # 0.005  0.02, default is 0.05, 0.001 is better
  # list(input_hdr$soil_def[1], "soil_depth", 200.0),         # 10
  # 
  # list(input_hdr$soil_def[1], "albedo", c(0.01)), # 0.01 0.28 c(0.01, 0.28)

  # list(input_hdr$soil_def[1], "maximum_snow_energy_deficit", -60.0), # -10.0000000
  list(input_hdr$soil_def[1], "pore_size_index", c(0.1,0.3) ), # 0.18 seq(0.1,0.3, length.out = 8)
  list(input_hdr$soil_def[1], "porosity_0", c(0.4,0.7)) # 0.58

  # list(input_hdr$landuse_def[1], "sh_l", c(1)),
  # list(input_hdr$landuse_def[1], "sh_g", 1),

  # ----- ZONE -----
  # list(input_hdr$zone_def, "lapse_rate_precip_default",  0.0016), #
  # list(input_hdr$zone_def, "min_snow_temp", -4.5), #c(-5, -2) c(-5, 0) -4.93615248 -4.5
  # list(input_hdr$zone_def, "max_snow_temp", c(4.5)) #c(-1, 5) c(-2, 5) 4.96597559
)



#n = 8
#input_def_pars = NULL
#input_def_pars = lapply(pars_list, runif_sample, n)
input_def_pars = def_par_allcomb(pars_list)
#input_def_pars = unif_sample_all(pars_list,n_pars_each = n)

#input_def_pars = fill_rep_pars(pars_list = pars_list)

# set mz and ksatz to be same as base values for efficiency mostly
# vars = lapply(input_def_pars, "[[",2)
# if ("m" %in% vars) { input_def_pars[[which(vars=="m_v")]][[3]] = input_def_pars[[which(vars=="m")]][[3]] }
# if ("Ksat_0" %in% vars) { input_def_pars[[which(vars=="Ksat_0_v")]][[3]] = input_def_pars[[which(vars=="Ksat_0")]][[3]] }
# 
# # copy pars for all soils
# input_def_pars = dup_soil_pars(input_def_pars, input_hdr)
# input_def_pars = IOin_def_pars_simple(input_def_pars)
# input_def_pars = NULL

# -------------------- Make Tec File --------------------
input_tec_data = IOin_tec_std(start = "1990 10 1 1", end = dates[2], output_state = F)
#input_tec_data = IOin_tec_std(start = "1979 10 1 1", end = dates[2], output_state = F)

# -------------------- Output filter --------------------

# outfilter = build_output_filter(
#   timestep = "daily",
#   output_format = "csv",
#   output_path = "output",
#   output_filename = paste0(script,"_basin"),
#   spatial_level = "basin",
#   spatial_ID = 1,
#   variables = c("patch.streamflow", "patch.snowpack.water_equivalent_depth", "hill.base_flow","patch.return_flow", "hill.gw.Qout", 
#                 "patch.rz_storage", "patch.unsat_storage", "patch.sat_deficit", "patch.rootzone.depth")
# )

outfilter = build_output_filter(
  timestep = "daily",
  output_format = "csv",
  output_path = "output",
  output_filename = paste0(script,"_patch"),
  spatial_level = "patch",
  spatial_ID = 1,
  variables = c("patch.streamflow", "patch.snowpack.water_equivalent_depth","patch.rz_storage", "patch.unsat_storage", "patch.sat_deficit", "patch.rootzone.depth", "patch.lai", "patch.totalc")
)

output_filter = IOin_output_filters(outfilter, file_name = paste0("./output/filters/",script,"_filter.yml"))


# -------------------- HANDLE RUN IDS --------------------
runid = GetRunID(increment = T)

save(input_rhessys, input_hdr, input_def_pars, input_tec_data, output_filter, file = paste0("robj/", name,"_RunID",runid, ".RData"))
# change output files to have an ID?
output_filter = AddRunIDtoOutputFilters(output_filter,runid)

# -------------------- RUN --------------------
write_param_table(input_def_pars)
rhout = run_rhessys_multi(
  input_rhessys = input_rhessys,
  hdr_files = input_hdr,
  def_pars = input_def_pars,
  tec_data = input_tec_data,
  output_filter = output_filter,
  return_cmd = T,
  n_cores = 11
)

# rhout = rhout[1:2]

hpc = T
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

gethpc = T
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
}
