# ============================== 3.1 CALIBRATION INPUTS ==============================
# Builds one calibration batch: RHESSys inputs, def file parameters (constants, frozen, and
# sampled ranges), tec, output filter, and a RunID. Saves everything to robj/<name>_RunID<k>.RData.
# Then run with 3.2_CALIBRATION_run.R (local or HPC), and evaluate/iterate with 3.3_CALIBRATION_eval.R.
# Adapted from the Ward project (CARB/Ward/R/3.2_CALIBRATION.R).
#
# Def file parameter lists use list(Def_file = , Variable = , Value = ):
#   - Value of length 1 = constant;  Value = c(lo, hi) = range to sample
#   - if a parameter is given more than once, the LAST entry is used (and reported)
library(RHESSysIOinR)
library(rhutils)
source("../../../Repos/rhutils/workflow/output_aliases.R") # output_vars_streamflowcal, etc.

# -------------------- Project/Run Name --------------------
name = "MYSITE_soil_cal"

# -------------------- Input RHESSys --------------------
clim = "clim/MYSITE_basestation"
dates = c("1979 1 1 1", "2020 9 30 24")

input_rhessys = IOin_rhessys_input(
  version = "../bin/rhessys7.5",
  tec_file = "tecfiles/MYSITE.tec",
  world_file = "worldfiles/MYSITE_spun.world",
  world_hdr_prefix = paste0("hdr_", name),
  flowtable = "flowtables/MYSITE.flow",
  start = dates[1],
  end = dates[2],
  output_folder = "output/",
  output_prefix = name,
  commandline_options = "-g -vmort_off"
)

# -------------------- Input Headers --------------------
input_hdr = IOin_hdr(
  basin = "defs/basin.def", hillslope = "defs/hill.def", zone = "defs/zone.def",
  soil = c("defs/soil_sandy-loam.def", "defs/soil_loam.def"), landuse = "defs/lu.def",
  stratum = c("defs/stratum_evergreen.def", "defs/stratum_shrub.def", "defs/stratum_grass.def", "defs/stratum_nonveg.def"),
  basestations = paste0(clim, ".base")
)
soil_def1 = input_hdr$soil_def[1]
soil_def2 = input_hdr$soil_def[2]
zone_def = input_hdr$zone_def

# -------------------- Def File Parameters --------------------
# constants (e.g. veg pars from spinup, soil depth)
const_pars = list(
  list(Def_file = soil_def1, Variable = "soil_depth", Value = 200)
)
const_pars = dup_soil_pars(const_pars, input_hdr) # copy soil pars to the other soil def(s)

# frozen values - only used for parameters NOT sampled in this batch (see below)
pars_froz = list(
  list(Def_file = soil_def1, Variable = "porosity_0", Value = 0.40),
  list(Def_file = soil_def2, Variable = "porosity_0", Value = 0.41),
  list(Def_file = zone_def, Variable = "lapse_rate_tmax", Value = 0.0058)
)

# calibration ranges c(lo, hi)
pars_cal = list(
  list(Def_file = soil_def1, Variable = "Ksat_0", Value = c(150, 400)),
  list(Def_file = soil_def1, Variable = "Ksat_0_v", Value = c(150, 400)),
  list(Def_file = soil_def1, Variable = "m", Value = c(0.1, 0.5)),
  list(Def_file = soil_def1, Variable = "m_v", Value = c(0.2, 0.35)),
  list(Def_file = soil_def2, Variable = "Ksat_0", Value = c(150, 400)),
  list(Def_file = soil_def2, Variable = "Ksat_0_v", Value = c(150, 400)),
  list(Def_file = soil_def2, Variable = "m", Value = c(0.25, 0.45)),
  list(Def_file = soil_def2, Variable = "m_v", Value = c(0.2, 0.35)),
  list(Def_file = input_hdr$hillslope_def, Variable = "gw_loss_coeff", Value = c(0.8, 1.0))
)

# -------------------- Sample --------------------
# newpars_out comes from 3.3 (narrowed ranges, already sampled); otherwise sample the full ranges
n_sets = 400
if (exists("newpars_out")) {
  cat("Using newpars_out from the previous iteration\n")
  pars_cal_out = newpars_out
} else {
  pars_cal_out = IOin_def_pars_latin_hypercube(pars_cal, n = n_sets)
  # space-filling Sobol (QMC) sequence instead (powers of 2, can be extended with skip =):
  # pars_cal_out = qmc_par_sample(pars_cal, n = 512, log_params = c(paste0(soil_def1, "::Ksat_0")))
}

# frozen values only for parameters that aren't sampled this batch (the sampled value wins)
par_key = function(pars) vapply(pars, function(x) paste(x[[1]], x[[2]], sep = "::"), character(1))
froz_sampled = par_key(pars_froz) %in% par_key(pars_cal_out)
if (any(froz_sampled)) {
  cat("Frozen parameters superseded by sampled ranges this batch:\n ", paste(par_key(pars_froz)[froz_sampled], collapse = "\n  "), "\n")
}

input_def_pars = c(const_pars, pars_froz[!froz_sampled], pars_cal_out)
input_def_pars = fill_rep_pars(input_def_pars)
# names, equal lengths, duplicates (last kept), def files in the header, parameter names RHESSys
# actually reads (typos are otherwise silently ignored), and physical bounds
input_def_pars = validate_def_pars(input_def_pars, hdr_files = input_hdr, rh_src = "../../../Repos/RHESSys-develop/rhessys")

# -------------------- Tec --------------------
# print start after the sim start = warm-up period excluded from output
input_tec_data = IOin_tec_std(start = "1981 10 1 1", end = dates[2], output_state = FALSE)

# -------------------- Output filter --------------------
outfilter = build_output_filter(
  timestep = "daily",
  output_format = "csv",
  output_path = "output",
  output_filename = paste0(name, "_basin"),
  spatial_level = "basin",
  spatial_ID = 1,
  variables = output_vars_streamflowcal
)
output_filter = IOin_output_filters(outfilter, file_name = paste0("./output/filters/filter_", name, ".yml"))

# -------------------- Run ID + save --------------------
runid = GetRunID(increment = TRUE)
output_filter = AddRunIDtoOutputFilters(output_filter, runid)
dir.create("robj", showWarnings = FALSE)
save(input_rhessys, input_hdr, input_def_pars, input_tec_data, output_filter, file = paste0("robj/", name, "_RunID", runid, ".RData"))
