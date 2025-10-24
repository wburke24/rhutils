# soil_spin
library(RHESSysIOinR)
library(rhutils)
library(data.table)
library(stringr)
# source("../R/fun_spinup.R")

# Uses a worldfile to generate single patch worlds based on each veg parameter, then runs each world and reincorporates the
# resulting soil nutrient values into the original worldfile

# ---------- Input world ----------
# world_path = "worldfiles/RedRockMSR30m_tree2shrubnobare.world"
# flow_path = "flowtables/RedRockMSR30m_tree2shrubnobare.flow"
world_path = "worldfiles/RedRockMSR30m_treegrass_LPC.world"
flow_path = "flowtables/RedRockMSR30m_treegrass_LPC.flow"

dest = "soil_spin"
if (!dir.exists(file.path("worldfiles",dest))) {
  dir.create(file.path("worldfiles",dest))
}
if (!dir.exists(file.path("flowtables",dest))) {
  dir.create(file.path("flowtables",dest))
}

# read world
world = as.data.table(read_world(world_path, hill_col = T, zone_col = T, patch_col = T))
# add cols
world = world_add_level_i(world)
world = world_add_patch_vegparmIDcol(world)
world = world_add_familyID_RuleID(world)

# --------- STANDARD VERSION -------------- make worlds from veg
standard = F
if (standard) {
  veg = unique(world$vegparm[!is.na(world$vegparm)])
  output_world_paths = file.path("worldfiles",dest, paste0(gsub(".world","", basename(world_path)),"_vegID", veg,".world")) 
  output_flow_paths = file.path("flowtables",dest, paste0(gsub(".world","", basename(world_path)),"_vegID", veg,".flow")) 
  # ---------- Make 1 Patch Worlds ----------
  # selecting patches based on single veg parm IDs, since world is 1 strata
  p1_veg = rbindlist(lapply(veg, function(x) world[min(which(world$vegparm == x)), ]))
  # make new worlds
  newworlds = lapply(p1_veg$unique_ID, FUN = extract_world, world = world)
}

# --------------- MSR Version -----------------
msr = T
if (msr) {
  rules = unique(world$rule_ID[!is.na(world$rule_ID)])
  output_world_paths = file.path("worldfiles",dest, paste0(gsub(".world","", basename(world_path)),"_ruleID", rules,".world"))
  output_flow_paths = file.path("flowtables",dest, paste0(gsub(".world","", basename(world_path)),"_ruleID", rules,".flow"))  
  # select patch families
  pfams = unlist(lapply(rules, select_pfam, world))
  # make new worlds
  newworlds = lapply(pfams, FUN = pfam_extract_world, world = world)

  # cover fraction VARIES - set to mean for each ruleID
  world_covers = world[vars == "cover_fraction"]
  world_covers$values = as.numeric(world_covers$values)
  cover_avg = world_covers[,lapply(.SD,mean), by = .(rule_ID), .SDcols = "values"]
  cover_avg$values = round(cover_avg$values,2)
  
  newworlds = lapply(newworlds, function(x) {
    x[vars == "cover_fraction", values := cover_avg$values[as.numeric(unique(x$rule_ID[!is.na(x$rule_ID)]))]]
    return(x)
  })

}

mapply(write_world,newworlds,output_world_paths)

# ---------- Make 1 Patch Flow Tables ----------
# newflows = lapply(newworlds, make_1pflow)
newflows = lapply(newworlds, make_1pfamflow)
mapply(writeLines, newflows, output_flow_paths)

# -------------------- RUN FOR SOIL NUTRIENT SPINUP --------------------
namenoext = gsub(".world","",basename(world_path))
output_world_paths = list.files(file.path("worldfiles/",dest),pattern = paste0(namenoext,".*.world$"),full.names = T)
output_flow_paths = list.files(file.path("flowtables/",dest),pattern = paste0(namenoext,".*.flow$"),full.names = T)
IDs = str_extract(output_world_paths, "(?<=ruleID)\\d+")

# IDs = c("1", "2", "3", "4", "5", "6", "7", "8")

run_cmds = list()

for (i in seq_along(IDs)) {
  # -------------------- Project/Run Name --------------------
  name = paste0("SoilSpin_ID",IDs[i])
  # -------------------- Input RHESSys --------------------
  clim = "clim/RedRock"
  
  #dates = c("1979 1 1 1", "2020 9 30 24")
  dates = c("1979 1 1 1", "3480 9 30 24")

  input_rhessys = IOin_rhessys_input(
    version = "../bin/rhessys7.5",
    tec_file = paste0("tecfiles/",name,".tec"),
    world_file = output_world_paths[i],
    world_hdr_prefix = paste0("hdr_",name),
    world_hdr_path = "hdr_files",
    flowtable = output_flow_paths[i],
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
  # ------------------- soil pars -------------------
  # n = 10

  # pars_list = list(
  #   # soils
  #   list(input_hdr$soil_def[1], Variable = "soil_depth", Value = c(1,3)), 
  #   list(input_hdr$soil_def[2], Variable = "soil_depth", Value = c(1,3)), 

  #   list(input_hdr$soil_def[1], Variable = "m", Value = c(0.01, 0.3)), #0.1953 default is 0.12
  #   list(input_hdr$soil_def[2], Variable = "m", Value = c(0.01, 0.3)), #0.2485
  #   list(input_hdr$soil_def[1], Variable = "m_v", Value = c(0.05, 0.3)), #
  #   list(input_hdr$soil_def[2], Variable = "m_v", Value = c(0.05, 0.3)), #

  #   list(input_hdr$soil_def[1], Variable = "Ksat_0", Value = c(3, 500)), #
  #   list(input_hdr$soil_def[2], Variable = "Ksat_0", Value = c(3, 500)), #
  #   list(input_hdr$soil_def[1], Variable = "Ksat_0_v", Value = c(3, 500)), #
  #   list(input_hdr$soil_def[2], Variable = "Ksat_0_v", Value = c(3, 500)), #
    
  #   list(input_hdr$soil_def[1], Variable = "pore_size_index", Value = c(0.1, 0.3)), # 0.204
  #   list(input_hdr$soil_def[2], Variable = "pore_size_index", Value = c(0.1, 0.3)), #
  #   list(input_hdr$soil_def[1], Variable = "porosity_0", Value = c(0.3, 0.55)), # 0.435
  #   list(input_hdr$soil_def[2], Variable = "porosity_0", Value = c(0.3, 0.55)), #
  #   list(input_hdr$soil_def[1], Variable = "psi_air_entry", Value = c(0.1, 0.3)), #0.218
  #   list(input_hdr$soil_def[2], Variable = "psi_air_entry", Value = c(0.1, 0.3)), #
    
  #   list(input_hdr$soil_def[1], Variable = "sat_to_gw_coeff", Value = c(0)), #1
  #   list(input_hdr$soil_def[2], Variable = "sat_to_gw_coeff", Value = c(0)) #
  # )
  # input_def_pars = lapply(pars_list, runif_sample, n)

  input_def_pars = NULL
  
  # -------------------- Make Tec File --------------------
  input_tec_data = IOin_tec_std(start = "1979 10 1 1", end = dates[2], output_state = T)
  
  # -------------------- Output filter --------------------
  source("R/output_aliases.R")

  outvars = c("patch.soil_cs.totalc", "patch.soil_ns.totaln", "patch.lai", "patch.rootzone.depth", "patch.rz_storage","patch.unsat_storage", "stratum.cs.totalc", "stratum.cs.leafc", "stratum.cs.cpool", "stratum.cs.live_stemc", "stratum.cs.dead_stemc", "patch.evaporation","patch.evaporation_surf","patch.transpiration_unsat_zone","patch.transpiration_sat_zone","stratum.epv.height")
  
  outfilter = build_output_filter(
    timestep = "daily",
    output_format = "csv",
    output_path = "output",
    output_filename = paste0("soilspin_basin_ID",IDs[i]),
    spatial_level = "basin",
    spatial_ID = 1,
    variables = outvars # c(output_vars_minimal, output_cpools,"patch.litter_cs.totalc", "patch.rootzone.depth")
  )
  output_filter = IOin_output_filters(outfilter, file_name = paste0("./output/filters/filter_ID",IDs[i],".yml"))
  
  run_cmds[[i]] = run_rhessys_single(
    input_rhessys = input_rhessys,
    hdr_files = input_hdr,
    def_pars = input_def_pars,
    tec_data = input_tec_data,
    output_filter = output_filter,
    return_cmd = T,
    write_run_metadata = F
  )
  
}

# -------------------- MANUAL PARALLEL RUNS --------------------
runid = GetRunID(increment = T)
# output_filter = AddRunIDtoOutputFilters(output_filter,runid)
save(run_cmds, file = paste0("robj/", name,"_RunID",runid, ".RData"))

parallel_runs(run_cmds = run_cmds)

out_dir = collect_output(basename = paste0("pfam_soilspin_RunID", GetRunID(),"_"))
plotpdf_allvars(out_dir = out_dir,out_name = paste0("pfam_soilspin_RunID",GetRunID()), step = "yearly", pdfwidth = 10, hide_legend = F, summary_plots = F )


# -------------------- MANUAL PARALLEL RUNS --------------------
manualpara = F
if (manualpara) {

  # -------------------- HANDLE RUN IDS --------------------
  runid = GetRunID(increment = T)
  # output_filter = AddRunIDtoOutputFilters(output_filter,runid)
  save(run_cmds, file = paste0("robj/", name,"_RunID",runid, ".RData"))

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

  out_dir = collect_output(basename = paste0("pfam_soilspin_RunID", GetRunID(),"_"))
  plotpdf_allvars(out_dir = out_dir,out_name = paste0("pfam_soilspin_RunID",GetRunID()), step = "yearly", pdfwidth = 10, hide_legend = F, summary_plots = F )

}

# -------------------- Run --------------------

# run_rhessys_single(
#   input_rhessys = input_rhessys,
#   hdr_files = input_hdr,
#   def_pars = input_def_pars,
#   tec_data = input_tec_data,
#   output_filter = output_filter,
#   return_cmd = F,
#   write_run_metadata = T
# )


# 
out_dir = collect_output()
# collect_params(dir = "./")
# out_dir = collect_csvs(dir = "output", dir_base = "spinsoils")

plotpdf_allvars(out_dir, "spinsoils")

# statefile = worldstate(input_rhessys$world_file)
# name = world_name(input_rhessys, "_soilspin")
# 
# file.rename(statefile, name)


# -------------------- Create New World with Spun Soil Nutrients --------------------

# Choose which worlds to use
world_path_spun = list.files("worldfiles/soil_spin/","Y3480", full.names = T)
spun_world_paths = world_path_spun
original_world_path = world_path
ID = "rule_ID"

new_world_from_spun_worlds = function(spun_world_paths, original_world_path, ID = "rule_ID") {
  # ==================== Check Inputs ====================
  # check valid ID
  if (!ID %in% c("rule_ID","veg_parm_ID")) {
    stop("ID must be 'rule_ID' or 'veg_parm_ID'")
  }
  # check spun worlds exist
  if (length(spun_world_paths) == 0) {
    stop("No spun world paths provided")
    if (!all(file.exists(spun_world_paths))) {
      stop("Not all spun world paths exist")
    }
  }
  if (!file.exists(original_world_path)) {
    stop("Original world path does not exist")
  }

  # ==================== read spun and original worlds ====================
  worlds_spun = lapply(spun_world_paths, FUN = function(X){as.data.table(read_world(X, patch_col = T))})
  worlds_spun = lapply(worlds_spun, world_add_patch_vegparmIDcol)
  world_dest = as.data.table(read_world(original_world_path, patch_col = T))
  world_dest = world_add_patch_vegparmIDcol(world_dest)

  if (ID == "rule_ID") {
    worlds_spun = lapply(worlds_spun, world_add_familyID_RuleID)
    world_dest = world_add_familyID_RuleID(world_dest)
    # if no rule_ID in spun worlds, get from dest via family ID
    if (all(sapply(worlds_spun, function(X) all(is.na(X$rule_ID))))) {
      worlds_spun = lapply(worlds_spun, function(X) {
        X = merge(X, unique(world_dest[,.(family_ID, rule_ID)]), by = "family_ID", all.x = T)
        return(X)
      })
    }
  }

  # ==================== check IDs in dest match spun ====================
  if (ID == "rule_ID") {
    ruleIDs = unique(world_dest$rule_ID[!is.na(world_dest$rule_ID)])
    ruleIDs_spun = sapply(worlds_spun, function(X) unique(X$rule_ID[!is.na(X$rule_ID)]))
    if (!all(ruleIDs %in% ruleIDs_spun)) {
      stop("Not all rule_IDs in destination world are present in spun worlds")
    }
  } else {
    vegparm = unique(world_dest$vegparm[!is.na(world_dest$vegparm)])
    vegparm_spun = sapply(worlds_spun, function(X) unique(X$vegparm[!is.na(X$vegparm)]))
    if (!all(vegparm %in% vegparm_spun)) {
      stop("Not all veg_parm_IDs in destination world are present in spun worlds")
    }
  }

  # vars to replace - ***come back to this***
  soil_vars = c("soil_cs.soil1c",  "soil_cs.soil2c", "soil_cs.soil3c", 
              "soil_cs.soil4c", "soil_ns.sminn", "soil_ns.nitrate"
              # "rootzone.depth", "snow_stored", "rain_stored", 
              # "epv.wstress_days", "epv.max_fparabs", "epv.min_vwc", 
              # "gw.storage", "gw.NO3"
              )
  # ==================== replace values in dest with spun ====================
  if (ID == "rule_ID") {
    l = rbindlist(worlds_spun)
    l = l[vars %in% c(soil_vars) & !is.na(rule_ID), .(rule_ID, veg_parm_ID, vars, values)]
    world_new = merge(world_dest, l, by = c("rule_ID", "veg_parm_ID", "vars"), all.x = T, suffixes = c("", ".spun"), sort = F)
  } else {
    l = rbindlist(worlds_spun)
    l = l[vars %in% c(soil_vars) & !is.na(vegparm), .(vegparm, vars, values)]
    world_new = merge(world_dest, l, by = c("vegparm", "vars"), all.x = T, suffixes = c("", ".spun"), sort = F)
  }

  world_new[!is.na(values.spun), values := values.spun]
  world_new[, values.spun := NULL]

  return(world_new)
}

new_world = new_world_from_spun_worlds(spun_world_paths = spun_world_paths, original_world_path = original_world_path, ID = ID)
write_world(new_world, gsub(".world", "_spun_nutrients.world", world_path))


oldver = F
if (oldver) {
  # old version
worlds_spun = lapply(world_path_spun, FUN = function(X){as.data.table(read_world(X, patch_col = T))})
worlds_spun = lapply(worlds_spun, world_add_familyID_RuleID)

soil_vars = c("soil_cs.soil1c",  "soil_cs.soil2c", "soil_cs.soil3c", 
              "soil_cs.soil4c", "soil_ns.sminn", "soil_ns.nitrate"
              # "litter.rain_stored", "litter_cs.litr1c", "litter_ns.litr1n", 
              # "litter_cs.litr2c", "litter_cs.litr3c", "litter_cs.litr4c", 
              # "rootzone.depth", "snow_stored", "rain_stored", 
              # "epv.wstress_days", "epv.max_fparabs", "epv.min_vwc", 
              # "gw.storage", "gw.NO3"
              )
world_dest = as.data.table(read_world(world_path, patch_col = T))
world_dest = world_add_familyID_RuleID(world_dest)
library(stringr)
ruleIDs = as.numeric(str_remove(str_extract(world_path_spun,"ruleID\\d+"),"ruleID"))
# vegids = as.numeric(gsub("vegID","", regmatches(world_path_spun, regexpr("vegID\\d+", world_path_spun))))

# ew bad
for (r in ruleIDs) {
  for (vid in vegparm) {
      for (v in soil_vars) {
    world_dest[vars %in% soil_vars & rule_ID == r & vars == v, "values"] = 
      worlds_spun[[which(ruleIDs == r)]][vars %in% soil_vars & rule_ID == r & vars == v, "values"]
  }
  }

}
} 




