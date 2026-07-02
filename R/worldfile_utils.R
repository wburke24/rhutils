# worldfile utils

# ================================================================================
# add patch areas based on worldfile
#' Add patch areas from worldfile
#'
#' @param output_df Data table with patchID column
#' @param world_source Path to worldfile or data frame
#' @export
add_patch_areas = function(output_df, world_source) {
  if ("area" %in% names(output_df)) {
    cat("output_df already contains 'area' column.")
    return(output_df)
  }
  world = RHESSysPreprocessing::read_world(world_source)
  patchareas = world[world$level == "patch" & world$vars == "area",c("values","ID")]
  names(patchareas) = c("area", "patchID")
  patchareas$patchID = as.numeric(patchareas$patchID)
  patchareas$area = as.numeric(patchareas$area)
  # do some checking first
  if (!length(unique(patchareas$patchID)) == length(unique(output_df$patchID)) | !all(patchareas$patchID %in% output_df$patchID)) {
    cat("Patch IDs in the data did not match IDs in the worldfile")
    return(output_df)
  }
  output_df = output_df[patchareas, on = .(patchID)]
  return(output_df)
}

# ================================================================================
# add patch veg parm ID based on worldfile
#' Add patch vegetation parameter IDs from worldfile
#'
#' @param output_df Data table with patchID column
#' @param world_source Path to worldfile or data frame
#' @export
add_patch_veg_parm_IDs = function(output_df, world_source) {
  if ("veg_parm_ID" %in% names(output_df)) {
    cat("output_df already contains 'veg_parm_ID' column.")
    return(output_df)
  }
  world = RHESSysPreprocessing::read_world(world_source, patch_col = TRUE)
  veg_parm_IDs = world[world$level == "canopy_strata" & world$vars == "veg_parm_ID",c("values","ID", "patch_ID")]
  names(veg_parm_IDs) = c("veg_parm_ID","canopy_strataID", "patchID")
  veg_parm_IDs$patchID = as.numeric(veg_parm_IDs$patchID)
  veg_parm_IDs$canopy_strataID = as.numeric(veg_parm_IDs$canopy_strataID)

  # collapse veg parms
  veg_parm_IDs = aggregate(veg_parm_IDs$veg_parm_ID,by = list(patchID = veg_parm_IDs$patchID), FUN = paste0, collapse = "_" )
  names(veg_parm_IDs) = c("patchID","veg_parm_ID")

  # do some checking first
  if (!length(unique(veg_parm_IDs$patchID)) == length(unique(output_df$patchID)) | !all(veg_parm_IDs$patchID %in% output_df$patchID)) {
    cat("Patch IDs in the data did not match IDs in the worldfile")
    return(output_df)
  }
  # output_df = output_df[veg_parm_IDs, on = .(patchID)]
  output_df = merge(output_df, veg_parm_IDs, by = "patchID")
  return(output_df)
}

# ================================================================================
# add strata veg parm ID based on worldfile to strata level output
#' Add strata vegetation parameter IDs from worldfile
#'
#' @param output_df Data table with stratumID column
#' @param world_source Path to worldfile or data frame
#' @export
add_strata_veg_parm_IDs = function(output_df, world_source) {
  if ("veg_parm_ID" %in% names(output_df)) {
    cat("output_df already contains 'veg_parm_ID' column.")
    return(output_df)
  }
  world = RHESSysPreprocessing::read_world(world_source)
  veg_parm_IDs = world[world$level == "canopy_strata" & world$vars == "veg_parm_ID",c("values","ID")]
  names(veg_parm_IDs) = c("veg_parm_ID","stratumID")
  veg_parm_IDs$stratumID = as.numeric(veg_parm_IDs$stratumID)

  # do some checking first
  if (!length(unique(veg_parm_IDs$stratumID)) == length(unique(output_df$stratumID)) | !all(veg_parm_IDs$stratumID %in% output_df$stratumID)) {
    cat("Stratum IDs in the data did not match IDs in the worldfile")
    return(output_df)
  }
  output_df = output_df[veg_parm_IDs, on = .(stratumID)]
  return(output_df)
}


# ================================================================================
# add patch family ID and area
#' Add patch family IDs and areas
#'
#' @param output_df Data table with patchID, area, year, month
#' @export
add_patchfamily_IDarea = function(output_df) {
  if ("familyID" %in% names(output_df) & "family_area" %in% names(output_df)) {
    cat("output_df already contains 'familyID' and 'family_area' columns.")
    return(output_df)
  }
  output_df$familyID = floor(output_df$patchID/100)
  family_area = output_df[,sum(area), by = c("familyID", "year","month")]
  names(family_area)[4] = "family_area"
  output_df = output_df[family_area, on = .(familyID, year, month)]
  output_df$area_prop = output_df$area/output_df$family_area
  return(output_df)
}

# ================================================================================
#' Add a variable as a new column in the world data frame
#' 
#' @param world A data frame representing the world data, typically produced by \code{read_world}.
#' @param varname The name of the variable to be added as a new column.
#' @return The modified world data frame with the new variable added as a column.
#' @export

world_add_var_as_col = function(world, varname) {
  if (!varname %in% world$vars) {
    stop(paste("Variable", varname, "not found in world data frame."))
  }
  levels = c("world", "basin", "hillslope", "zone", "patch", "canopy_strata")
  # find level of varname
  var_level = unique(world$level[world$vars == varname])
  # check by level, if there are more than 1 num_x, then join by current level IDs, otherwise, go up a level and check again
  level_index = match(var_level, levels)
  level_i = level_index
  while (level_i > 1) {
    level_i = level_i - 1
    level_name = levels[level_i]
    n_level = world[world$level == level_name & grepl("num_", world$vars), "values"] %>% max()
    if (n_level > 1) {
      level_i = level_i + 1
      level_name = levels[level_i]
      break
    }
  }
  world = as.data.table(world)
  # join by the ID of level_i
  if (level_name == "canopy_strata") {
    world = merge.data.table(world, world[world$vars == varname, .(ID, values)], by = "ID", all.x = TRUE, sort = F)
  } else if (level_name == "patch") {
    world = merge.data.table(world, world[world$vars == varname, .(patch_ID, values)], by = "patch_ID", all.x = TRUE, sort = F)
  } else if (level_name == "zone") {
    world = merge.data.table(world, world[world$vars == varname, .(zone_ID, values)], by = "zone_ID", all.x = TRUE, sort = F)
  } else if (level_name == "hillslope") {
    world = merge.data.table(world, world[world$vars == varname, .(hillslope_ID, values)], by = "hillslope_ID", all.x = TRUE, sort = F)
  } else if (level_name == "basin") {
    return("Fix this.\n")
  } else if (level_name == "world") {
    return("Trying to join to world probably not working.\n")
  }
  names(world)[names(world) == "values.y"] = varname
  names(world)[names(world) == "values.x"] = "values"

  return(as.data.frame(world))
}


# ================================================================================
# aggregate to patch family using area weights
#' Aggregate patch data to patch families
#'
#' @param output_df Data table with patch-level outputs
#' @export
patches2patchfamily = function(output_df) {
  reqvars = c("basinID", "hillID", "zoneID", "familyID", "year","month","day","area_prop")
  if (any(!reqvars %in% names(output_df))) {
    cat("Missing required column(s): ", reqvars[!reqvars %in% names(output_df)] )
  }
  aggvars = names(output_df)[!names(output_df) %in% c("day", "month", "year", "basinID", "hillID", "zoneID", "patchID", "area", "familyID", "family_area", "area_prop","run","date","wy","yd","Aridity", "run_type","veg_parm_ID")]
  output_df = output_df[,(aggvars) := .SD*area_prop, .SDcols = aggvars]
  # aggvars2 = names(output_df)[names(output_df) %in% c("basinID", "hillID", "zoneID", "familyID", "year","wy","yr_mn","month","day")]
  df_out = output_df[,lapply(.SD, sum) ,by = c("basinID", "hillID", "zoneID", "familyID", "year","month","day"), .SDcols = c(aggvars,"area")]
  return(df_out)
}


# ================================================================================
# aggregate to patches to basin by veg parm ID using area weights
# @export
# patches2basinvegID = function(output_df) {
#
#   aggvars = names(output_df)[!names(output_df) %in% c("day", "month", "year", "basinID", "hillID", "zoneID", "patchID", "area", "familyID", "family_area", "area_prop")]
#   output_df = output_df[,(aggvars) := .SD*area_prop, .SDcols = aggvars]
#   # aggvars2 = names(output_df)[names(output_df) %in% c("basinID", "hillID", "zoneID", "familyID", "year","wy","yr_mn","month","day")]
#   df_out = output_df[,lapply(.SD, sum) ,by = c("basinID", "hillID", "zoneID", "familyID", "year","month","day"), .SDcols = c(aggvars,"area")]
#   return(df_out)
# }

# ================================================================================
# add var in worldfile below specified loc_var
#' Insert a variable into a worldfile table
#'
#' @param world Worldfile data frame
#' @param var Variable name to insert
#' @param value Value to assign to the new variable
#' @param loc_var Existing variable name after which to insert
#' @return Updated worldfile data frame with the new row inserted
#' @export
world_add_var = function(world, var, value, loc_var) {
  
  insert_pos <- which(world$vars == loc_var)
  # Row to insert
  insert_row = world[insert_pos[1],]
  insert_row$values = value
  insert_row$vars = var

  insert_pos_shift = insert_pos - data.table::shift(insert_pos, 1, fill = 0)
  insert_pos_factor = rep(seq_along(insert_pos), insert_pos_shift)
  insert_pos_factor = c(insert_pos_factor, rep(tail(insert_pos_factor,1)+1, nrow(world)-tail(insert_pos,1)))

  world_split = split(world,insert_pos_factor)
  world_split = lapply(world_split, function(X,Y) {
    tmp = rbind(X,Y);tmp[nrow(tmp),"unique_ID"] = tmp[nrow(tmp)-1,"unique_ID"];return(tmp)
  }, insert_row)
  
  world_comb = data.table::rbindlist(world_split)
  world_comb = world_comb[-nrow(world_comb),]

  return(world_comb)
  

  # Adjust for shifting indices after insertion
  # offset <- 0
  # for (pos in insert_positions) {
  #   world <- rbind(
  #     world[1:(pos + offset), ],
  #     insert_row,
  #     world[(pos + offset + 1):nrow(world), ]
  #   )
  #   offset <- offset + 1
  # }
}

# ================================================================================
#' Extract a single hillslope from a worldfile table
#'
#' Given a full worldfile table (as returned by \code{read_world}), extract
#' all rows corresponding to a single hillslope, identified by its
#' \code{hillslope_ID}. The function includes the world and basin header rows
#' above the selected hillslope and all rows up to (but not including) the
#' next hillslope in the table.
#'
#' @param world A \code{data.frame} or \code{data.table} representing the
#'   parsed worldfile, typically produced by \code{read_world}. It must
#'   contain at least the columns \code{vars}, \code{values}, and
#'   \code{level}.
#' @param hillslope_id Integer ID of the hillslope to extract; this is
#'   matched against rows where \code{vars == "hillslope_ID"} and
#'   \code{as.numeric(values) == hillslope_id}.
#'
#' @return A subset of \code{world} containing the rows for the specified
#'   hillslope, including relevant world and basin rows. An error is thrown
#'   if the hillslope ID is not found or if multiple matching hillslopes are
#'   detected.
#'
#' @examples
#' \dontrun{
#' world <- read_world("my_worldfile.world")
#' hill1 <- extract_hillslope_from_world(world, hillslope_id = 1)
#' }
#'
#' @export
extract_hillslope_from_world = function(world, hillslope_id) {
  # Filter to selected hillslope
  hillslope_rows = which(world$vars == "hillslope_ID" & as.numeric(world$values) == hillslope_id)
  if(length(hillslope_rows) == 0) {
    stop(paste("Hillslope ID", hillslope_id, "not found in worldfile"))
  } else if(length(hillslope_rows) > 1) {
    stop(paste("Multiple entries found for hillslope ID", hillslope_id))
  }

  # from hill id, select to next hillslope_ID
  start_row = hillslope_rows
  end_row = which(world$vars == "hillslope_ID" & as.numeric(world$values) != hillslope_id & seq_along(world$vars) > hillslope_rows)[1] - 1
  if(is.na(end_row)) {
    end_row = nrow(world)
  }
  filtered_world = world[start_row:end_row, ]

  # add world and basin to top
  basin_rows = which(world$level == "basin")
  world_rows = which(world$level == "world")
  filtered_world = rbind(world[world_rows, ], world[basin_rows, ], filtered_world)

  # set num hillslopes to 1
  filtered_world[filtered_world$level == "basin" & filtered_world$vars == "num_hillslopes", "values"] = 1
  # set basin x y z to hillslope x y z
  hill_x = filtered_world[filtered_world$level == "hillslope" & filtered_world$vars == "x", "values"]
  hill_y = filtered_world[filtered_world$level == "hillslope" & filtered_world$vars == "y", "values"]
  hill_z = filtered_world[filtered_world$level == "hillslope" & filtered_world$vars == "z", "values"]
  filtered_world[filtered_world$level == "basin" & filtered_world$vars == "x", "values"] = hill_x
  filtered_world[filtered_world$level == "basin" & filtered_world$vars == "y", "values"] = hill_y
  filtered_world[filtered_world$level == "basin" & filtered_world$vars == "z", "values"] = hill_z

  return(filtered_world)
}

# read_world = function (worldfile, hill_col = F, zone_col = F, patch_col = F) 
# {
#     read_world = readLines(worldfile, warn = FALSE, encoding = "UTF-8")
#     read_world = read_world[nchar(read_world) > 0]
#     world = strsplit(trimws(read_world), "\\s+")
#     world = data.frame(matrix(unlist(world), nrow = length(world), 
#         byrow = T), stringsAsFactors = FALSE)
#     names(world) = c("values", "vars")
#     idvars = c("world_ID", "basin_ID", "hillslope_ID", "zone_ID", 
#         "patch_ID", "canopy_strata_ID")
#     if (any(!idvars %in% world$vars)) {
#         missingid = idvars[!idvars %in% world$vars]
#         stop("Missing or incorrect ID vars in the world file:", 
#             missingid)
#     }
#     index_all = which(world$vars == "world_ID" | world$vars == 
#         "basin_ID" | world$vars == "hillslope_ID" | world$vars == 
#         "zone_ID" | world$vars == "patch_ID" | world$vars == 
#         "canopy_strata_ID")
#     index_names = gsub("_ID", "", x = world$vars[index_all])
#     index_max = c(index_all[2:length(index_all)] - 1, length(world$vars))
#     testing = F
#     if (testing) {
#         tmp = unname(unlist(mapply(rep, index_names, (index_max - 
#             index_all) + 1)))
#         length(tmp)
#         head(tmp, 30)
#     }
#     world$level = unname(unlist(mapply(rep, index_names, (index_max - 
#         index_all) + 1)))
#     world$ID = unname(unlist(mapply(rep, world$values[index_all], 
#         (index_max - index_all) + 1)))
#     if (hill_col) {
#         index_hill = which(world$vars == "hillslope_ID")
#         if (length(index_hill) > 1) {
#             index_hill_max = c(index_hill[2:length(index_hill)] - 
#                 1, length(world$vars))
#         }
#         else {
#             index_hill_max = length(world$vars)
#         }
#         world$hillslope_ID = c(rep(NA, index_hill[1] - 1), unname(unlist(mapply(rep, 
#             world$values[index_hill], (index_hill_max - index_hill) + 
#                 1))))
#     }
#     if (zone_col) {
#         index_zone = which(world$vars == "zone_ID")
#         if (length(index_zone) > 1) {
#             index_zone_max = c(index_zone[2:length(index_zone)] - 
#                 1, length(world$vars))
#         }
#         else {
#             index_zone_max = length(world$vars)
#         }
#         world$zone_ID = c(rep(NA, index_zone[1] - 1), unname(unlist(mapply(rep, 
#             world$values[index_zone], (index_zone_max - index_zone) + 
#                 1))))
#         world$zone_ID[world$level %in% c("basin", "hillslope")] = NA
#     }
#     if (patch_col) {
#         index_patch = which(world$vars == "patch_ID")
#         if (length(index_patch) > 1) {
#             index_patch_max = c(index_patch[2:length(index_patch)] - 
#                 1, length(world$vars))
#         }
#         else {
#             index_patch_max = length(world$vars)
#         }
#         world$patch_ID = c(rep(NA, index_patch[1] - 1), unname(unlist(mapply(rep, 
#             world$values[index_patch], (index_patch_max - index_patch) + 
#                 1))))
#         world$patch_ID[world$level %in% c("basin", "hillslope", 
#             "zone")] = NA
#     }
#     world$unique_ID = unname(unlist(mapply(rep, c(1:length(index_names)), 
#         (index_max - index_all) + 1)))
#     return(world)
# }

# ================================================================================
#' Read stratum veg IDs and hierarchy IDs from a worldfile
#'
#' Fast parser for RHESSys worldfiles that extracts only the hierarchy IDs
#' (`basin_ID`, `hillslope_ID`, `zone_ID`, `patch_ID`, `canopy_strata_ID`) and
#' `veg_parm_ID`, returning one row per stratum (`veg_parm_ID` line).
#'
#' This avoids constructing the full world table and is substantially faster and
#' lower-memory than `read_world()` for this specific use case.
#'
#' @param worldfile Path to a RHESSys worldfile.
#'
#' @return A data frame with columns: `basin_ID`, `hillslope_ID`, `zone_ID`,
#'   `patch_ID`, `stratum_ID`, `veg_parm_ID`.
#' @export
read_world_strata_veg_parm_IDs = function(worldfile) {
  read_world = readLines(worldfile, warn = FALSE, encoding = "UTF-8")
  read_world = trimws(read_world)
  read_world = read_world[nchar(read_world) > 0]

  vars = sub(".*\\s+", "", read_world, perl = TRUE)
  values = sub("\\s+\\S+$", "", read_world, perl = TRUE)

  keep_vars = c("basin_ID", "hillslope_ID", "zone_ID", "patch_ID", "canopy_strata_ID", "veg_parm_ID")
  keep_ind = vars %in% keep_vars
  vars = vars[keep_ind]
  values = values[keep_ind]

  reqvars = c("basin_ID", "hillslope_ID", "zone_ID", "patch_ID", "canopy_strata_ID")
  if (any(!reqvars %in% vars)) {
    missingid = reqvars[!reqvars %in% vars]
    stop("Missing or incorrect ID vars in the world file:", paste(missingid, collapse = ", "))
  }

  n_veg = sum(vars == "veg_parm_ID")
  basin_out = character(n_veg)
  hill_out = character(n_veg)
  zone_out = character(n_veg)
  patch_out = character(n_veg)
  strata_out = character(n_veg)
  veg_out = character(n_veg)

  basin_ID = NA_character_
  hillslope_ID = NA_character_
  zone_ID = NA_character_
  patch_ID = NA_character_
  stratum_ID = NA_character_

  out_i = 0L
  for (i in seq_along(vars)) {
    var_i = vars[i]
    val_i = values[i]

    if (var_i == "basin_ID") {
      basin_ID = val_i
    } else if (var_i == "hillslope_ID") {
      hillslope_ID = val_i
    } else if (var_i == "zone_ID") {
      zone_ID = val_i
    } else if (var_i == "patch_ID") {
      patch_ID = val_i
    } else if (var_i == "canopy_strata_ID") {
      stratum_ID = val_i
    } else if (var_i == "veg_parm_ID") {
      out_i = out_i + 1L
      basin_out[out_i] = basin_ID
      hill_out[out_i] = hillslope_ID
      zone_out[out_i] = zone_ID
      patch_out[out_i] = patch_ID
      strata_out[out_i] = stratum_ID
      veg_out[out_i] = val_i
    }
  }

  out = data.frame(
    basin_ID = basin_out,
    hillslope_ID = hill_out,
    zone_ID = zone_out,
    patch_ID = patch_out,
    stratum_ID = strata_out,
    veg_parm_ID = veg_out,
    stringsAsFactors = FALSE
  )

  out$basin_ID = as.numeric(out$basin_ID)
  out$hillslope_ID = as.numeric(out$hillslope_ID)
  out$zone_ID = as.numeric(out$zone_ID)
  out$patch_ID = as.numeric(out$patch_ID)
  out$stratum_ID = as.numeric(out$stratum_ID)

  return(out)
}

# testing ------------
# out = read_world_strata_veg_parm_IDs(worldfile)
# source("c:/Users/burke/Documents/Repos/rhutils/R/worldfile_utils.R")
# worldfile = "../../PNW/GateCreek/worldfiles/Gatecreek_stable_v3.world"
# fast = system.time(invisible(read_world_strata_veg_parm_IDs(worldfile)))["elapsed"]

# --------- shift vars from soil spun up world to world w 4 msr patches -----------

#' Shift values from a soil spun-up worldfile to a new worldfile with multiple patches
#' 
#' This function takes a soil spun-up worldfile and shifts the values to a new worldfile that has multiple patches. It essentially moves values by patch family ID, and within each patch family, it distributes patch (and strata) values from one patch to many based on vegetation ID. NOT HEAVILY TESTED, WILL HAVE BUGS.
#' @param world_in_path Path to the input worldfile
#' @param world_out_path Path to the output worldfile
#' @param world_output_filename Name of the output worldfile
#' @export

shift_msr_world_vals = function(world_in_path, world_out_path, world_output_filename) {
  cat("This will have bugs, not heavily tested. Use with caution.\n")
  
  # ---------- read worldfiles ----------
  world_in = as.data.table(read_world(worldfile = world_in_path))
  world_out = as.data.table(read_world(worldfile = world_out_path))
  
  # ---------- vars to change ----------
  vars_zonehill = c("gw.storage", "gw.NO3")
  vars_list = c("Ksat_vertical", "mpar", "rz_storage", "unsat_storage", "sat_deficit", "snowpack.water_equivalent_depth", "snowpack.water_depth", "snowpack.T", 
                "snowpack.surface_age", "snowpack.energy_deficit", "litter.cover_fraction","litter.rain_stored", "litter_cs.litr1c", "litter_ns.litr1n", 
                "litter_cs.litr2c", "litter_cs.litr3c", "litter_cs.litr4c", "soil_cs.soil1c", "soil_ns.sminn", "soil_ns.nitrate", "soil_cs.soil2c", "soil_cs.soil3c", 
                "soil_cs.soil4c", "rootzone.depth", "snow_stored", "rain_stored", "cs.cpool", "cs.leafc", "cs.dead_leafc", 
                "cs.leafc_store", "cs.leafc_transfer", "cs.live_stemc", "cs.livestemc_store", "cs.livestemc_transfer", "cs.dead_stemc", "cs.deadstemc_store", 
                "cs.deadstemc_transfer", "cs.live_crootc", "cs.livecrootc_store", "cs.livecrootc_transfer", "cs.dead_crootc", "cs.deadcrootc_store", 
                "cs.deadcrootc_transfer", "cs.frootc", "cs.frootc_store", "cs.frootc_transfer", "cs.cwdc", "epv.prev_leafcalloc", "ns.npool", "ns.leafn", "ns.dead_leafn", 
                "ns.leafn_store", "ns.leafn_transfer", "ns.live_stemn", "ns.livestemn_store", "ns.livestemn_transfer", "ns.dead_stemn", "ns.deadstemn_store", 
                "ns.deadstemn_transfer", "ns.live_crootn", "ns.livecrootn_store", "ns.livecrootn_transfer", "ns.dead_crootn", "ns.deadcrootn_store", 
                "ns.deadcrootn_transfer", "ns.frootn", "ns.frootn_store", "ns.frootn_transfer", "ns.cwdn", "ns.retransn", "epv.wstress_days", "epv.max_fparabs", 
                "epv.min_vwc")
  
  # ---------- check patch and family counts ----------
  in_pID = unique(world_in[world_in$level == "patch", "ID"])
  #in_famID = unique(world_in[world_in$vars == "family_ID", "values"])
  # ^^ DOESNT WORK BECAUSE FAM ID WASNT OUTPUT W WORLD STATE
  in_famID = unique(substr(in_pID$ID, 0, nchar(in_pID$ID) - 2))
  out_pID = unique(world_out[world_out$level == "patch", "ID"])
  out_famID = unique(world_out[world_out$level == "patch" & world_out$vars == "family_ID", "values"])
  cat("          Input | Output\nPatches:", length(in_pID$ID), " | ", length(out_pID$ID), "\nFamilies:", length(in_famID), " | ", length(out_famID$values))
  
  # ---------- add family ID column ----------
  world_in$pID = world_in$ID
  world_in$pID[!(world_in$level == "patch" | world_in$level == "canopy_strata")] = NA
  world_in$pID[!is.na(world_in$pID) & world_in$level == "canopy_strata"] = 
    substr(world_in$pID[!is.na(world_in$pID) & world_in$level == "canopy_strata"], 0, nchar(world_in$pID[!is.na(world_in$pID) & world_in$level == "canopy_strata"]) - 1 )
  world_in$famID = world_in$pID
  world_in$famID[!is.na(world_in$famID)] = substr(world_in$famID[!is.na(world_in$famID)], 0, nchar(world_in$famID[!is.na(world_in$famID)]) - 2 )
  
  world_out$pID = world_out$ID
  world_out$pID[!(world_out$level == "patch" | world_out$level == "canopy_strata")] = NA
  world_out$pID[!is.na(world_out$pID) & world_out$level == "canopy_strata"] = 
    substr(world_out$pID[!is.na(world_out$pID) & world_out$level == "canopy_strata"], 0, nchar(world_out$pID[!is.na(world_out$pID) & world_out$level == "canopy_strata"]) - 1 )
  world_out$famID = world_out$pID
  world_out$famID[!is.na(world_out$famID)] = substr(world_out$famID[!is.na(world_out$famID)], 0, nchar(world_out$famID[!is.na(world_out$famID)]) - 2 )
  
  # ---------- add vegparm column  ----------
  in_vegparm = world_in[world_in$vars == "veg_parm_ID", "values"]
  in_vegparm$values = as.numeric(in_vegparm$values)
  in_vegparm$values[in_vegparm$values > 20] = in_vegparm$values[in_vegparm$values > 20] - 20
  out_patches = unique(world_out[world_out$level == "patch", "ID"])
  in_patches = unique(world_in[world_in$level == "patch", "ID"])
  in_veg_patches = data.table(in_vegparm, in_patches)
  names(in_veg_patches) = c("vegparm", "pID")
  world_in = merge.data.table(world_in, in_veg_patches, by = "pID", all.x = TRUE, sort = F)
  
  out_vegparm = world_out[world_out$vars == "veg_parm_ID", "values"]
  out_vegparm$values = as.numeric(out_vegparm$values)
  out_vegparm$values[out_vegparm$values > 20] = out_vegparm$values[out_vegparm$values > 20] - 20
  out_patches = unique(world_out[world_out$level == "patch", "ID"])
  out_veg_patches = data.table(out_vegparm, out_patches)
  names(out_veg_patches) = c("vegparm", "pID")
  world_out = merge.data.table(world_out, out_veg_patches, by = "pID", all.x = TRUE, sort = F)
  
  # ---------- init world for merge ----------
  world_merge = world_out
  
  # ---------- merge zone vars ----------
  world_merge[world_merge$level %in% c("zone", "hillslope")  & world_merge$vars %in% vars_zonehill, c("level","ID","vars","values")] = 
    merge.data.table(world_out[world_out$level %in% c("zone", "hillslope")  & world_out$vars %in% vars_zonehill, c("vars","level","ID")], 
                     world_in[world_in$level %in% c("zone", "hillslope")  & world_in$vars %in% vars_zonehill, c("values","vars","level","ID")], 
                     by = c("level", "ID", "vars"), sort = F, all.x = T)
  
  # ---------- merge patch+strata vars ----------
  world_merge[!is.na(world_merge$famID) & world_merge$vars %in% vars_list, c("famID", "vegparm", "vars", "level","ID","unique_ID","pID","values")] = 
    merge.data.table(world_out[!is.na(world_out$famID) & world_out$vars %in% vars_list,c("vars","level","ID","unique_ID","famID","pID","vegparm")], 
                     world_in[!is.na(world_in$famID) & world_in$vars %in% vars_list,c("values","vars","famID","vegparm")], 
                     by = c("famID", "vegparm", "vars"), sort = F, all.x = T)
  
  # ---------- Write output world ----------
  write_world(world_merge, world_output_filename)
  
  return("Success!")
}