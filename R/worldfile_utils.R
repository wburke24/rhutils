# worldfile utils

# ================================================================================
# add patch areas based on worldfile
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
# aggregate to patch family using area weights
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