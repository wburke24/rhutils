# worldfile utils

# ================================================================================
# add patch areas based on worldfile
#' @export
add_patch_areas = function(output_df, world_source) {
  if ("area" %in% names(output_df)) {
    cat("output_df already contains 'area' column.")
    return(output_df)
  }
  world = read_world(world_source)
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
  world = read_world(world_source, patch_col = T)
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
  world = read_world(world_source)
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
