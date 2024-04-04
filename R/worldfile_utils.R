# worldfile utils

# ================================================================================
# add patch areas based on worldfile
#' @export
add_patch_areas = function(df, world_source) {
  if ("area" %in% names(df)) {
    return("df already contains 'area' column.")
  }
  world = read_world(world_source)
  patchareas = world[world$level == "patch" & world$vars == "area",c("values","ID")]
  names(patchareas) = c("area", "patchID")
  patchareas$patchID = as.numeric(patchareas$patchID)
  patchareas$area = as.numeric(patchareas$area)
  # do some checking first
  if (!length(unique(patchareas$patchID)) == length(unique(df$patchID)) | !all(patchareas$patchID %in% df$patchID)) {
    return("Patch IDs in the data did not match IDs in the worldfile")
  }
  df = df[patchareas, on = .(patchID)]
  return(df)
}

# ================================================================================
# add patch family ID and area
#' @export
add_patchfamily_IDarea = function(df) {
  if ("familyID" %in% names(df) & "family_area" %in% names(df)) {
    return("df already contains 'familyID' and 'family_area' columns.")
  }
  df$familyID = floor(df$patchID/100)
  family_area = df[,sum(area), by = c("familyID", "year","month")]
  names(family_area)[4] = "family_area"
  df = df[family_area, on = .(familyID, year, month)]
  df$area_prop = df$area/df$family_area
  return(df)
}

# ================================================================================
# aggregate to patch family using area weights
#' @export
patches2patchfamily = function(df) {
  aggvars = names(df)[!names(df) %in% c("day", "month", "year", "basinID", "hillID", "zoneID", "patchID", "area", "familyID", "family_area", "area_prop")]
  df = df[,(aggvars) := .SD*area_prop, .SDcols = aggvars]
  # aggvars2 = names(df)[names(df) %in% c("basinID", "hillID", "zoneID", "familyID", "year","wy","yr_mn","month","day")]
  df_out = df[,lapply(.SD, sum) ,by = c("basinID", "hillID", "zoneID", "familyID", "year","month","day"), .SDcols = c(aggvars,"area")]
  return(df_out)
}
