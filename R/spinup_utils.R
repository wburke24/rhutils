# fun_spinup


# add level index to worldfile dataframe
#' @export
world_add_level_i = function(world) {
  # add numeric level col
  world$level_i = world$level
  l = c("world","basin","hillslope","zone","patch","canopy_strata")
  li = c(1,2,3,4,5,6)
  world[.(from = l, to = li), on = paste0("level_i", "==from"), ("level_i") := i.to]
  world$level_i = as.numeric(world$level_i)
  world$i = 1:length(world$vars)
  return(world)
}

# add vegparmID col to worldfile dataframe - IF there are more than 1 strata, vegID is only or strata level
#' @export
world_add_patch_vegparmIDcol = function(world) {
  if (!"patch_ID" %in% names(world)) {
    index_patch = which(world$vars == "patch_ID")
    if (length(index_patch) > 1) {
      index_patch_max = c(index_patch[2:length(index_patch)] - 1, length(world$vars))
    } else {
      index_patch_max = length(world$vars)
    }
    world$patch_ID = c(rep(NA, index_patch[1]-1) ,unname(unlist(mapply(rep, world$values[index_patch], (index_patch_max - index_patch) + 1 ))))
    world$patch_ID[world$level %in% c("basin", "hillslope","zone")] = NA
  }

  # get unique vegID combinations by patch
  world = as.data.table(world)
  vegparm_by_patch <- world[world$vars == "veg_parm_ID", .(values, patch_ID)][, .(veg_parm_ID = paste(values, collapse = "_")), by = patch_ID]

  world = merge.data.table(world, vegparm_by_patch, by = "patch_ID", all.x = TRUE, sort = F)
  return(as.data.frame(world))
}

# add family ID and rule ID, will cover the strata and patches
#' @export
world_add_familyID_RuleID = function(world) {
  if (any(world$vars == "family_ID")) {
    if (is.null(world$patch_ID)) {
      stop("add patch ID first")
    }
    world$sort_index = seq_len(nrow(world))
    
    pfams = world[world$vars == "family_ID", c("patch_ID","values")]
    names(pfams)[2] = "family_ID"
    world = merge(world, pfams, by = "patch_ID", sort = F, all = T)

    if (any(world$vars == "asp_rule")) {
      rules = world[world$vars == "asp_rule",c("family_ID","values")]
      names(rules)[2] = "rule_ID"
      rules = unique(rules)
      world = merge(world, rules, by = "family_ID", sort = F, all = T)
    } else {
      cat("No asp_rule found in worldfile, omitting.")
    }

    world = world[order(world$sort_index), ]
    world$sort_index = NULL
    row.names(world) = NULL

    return(world)

  } else {
    cat("No family_ID found in world")
    return(NULL)
  }

}

# extract a world based on a target unique index ID
#' @export
extract_world = function(world, target_unique_ID) {

  if ("level_i" %in% names(world)) {
    cat("level_i already present in world\n")
  } else {
    cat("adding level_i to world\n")
    world = world_add_level_i(world)
  }
  
  # use data.table for easier subsetting
  world = as.data.table(world)

  # use to look up parent/child levels
  IDworld = world[
    which(world$vars == "world_ID" | world$vars == "basin_ID" | world$vars == "hillslope_ID" |
            world$vars == "zone_ID" | world$vars == "patch_ID" | world$vars == "canopy_strata_ID"), 
    c("ID", "unique_ID", "level_i", "i")]
  
  # c("patch_ID", "ID", "unique_ID", "level_i", "veg_parm_ID", "i")
  
  target = IDworld[unique_ID == target_unique_ID,]
  # Parents should only be single levels, ie if target is a patch, you only need the single encompassing zone
  l_parent = c(1:6)[c(1:6) < target$level_i]
  unid_parent = rep(NA, length(l_parent))
  for (l in l_parent) {
    unid_parent[l] = IDworld[level_i == l, ][i < target$i,][which.min(abs(target$i - i)), "unique_ID"]
  }
  # child could be multiple units, need everything until the next element at the same level as target
  l_child = c(1:6)[c(1:6) > target$level_i]
  unid_child = list()
  max_i = IDworld[IDworld$level_i == (target$level_i) & i > target$i, min(i)]
  for (l in  seq_along(l_child)) {
    unid_child[l] = IDworld[level_i == l_child[l], ][i > target$i & i < max_i, "unique_ID"]
  }
  unid_extract = sort(c(unlist(unid_parent), target_unique_ID, unlist(unid_child)))
  world_extract = world[unique_ID %in% unid_extract,]
  IDworld_extract = IDworld[IDworld$unique_ID %in% unid_extract, ]
  # correct number of sub units and zone areas
  IDworld_extract$ct = NA
  zoneareas = data.table(old = world_extract[world_extract$level == "zone" & world_extract$vars == "area", values])
  zoneareas$new = NA
  zi = 0
  for (ind in which(IDworld_extract$level_i < 6)) {
    max_i = IDworld_extract[IDworld_extract$level_i == IDworld_extract$level_i[ind] & i > IDworld_extract$i[ind], suppressWarnings(min(i))]
    IDworld_extract$ct[ind] = length(IDworld_extract[IDworld_extract$level_i == (IDworld_extract$level_i[ind] + 1),][i > IDworld_extract$i[ind] & i < max_i, i])
    if (IDworld_extract$level_i[ind] == 4) {
      zi = zi + 1
      zoneareas$new[zi] = sum(as.numeric(world_extract[world_extract$vars == "area" & 
                                                         world_extract$level_i == (IDworld_extract$level_i[ind] + 1),][i > IDworld_extract$i[ind] & i < max_i, values]))
    }
  }
  world_extract$values[grepl("^num_\\w", world_extract$vars)] = as.character(IDworld_extract$ct[which(IDworld_extract$level_i < 6)])
  world_extract[world_extract$level == "zone" & world_extract$vars == "area", "values"] = as.character(zoneareas$new)
  
  return(world_extract)
}

# extract a world based on a target unique index ID
#' @export
pfam_extract_world = function(familyID, world) {
  # assume/check - world has familyID, familyID cover patch and strata
  # also assume add check, for hillslope and zone cols
  # only need parent levels, and only need to correct those
  cat("Assumes world has zone, hill, patch family, and patch ID cols, and using datatable\n")
  # data.table functions accessed via ::
  world = as.data.table(world)

  # this is patches and strata that match the patch family
  target = world[family_ID == familyID,]

  if (nrow(target) == 0) {
    stop("Something went wrong: no patches found for familyID:", familyID)
  }

  # use unique IDs for simplicity
  target_uniqID = unique(target$unique_ID)
  
  # find the zone, hillslope, and basin+world that contain the patch fam
  # world + basin
  target_world_basin_uniqID = unique(world[world$level %in% c("world","basin"), unique_ID])

  # for zone and hill, use index/number of first patch and work backwards until the preceeding zone and hill
  allzone_uniqIDs = unique(world[world$level == "zone", unique_ID])
  target_zone_uniqID = max(allzone_uniqIDs[allzone_uniqIDs < min(target_uniqID)])
  #hillslope
  allhill_uniqIDs = unique(world[world$level == "hillslope", unique_ID])
  target_hill_uniqID = max(allhill_uniqIDs[allhill_uniqIDs < min(target_uniqID)])

  if (length(unique(target$zone_ID)) > 1 || length(unique(target$hillslope_ID)) > 1) {
    stop("This code only worls for 1 zone 1 hillslope ")
  }

  world_extract = world[unique_ID %in% c(target_world_basin_uniqID,target_hill_uniqID,target_zone_uniqID, target_uniqID),]
  
  # fix zone area - should already be correct, but double check
  areasdf = world_extract[world_extract$vars == "area", ]
  if (as.numeric(areasdf[areasdf$level == "zone", values]) != sum(as.numeric(areasdf[areasdf$level == "patch", values])) ) {
    world_extract[world_extract$vars == "area" & world_extract$level == "zone", "values"] = sum(as.numeric(areasdf[areasdf$level == "patch", values]))
  }
  #fix zone ct, probably fine also
  if (as.numeric(world_extract[world_extract$vars == "num_patches", values]) != length(unique(world_extract[world_extract$level=="patch",patch_ID])) ) {
    world_extract[world_extract$vars == "num_patches", "values"] != length(unique(world_extract[world_extract$level=="patch",patch_ID]))
  }
  
  # fix hill zone ct
  world_extract[world_extract$vars == "num_zones", "values"] = length(unique(world_extract[world_extract$level=="zone","zone_ID"]))
  world_extract[world_extract$vars == "num_hillslopes", "values"] = length(unique(world_extract[world_extract$level=="hillslope","hillslope_ID"]))
  
  return(world_extract)
}

#' @export
select_pfam = function(rule, world) {
  tmp = as.numeric(as.data.frame(world[world$rule_ID == rule & world$vars == "z","values"])$values) 
  medzi = which.min(abs(tmp - median(tmp)))
  # get patch fam of first patch w this elevation
  famid = world[world$rule_ID == rule & world$vars == "z", "family_ID"][medzi]
  return(famid)
}

# # ASSUMES 1 HILLSLOPE
#' @export
make_1pflow = function(world) {
  hill = unique(world$ID[world$level == "hillslope"])
  patch = unique(world$pID[!is.na(world$pID)])
  zone = unique(world$ID[world$level == "zone"])
  xcen = world$values[world$level == "patch" & world$vars == "x"]
  ycen = world$values[world$level == "patch" & world$vars == "y"]
  zcen = world$values[world$level == "patch" & world$vars == "z"]
  area = world$values[world$level == "patch" & world$vars == "area"]
  flow_out = paste0(length(hill),"\n",
                    hill, "\t", length(patch),"\n",
                    patch, "\t", zone, "\t", hill, "\t", xcen, "\t", ycen, "\t", zcen, "\t", area, "\t", area, "\t", 1, "\t", area, "\t", 0)
  return(flow_out)
}

#' @export
make_1pfamflow = function(world) {

  hill = unique(world$ID[world$level == "hillslope"])
  patch = unique(world$pID[!is.na(world$pID)])
  zone = unique(world$ID[world$level == "zone"])
  xcen = world$values[world$level == "patch" & world$vars == "x"]
  ycen = world$values[world$level == "patch" & world$vars == "y"]
  zcen = world$values[world$level == "patch" & world$vars == "z"]
  area = world$values[world$level == "patch" & world$vars == "area"]
  areapct = as.numeric(area)/sum(as.numeric(area))

  # treat as stream patch
  patch_out = paste0(patch, "\t", zone, "\t", hill, "\t", xcen, "\t", ycen, "\t", zcen, "\t", area, "\t", area, "\t", 1, "\t", area, "\t", 0)
  
  flow_out = paste0(length(hill),"\n",
                    hill, "\t", length(patch),"\n",
                    paste0(patch_out, collapse = "\n"))
 
  return(flow_out)
}


#' @export
# selecting patches based on veg parm IDs and median cover + elevation, using unique ID to extract below
select_mean_vegID_patch = function(x, world) {
  cat("Selecting patch for veg_parm_ID:", x, "\n")
  cat("Using median elevation and cover fraction to select patch\n")
  cat("This is for STANDARD (non MSR) worldfiles\n")
  # USE: patch_selected_uniqueIDs = sapply(veg, select_mean_vegID_patch, world = world)
  subset = world[world$veg_parm_ID == x & !is.na(world$veg_parm_ID), ]
  elev = as.numeric(subset[subset$vars == "z", "values"])
  cover = as.numeric(subset[subset$vars == "cover_fraction", "values"])
  med_elev = median(elev)
  med_cover = median(cover)
  # find closest to median
  dist = sqrt((elev - med_elev)^2 + (cover - med_cover)^2)
  return(subset$unique_ID[which.min(dist)])
}

#' @export
# selecting patches based on rule IDs and median cover + elevation, using family ID to extract below
select_familyID_per_vegID_MSR = function(x, world) {
  cat("Selecting patch FAMILY ID for rule_ID:", x, "\n")
  cat("\tUsing median elevation and cover fraction to select patch\n")
  cat("\tThis is for MSR (not standard) worldfiles\n")
  # USE: patch_selected_uniqueIDs = sapply(veg, select_mean_vegID_patch, world = world)
  subset = world[world$rule_ID == x & !is.na(world$rule_ID), ]

  elev = as.numeric(subset[subset$vars == "z", "values"])
  cover = as.numeric(subset[subset$vars == "cover_fraction", "values"])
  med_elev = median(elev)
  med_cover = median(cover)
  cat("\tMedian Elevation:", med_elev,
      "\n\tMedian Cover Fraction:", med_cover, "\n")
  # find closest to median
  dist = sqrt((elev - med_elev)^2 + (cover - med_cover)^2)
  return(subset$family_ID[which.min(dist)])
}

#' @export

new_world_from_spun_worlds = function(spun_world_paths, original_world_path, ID = "rule_ID") {
  # data.table functions accessed via ::
  
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

  cat("Combining ", length(spun_world_paths), "spun worlds to create new world with spun soil nutrients\n")

  # ==================== read spun and original worlds ====================
  worlds_spun = lapply(spun_world_paths, FUN = function(X){as.data.table(read_world(X, patch_col = T))})
  worlds_spun = lapply(worlds_spun, world_add_patch_vegparmIDcol)
  world_dest = as.data.table(read_world(original_world_path, patch_col = T))
  world_dest = world_add_patch_vegparmIDcol(world_dest)
  
  worlds_spun = lapply(worlds_spun, as.data.table )
  world_dest = as.data.table(world_dest)

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
  
  worlds_spun = lapply(worlds_spun, as.data.table )
  world_dest = as.data.table(world_dest)

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