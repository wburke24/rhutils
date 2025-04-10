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
  # ---------- add patch and vegparm columns ----------
  world$pID = world$ID
  world$pID[!(world$level == "patch" | world$level == "canopy_strata")] = NA
  world$pID[!is.na(world$pID) & world$level == "canopy_strata"] = 
    substr(world$pID[!is.na(world$pID) & world$level == "canopy_strata"], 0, nchar(world$pID[!is.na(world$pID) & world$level == "canopy_strata"]) - 1 )
  
  in_vegparm = world[world$vars == "veg_parm_ID", "values"]
  in_patches = unique(world[world$level == "patch", "ID"])
  in_veg_patches = data.table(in_vegparm, in_patches)
  names(in_veg_patches) = c("vegparm", "pID")
  world = merge.data.table(world, in_veg_patches, by = "pID", all.x = TRUE, sort = F)
  return(world)
}

# add family ID and rule ID, will cover the strata and patches
#' @export
world_add_familyID_RuleID = function(world) {
  if (any(world$vars == "family_ID")) {
    if (is.null(world$patch_ID)) {
      stop("add patch ID first")
    }

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

    return(world)

  } else {
    cat("No family_ID found in world")
    return(NULL)
  }

}

# extract a world based on a target unique index ID
#' @export
extract_world = function(world, target_unique_ID) {
  # use to look up parent/child levels
  IDworld = world[
    which(world$vars == "world_ID" | world$vars == "basin_ID" | world$vars == "hillslope_ID" |
            world$vars == "zone_ID" | world$vars == "patch_ID" | world$vars == "canopy_strata_ID"), 
    c("pID", "ID", "unique_ID", "level_i", "vegparm", "i")]
  
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

  # this is patches and strata that match the patch family
  target = world[family_ID == familyID,]
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
  tmp = as.numeric(as.data.frame(world[world$rule_ID == rule & vars == "z","values"])$values) 
  medzi = which.min(abs(tmp - median(tmp)))
  # get patch fam of first patch w this elevation
  famid = world[world$rule_ID == rule & vars == "z", family_ID][medzi]
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
