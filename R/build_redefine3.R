#' build_redefine3
#' 
#' One more try better now
#' @export
build_redefine3 = function(worldfile, out_file = NULL, vars = NULL, values = NULL, std_thin = NULL, patchID = NULL, strataID = NULL, veg_parm_ID = NULL, add = FALSE) {
  # ---------- Check Arguments ----------
  # Either need vars + values or std_thin
  if ((is.null(vars) | is.null(values)) & is.null(std_thin)) {
    stop(cat("Input is required for both `vars` and `values`, or `std_thin`"))
  }
  # if using vars + values - values must either be length of vars or 1
  if ((!is.null(vars) & !is.null(values)) && length(vars) != length(values) && length(values) != 1) {
    stop(cat("Input length mismatch:", length(vars), "input `vars` and", length(values),
             "input `values`. `length(values) == length(vars)` or `length(values) == 1`.\n"))
  }

  # read and parse
  if (is.character(worldfile) & length(worldfile) == 1) {
    # read to be used for final line by line replacement
    world = RHESSysPreprocessing::read_world(worldfile)
  } else {
    world = worldfile
  }

  if (sum(world$values == "-9999") > 2 & !add) {
    cat("Worldfile already contains -9999 values, if this is a redefine world you are adding to, set add=T, otherwise all undefined values (from this function call) will be set to -9999\n")
  }

  # thinning vars
  thin_vars =  c(
    "cs.cpool",
    "cs.leafc",
    "cs.dead_leafc",
    "cs.live_stemc",
    "cs.dead_stemc",
    "cs.live_crootc",
    "cs.dead_crootc",
    "cs.frootc"
  )
  # other_thin_vars = c("cover_fraction", "gap_fraction", "cs.stem_density") # not currently thinning these
  if (!"veg_parm_ID" %in% names(world)) {
    world = world_add_patch_vegparmIDcol(world)
  }

  redef_strata = rep.int(TRUE, length(world$vars))
  redef_veg_strata = rep.int(TRUE, length(world$vars))
  redef_patch = rep.int(TRUE, length(world$vars))
  
  # ---------- Thinning redefine ----------
  redef_index = NULL
  if (!is.null(std_thin)) {
    # filter by veg parm, strata, patch if given ================
    if (!is.null(patchID)) { # this only works if changing patch vars
      redef_patch = world$patch_ID %in% as.character(patchID)
    }
    if (!is.null(strataID)) {
      # functionality to support using just 1 or 2
      # if (all(nchar(strataID) == 1) & all(nchar(unique(world$ID[world$level == "canopy_strata"])) > 1)) {
      #   redef_strata = strata_IDs[substr(strata_IDs, nchar(strata_IDs), nchar(strata_IDs)) == as.character(strataID)]
      # }
      redef_strata = world$level == "canopy_strata" & world$ID %in% as.character(strataID)
    }
    if (!is.null(veg_parm_ID)) {
      redef_veg_strata = world$veg_parm_ID %in% as.character(veg_parm_ID)
    }
    redef_index = which(redef_patch & redef_strata & redef_veg_strata & (world$vars %in% thin_vars))
    if (length(redef_index) == 0) {
      redef_index = NULL
      cat("No vars to redefine found in worldfile for std thin querry\n")
    }
    redef_values = as.character(rep.int(std_thin, length(redef_index)))
    if (!is.null(redef_index)) {
      world$values[redef_index] = redef_values
    }
  }

  # ---------- Find and Replace Vars ----------
  replace_index_all = NULL
  if (!is.null(vars) & !is.null(values)) {
    # if only 1 value given but multiple vars to replace, repeat value
    if (length(vars) > 1 & length(values) == 1) {
      values = rep.int(values, length(vars))
    }
    # filter by veg parm, strata, patch if given ================
    if (!is.null(patchID)) { # this only works if changing patch vars
      redef_patch = world$patch_ID %in% as.character(patchID)
    }
    if (!is.null(strataID)) {
      redef_strata = world$level == "canopy_strata" & world$ID %in% as.character(strataID)
    }
    if (!is.null(veg_parm_ID)) {
      redef_veg_strata = world$veg_parm_ID %in% as.character(veg_parm_ID)
    }
    replace_index_all = which(redef_patch & redef_strata & redef_veg_strata & (world$vars %in% vars))
    
    for (i in 1:length(vars)) {
      replace_index = which(redef_patch & redef_strata & redef_veg_strata & (world$vars %in% vars[i]))

      if (length(replace_index) == 0) {stop(noquote("var to replace can't be found in worldfile.\n")) }
      # if unique values for every instance of var to be replaces were given, do nothing, otherwise repeat to get enough replacement values
      if (length(values[i]) != length(replace_index)) {
        new_value = rep(values[i], length(replace_index)/length(values[i]))
      } else {
        new_value = values[i]
      }
      # replace
      if (!is.null(replace_index)) {
        world$values[replace_index] = new_value
      }
    }
  }

  if ( (is.null(redef_index) || all(!redef_index)) & (is.null(replace_index_all) || all(!replace_index_all)) ) {
    cat("No vars matched criteria, all set to -9999.\n")
  }

  if (add) {
    cat("Adding to existing redefine file values. Assumes undefined values are already -9999\n")
  } else {
    cat("Replacing all non-redefined values with -9999.\n")
    # ---------- Replace all other values w -9999 ----------
    keep_vars = c(
      "world_ID",
      "basin_ID",
      "hillslope_ID",
      "zone_ID",
      "patch_ID",
      "canopy_strata_ID",
      "num_basins",
      "num_hillslopes",
      "num_zones",
      "num_patches",
      "num_canopy_strata",
      "num_stratum",
      "basin_n_basestations",
      "basin_basestation_ID",
      "hillslope_n_basestations",
      "hillslope_basestation_ID",
      "zone_n_basestations",
      "zone_basestation_ID",
      "patch_n_basestations",
      "patch_basestation_ID",
      "canopy_strata_n_basestations",
      "canopy_strata_basestation_ID"
    )
    keep_index = c(unique(c(redef_index, replace_index_all)), which(world$vars %in% keep_vars))
    keep_index = sort(keep_index)
    no_change_vars = c(1:nrow(world))[-keep_index]
    world$values[no_change_vars] = "-9999"
  }

  # ---------- Write file ----------
  if (!is.null(out_file)) {
    write_world(world = world, path = out_file)
    cat("Successfully wrote redefine worldfile to",out_file,"\n")
  } else {
    cat("Successfully modified redefine worldfile and retuned within R\n")
    return(world)
  }

}
