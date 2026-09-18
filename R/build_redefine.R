#' build_redefine
#' 
#' Creates a redefine world file, based on input worldfile and user specified variables to redefine. Can either specify variables and values to replace, or a standard thinning value to apply to a set of predefined carbon storage variables. Optionally can filter by patchID, strataID, and veg_parm_ID.
#' @param worldfile Path to worldfile or worldfile data.frame
#' @param out_file Path to output redefine worldfile
#' @param vars Variable names to redefine
#' @param values Values to assign to variables
#' @param thinning_redef_value Thinning redefine value to apply to a set of predefined carbon storage variables. If specified, vars and values are ignored.
#' @param patchID Patch IDs to filter
#' @param strataID Strata IDs to filter
#' @param veg_parm_ID Vegetation parameter IDs to filter
#' @param redef_in Input redefine file to add to
#' @param add Add to existing redefine file (TRUE/FALSE)
#' @export

build_redefine = function(worldfile,
                          out_file = NULL,
                          vars = NULL,
                          values = NULL,
                          thinning_redef_value = NULL,
                          patchID = NULL,
                          strataID = NULL,
                          veg_parm_ID = NULL,
                          redef_in = NULL,
                          add = FALSE) {

  # ==================== Check Arguments ====================
  if (!file.exists(worldfile)) { stop(noquote(paste0("No file found at: ", worldfile))) }
  if (!is.null(out_file) && file.exists(out_file)) {cat("File:",out_file,"will be overwritten.\n")}
  # Either need vars + values or thinning_redef_value
  if ((is.null(vars) || is.null(values)) && is.null(thinning_redef_value)) {
    stop("Input is required for both `vars` and `values`, or `thinning_redef_value (which uses a preset subset of vars to redefine).`")
  }
  # if using vars + values - values must either be length of vars or 1
  if ((!is.null(vars) & !is.null(values)) && length(vars) != length(values) && length(values) != 1) {
    stop(sprintf(
      "Input length mismatch: %d input `vars` and %d input `values`. `length(values) == length(vars)` or `length(values) == 1`.",
      length(vars), length(values)
    ))
  }
    if (!is.null(redef_in) && !add) {
    stop("Input redefine file provided but add = FALSE. If you want to add to an existing redefine file, set add = TRUE.")
  }
  if (add && is.null(redef_in)) {
    stop("add = TRUE but no redefine file provided in redef_in arg.")
  }

  # Report operation type
  if (!is.null(thinning_redef_value)) {
    cat("Mode: Creating new thinning worldfile redefine (", thinning_redef_value, ")\n", sep = "")
  } else if (add) {
    cat("Mode: Adding to existing worldfile redefine\n")
  } else {
    cat("Mode: Creating new worldfile redefine\n")
  }

  # ==================== Read and Parse Worldfile ====================
  if (is.character(worldfile) && length(worldfile) == 1) {
    # read to be used for final line by line replacement
    world = RHESSysPreprocessing::read_world(worldfile, patch_col = TRUE)
  } else if (is.data.frame(worldfile)) {
    world = worldfile
  } else {
    stop("Input worldfile must be either a path to a worldfile or a worldfile data.frame")
  }
  if (sum(world$values == "-9999") > 2) {
    cat("Worldfile already contains -9999 values, if this is a redefine world you are adding to, set add=T and input redefine dataframe as the redef_in arg.\n")
  }

  # ==================== Add veg_parm_ID Columns if Missing ====================
  if (!"veg_parm_ID" %in% names(world)) {
    world = world_add_patch_vegparmIDcol(world)
    if (any(grepl("_", world$veg_parm_ID))) {
      world$veg_parm_ID1 = as.numeric(sapply(strsplit(world$veg_parm_ID, "_"), function(x) x[1]))
      world$veg_parm_ID2 = as.numeric(sapply(strsplit(world$veg_parm_ID, "_"), function(x) {if(length(x) > 1) {x[2]} else {NA}}))
      world$veg_parm_ID = NULL
      world = world_add_var_as_col(world, "veg_parm_ID")
    }
  }
  # - If veg_parm_ID2 exists, there are 2 vegparm per patch. 
  # - Strata level selection will depend on 'veg_parm_ID' but patch will depend on 'veg_parm_ID1' and 'veg_parm_ID2'. 
  # - If only 1 vegparm per patch, then strata and patch will depend on 'veg_parm_ID'.

  # ==================== Apply Thinning Redefine Value ====================
  if (!is.null(thinning_redef_value)) {
    # thinning vars - all strata level
    vars <- thin_vars <- c(
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
    values <- rep.int(thinning_redef_value, length(thin_vars))
  }

  # if only 1 value given but multiple vars to replace, repeat value
  if (length(vars) > 1 && length(values) == 1) {
    values = rep.int(values, length(vars))
  }
  
  # ==================== Filter & Subset Worldfile ====================
  # Initialize filter masks; all TRUE means "include all" unless filter is specified
  n_rows = length(world$vars)
  redef_strata = redef_veg_strata = redef_veg_patch = redef_patch = rep.int(TRUE, n_rows)

  redef_index_byvar = list()

  # ==================== Determine Variable Levels ====================
  var_levels <- vapply(vars,
    function(x) {
      y <- unique(world$level[world$vars %in% x])
      if (length(y) != 1) {
        stop(paste("Var", x, "has multiple levels:", paste(y, collapse = ", "), "and cannot be redefined currently."))
      } else {
        return(y)
      }
    },
    FUN.VALUE = character(1)
  )
  # var_levels = sapply(vars, function(x) unique(world$level[world$vars %in% x]))

  # ----- Patch -----
  if (!is.null(patchID)) { # this only works if changing patch vars
    redef_patch = world$patch_ID %in% as.character(patchID)
  }
  # ----- Strata -----
  if (!is.null(strataID)) {
    redef_strata = world$level == "canopy_strata" & world$ID %in% as.character(strataID)
  }
  # ----- Veg Parm ID -----
  if (!is.null(veg_parm_ID) && "canopy_strata" %in% var_levels) {
    redef_veg_strata = world$veg_parm_ID %in% as.character(veg_parm_ID)
  }
  if (!is.null(veg_parm_ID) && "patch" %in% var_levels) {
    if ("veg_parm_ID2" %in% names(world)) {
      redef_veg_patch = world$veg_parm_ID1 %in% as.character(veg_parm_ID) | world$veg_parm_ID2 %in% as.character(veg_parm_ID)
    } else {
      redef_veg_patch = world$veg_parm_ID %in% as.character(veg_parm_ID)
    }
  }
  
  # ==================== Find and Replace Vars ====================
  for (i in seq_along(vars)) {
    var_level = var_levels[vars[i]]
    if (!is.null(veg_parm_ID)) {
      if (var_level == "canopy_strata") {
        redef_veg = redef_veg_strata
      } else if (var_level == "patch") {
        redef_veg = redef_veg_patch
      } else {
        stop(paste("Var", vars[i], "is not at patch or canopy_strata level, cannot redefine using veg_parm_ID filter."))
      }
    } else {
      redef_veg = rep.int(TRUE, length(world$vars))
    }

    replace_index = which(redef_patch & redef_strata & redef_veg & (world$vars == vars[i]))
    redef_index_byvar[[i]] = replace_index

    if (length(replace_index) == 0) {stop(noquote(paste("Var", vars[i], "to replace can't be found in worldfile.\n"))) }
    if (length(replace_index) > 1) {
      new_value = rep(values[i], length(replace_index))
    } else {
      new_value = values[i]
    }
    # replace
    world$values[replace_index] = new_value
  }

  # ==================== Check For Matches ====================
  redef_index_byvar = unlist(redef_index_byvar)
  if ((is.null(redef_index_byvar) || length(redef_index_byvar) == 0) ) {
    # cat("No vars matched criteria, all set to -9999.\n")
    stop("No vars matched criteria, aborting.\n")
  }
  dput(sort(2^c(1:20), T))
  # Report filter summary
  cat(sprintf("  Found %d rows to redefine across %d variables\n", 
              length(redef_index_byvar), length(vars)))

  # ==================== Create new or modify Redefine ====================
  # *** Set unmatched values to -9999 ***
  if (add) {
    cat("Adding to existing redefine file values. Assumes undefined values are already -9999\n")
    # some basic checks that the input redefine and worldfile have the same structure
    if (!all(world$vars == redef_in$vars)) {
      stop("The input redefine file and the worldfile do not have the same structure.")
    }
    redef_in$values[redef_index_byvar] = world$values[redef_index_byvar]
    world = redef_in
  } else {
    cat("Replacing all non-redefined values with -9999.\n")
    # ---------- Replace all other values w -9999 ----------
    # Structural/connectivity variables that must remain to maintain worldfile integrity;
    # all other vars will be set to -9999 unless explicitly redefined above
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
    keep_index = c(unique(c(redef_index_byvar)), which(world$vars %in% keep_vars))
    keep_index = sort(keep_index)
    no_change_vars = c(1:nrow(world))[-keep_index]
    world$values[no_change_vars] = "-9999"
    
    # Summary before output
    n_redef = sum(world$values != "-9999")
    cat(sprintf("  Total redefined values: %d\n", n_redef))
  }

  # ==================== Write File ====================
  if (!is.null(out_file)) {
    write_world(world = world, path = out_file)
    cat("Successfully wrote redefine worldfile to",out_file,"\n")
  } else {
    cat("Successfully modified redefine worldfile and returned within R\n")
    return(world)
  }

}
