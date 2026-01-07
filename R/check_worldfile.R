#' check_worldfile
#'
#' Read in a worldfile and output key info on that worldfile.
#' @export

check_worldfile = function(worldfile) {
  # check that worldfile exists
  if (!file.exists(worldfile)) {
    stop("worldfile does not exist: ",worldfile,"\n")
  }
  # read in worldfile
  world = RHESSysPreprocessing::read_world(worldfile)

  format_ids = function(ids) {
    if (length(ids) > 20) {
      return(paste0("[",length(ids)," total] ", paste0(ids[1:20], collapse = " "),"..." ) )
    } else {
      return(paste0("[",length(ids)," total] ", paste0(ids, collapse = " ")))
    }
  }

  # get unique basins, hillslopes, zones, patches
  basins = unique(world$ID[world$level=="basin"])
  hills = unique(world$ID[world$level=="hillslope"])
  zones = unique(world$ID[world$level=="zone"])
  patches = unique(world$ID[world$level=="patch"])
  # get strata counts
  strata_1 = nrow(world[world$vars == "num_canopy_strata" & world$values == 1, ])
  strata_2 = nrow(world[world$vars == "num_canopy_strata" & world$values == 2, ])
  
  # get patch sizes
  patchsizes = unique(world$values[world$vars == "area" & world$level=="patch"])
  psizes = summary(factor(world$values[world$vars == "area" & world$level=="patch"]))
  psizesdf = data.frame(area_sq_m = names(psizes), count = unname(psizes))
  psizesdf = psizesdf[order(psizesdf$count, decreasing = T),]
  rownames(psizesdf) = seq_len(nrow(psizesdf))
  if (nrow(psizesdf) > 20) {
    psizesdf = psizesdf[1:20,]
  }

  # check for family IDs
  famIDs = world$values[world$vars == "family_ID" & world$level=="patch"]
  if (length(famIDs) > 0) {
    famct = table(factor(famIDs))
    famidct = data.frame(table(factor(famct)))
    names(famidct) = c("Patches in family","Count")
    families = unique(famIDs)
  }

  var_stats = data.frame(variable = NA, min = NA, max = NA, mean = NA)

  # check horizons in degrees - values are in sin(radians) so convert back with asin(x) * 180/pi
  whors = as.numeric(world$values[world$vars == "w_horizon"])
  ehors = as.numeric(world$values[world$vars == "e_horizon"])
  # test check
  # c("w_horizon raw", min(whors), max(whors), mean(whors))
  # c("e_horizon raw", min(ehors), max(ehors), mean(ehors))
  whors_deg = asin(whors) * 180 / pi
  ehors_deg = asin(ehors) * 180 / pi

  # CHECK: COVER FRACT, GAP FRAC, SLOPE, ASPECT
  cover_fracs = as.numeric(world$values[world$vars == "cover_fraction"])
  var_stats[1,] = c("cover_fraction", min(cover_fracs), max(cover_fracs), mean(cover_fracs))
  gap_fracs = as.numeric(world$values[world$vars == "gap_fraction"])
  var_stats[2,] = c("gap_fraction", min(gap_fracs), max(gap_fracs), mean(gap_fracs))
  slope = as.numeric(world$values[world$vars == "slope"])
  var_stats[3,] = c("slope", min(slope), max(slope), mean(slope))
  aspect = as.numeric(world$values[world$vars == "aspect"])
  var_stats[4,] = c("aspect", min(aspect), max(aspect), mean(aspect))

  var_stats[5,] = c("w_horizon (deg)", min(whors_deg), max(whors_deg), mean(whors_deg))
  var_stats[6,] = c("e_horizon (deg)", min(ehors_deg), max(ehors_deg), mean(ehors_deg))
  
  var_stats[,2:4] = round(sapply(var_stats[,2:4], as.numeric),4)

  #output
  cat("====================\n")
  cat("Worldfile Info\n")
  cat("====================\n")
  cat("File: ",worldfile,"\n")

  cat("Basins: ", format_ids(basins),"\n")
  cat("Hillslopes: ", format_ids(hills),"\n")
  cat("Zones: ", format_ids(zones),"\n")
  cat("Patches: ", format_ids(patches),"\n")
  cat("Canopy Strata - 1 strata:", strata_1,"| 2 strata:", strata_2,"\n")

  cat("\n")
  if (length(famIDs) > 0) {
    cat("MSR IS being used.\n")
    cat("Patch Families: ", format_ids(families),"\n")
    print(famidct)
  } else {
    cat("MSR is NOT being used.\n")
  }
  cat("\nPatch Sizes (20 most common): \n")
  print(psizesdf)

  cat("\nKey Variable Stats:\n")
  print(var_stats, row.names = F)

  return(list(basins=basins, hills=hills, zones=zones, patches=patches, strata_1=strata_1, strata_2=strata_2, families=if(exists("families")) families else NULL, famidct=if(exists("famidct")) famidct else NULL, psizesdf=psizesdf, var_stats=var_stats))

}

