#' check_worldfile
#'
#' Read in a worldfile and output key info on that worldfile.
#' @export

check_worldfile = function(worldfile) {
  world = read_world(worldfile)

  format_ids = function(ids) {
    if (length(ids) > 20) {
      return(paste0("[",length(ids)," total] ", paste0(ids[1:20], collapse = " "),"..." ) )
    } else {
      return(paste0("[",length(ids)," total] ", paste0(ids, collapse = " ")))
    }
  }

  basins = unique(world$ID[world$level=="basin"])
  hills = unique(world$ID[world$level=="hillslope"])
  zones = unique(world$ID[world$level=="zone"])
  patches = unique(world$ID[world$level=="patch"])

  patchsizes = unique(world$values[world$vars == "area" & world$level=="patch"])
  psizes = summary(factor(world$values[world$vars == "area" & world$level=="patch"]))
  psizesdf = data.frame(area_sq_m = names(psizes), count = unname(psizes))
  psizesdf = psizesdf[order(psizesdf$count, decreasing = T),]
  rownames(psizesdf) = seq_along(psizesdf$area)
  if (nrow(psizesdf) > 20) {
    psizesdf = psizesdf[1:20,]
  }

  famIDs = world$values[world$vars == "family_ID" & world$level=="patch"]
  if (length(famIDs) > 0) {
    famct = table(factor(famIDs))
    famidct = data.frame(table(factor(famct)))
    names(famidct) = c("Patches in family","Count")
    families = unique(famIDs)
  }

  # horizon check
  # whors = world$values[world$vars == "w_horizon"]
  # ehors = world$values[world$vars == "e_horizon"]

  #output
  cat("====================\n")
  cat("Worldfile Info\n")
  cat("====================\n")
  cat("File: ",worldfile,"\n")

  cat("Basins: ", format_ids(basins),"\n")
  cat("Hillslopes: ", format_ids(hills),"\n")
  cat("Zones: ", format_ids(zones),"\n")
  cat("Patches: ", format_ids(patches),"\n")

  cat("\n")
  if (length(famIDs) > 0) {
    cat("MSR IS being used.\n")
    cat("Patch Families: ", format_ids(families),"\n")
    print(famidct)
  } else {
    cat("MSR IS NOT being used. ")
  }

  cat("\nPatch Sizes (20 most common): \n")
  print(psizesdf)


}

