# General and miscellaneous utilities

# copy a source world redefine file and rename it using a specified date
#' @export
tec_copy_redef = function(input_redef, redef_date, worldfile, overwrite = F) {
  if (is.character(redef_date) & length(redef_date) == 1) {
    redef_date = unlist(unname(strsplit(redef_date, "\\s+")))
  }
  filename = paste0(worldfile, ".Y", redef_date[1], "M", redef_date[2], "D", redef_date[3], "H", redef_date[4])
  file.copy(input_redef, filename, overwrite = overwrite)
  return(redef_date)
}

# this is to reouput a worldfile read into R via read_world
#' @export
write_world = function(world, path) {
  # world input should be output from read_world
  spacing = dplyr::recode(world$level, "world" = "", "basin" = "\t", "hillslope" = "\t\t", "zone" = "\t\t\t", "patch" = "\t\t\t\t", "canopy_strata" = "\t\t\t\t\t")
  world_str = paste0(spacing, world$values, "\t\t\t" , world$vars)
  writeLines(text = world_str, con = path)
}

# find a world.state file and path, useful for finding and renaming newly made worldfiles
#' @export
worldstate = function(worldfile) {
  world_state = list.files(path = dirname(worldfile), pattern = paste0(basename(worldfile), ".*\\.state"), full.names = T)
  if (length(world_state) > 1) {
    warning("More than one worldfile .state files found:", world_state)
    return(NULL)
  } else if (length(world_state) == 0) {
    warning("No worldfile .state files found")
    return(NULL)
  } else {
    return(world_state)
  }
}

#' @export
daily_dates = function(X, Y) {
  out = X
  out$run = Y
  out$year_month = zoo::as.yearmon(paste0(out$year,"-", out$month))
  out$ym_ind = as.numeric(out$year_month)
  return(out)
}

# water balance for multiple basin outputs using output filters
#' @export
watbal_basin_of_multi = function(out_dir) {
  basinfiles = list.files(out_dir, "basin.csv", full.names = T)
  run_names = gsub("_basin.csv", "", basename(basinfiles))
  basin_list = lapply(basinfiles, fread)
  vars_in_basin = names(basin_list[[1]])[!names(basin_list[[1]]) %in%
                                           c("day", "month", "year", "basinID", "hillID", "zoneID", "patchID", "stratumID", "date", "run")]
  watbal = list()
  for (i in seq_along(basin_list)) {
    watbal[[i]] = RHESSysIOinR::watbal_basin_of(basin_list[[i]])
  }
  names(watbal) = run_names
  return(watbal)
}

# IDK WTF this is - autogen subset and output text
get_fixed_vars = function(DT) {
  isf = which(unname(sapply(DT, is.factor)))
  un = sapply(DT[ , ..isf ], FUN = unique )
  isun = sapply(un, FUN = function(X) length(X) == 1 )
  res = sapply(un[isun], as.character)
  out = paste(names(res), res, collapse = " | ",sep = ":")
  return(out)
}




