# General and miscellaneous utilities

#' @export
setup_rhessys_folders = function(base_dir) {
  # base_dir = "~/Projects/RedRock/"
  
  len = 57 + nchar(base_dir)
  cat(paste0(rep("=",len),collapse = "") ,"\n")
  cat("*** Creating RHESSys project folders in directory: '",base_dir,"' ***\n",sep = "")
  cat(paste0(rep("=",len),collapse = "") ,"\n")

  # this is just a list of folders to check if they exist, and if not, create them
  # doing no capitalization
  make_dirs = c("clim", "defs","flowtables","output","plots","preprocessing","R","robj","scripts","tecfiles","worldfiles","output/filters","preprocessing/preprocess_out","preprocessing/spatial_source")

  # if (endsWith(base_dir,.Platform$file.sep)) {
  #   gsub("/$","test" ,base_dir)
  #   stringr::str_remove(base_dir)
  # }

  for (i in seq_along(make_dirs)) {
    if (!dir.exists(file.path(base_dir, make_dirs[i]))) {
      dir.create(file.path(base_dir, make_dirs[i]))
      cat("Created '",make_dirs[i],"' folder.\n",sep="")
    } else {
      cat("Skipped '",make_dirs[i],"' folder. Already existed.\n",sep="")
    }
  }

  cat(paste0(rep("=",48),collapse = "") ,"\n")
  cat("*** RHESSys project folder creation complete ***\n",sep = "")
  cat(paste0(rep("=",48),collapse = "") ,"\n")
  # return(NULL)

}

# ================================================================================

# readpars = read.csv("output/rh_out_2022-12-08--09-39-06/params/all_def_changes_2022-12-08_09.39.06.params",)
# table2list = function(X, Y) {
#   value = unname(X[names(X)==Y])
#   if (!is.na(suppressWarnings(as.numeric(value)))) {
#     value = as.numeric(value)
#   }
#   out = list(unname(X["def_file"]), unname(X["variable"]), value)
#   return(out)
# }
# inputdefsread = apply(readpars,MARGIN = 1, FUN = table2list, "run_60")
#
# dput(inputdefsread)

# ================================================================================

# text and sound alert, inside of collect output
#' @export
sim_alert = function(txt = "Simulations Complete", fg = 46, bg = 24) {
  # txt = "\t\t\tSimulations Complete\t\t\t"
  # 0:255
  # fg = 46
  # bg = 24
  w = getOption("width") + 4
  pad = paste0(rep(" ",(w - nchar(txt))/2),collapse = "")
  cat(paste0("\033[38;5;",fg,";48;5;",bg,"m",pad,txt,pad,"\033[0m","\n"))
  beepr::beep(2)
}

# ================================================================================
# copy a source world redefine file and rename it using a specified date
#' @export
tec_copy_redef = function(input_redef, redef_date, worldfile, overwrite = F) {
  if (length(input_redef) > 1) {
    stop(paste0("Too many input redefine files: ",input_redef))
  }
  if (is.character(redef_date) & length(redef_date) == 1) {
    redef_date = unlist(unname(strsplit(redef_date, "\\s+")))
  }
  filename = paste0(worldfile, ".Y", redef_date[1], "M", redef_date[2], "D", redef_date[3], "H", redef_date[4])
  file.copy(input_redef, filename, overwrite = overwrite)
  return(redef_date)
}

# ================================================================================
# this is to reouput a worldfile read into R via read_world
#' @export
write_world = function(world, path) {
  # world input should be output from read_world
  spacing = dplyr::recode(world$level, "world" = "", "basin" = "\t", "hillslope" = "\t\t", "zone" = "\t\t\t", "patch" = "\t\t\t\t", "canopy_strata" = "\t\t\t\t\t")
  world_str = paste0(spacing, world$values, "\t\t\t" , world$vars)
  writeLines(text = world_str, con = path)
}

# ================================================================================
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

# ================================================================================
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

# ================================================================================
# IDK WTF this is - autogen subset and output text
# get_fixed_vars = function(DT) {
#   isf = which(unname(sapply(DT, is.factor)))
#   un = sapply(DT[ , ..isf ], FUN = unique )
#   isun = sapply(un, FUN = function(X) length(X) == 1 )
#   res = sapply(un[isun], as.character)
#   out = paste(names(res), res, collapse = " | ",sep = ":")
#   return(out)
# }


# ================================================================================
# get runID - for setting output ids and collecting output and associating with an r obj
#' @export
GetRunID = function(increment = FALSE, reset = FALSE) {
  runidfile = ".runid"
  if (!file.exists(runidfile)) {
    runcon = file(runidfile,open = "w+")
    writeLines(text = "0",runcon)
    close(runcon)
    cat("Current run ID: ",0,"\n")
    return(0)
  }
  runcon = file(runidfile,open = "r+")
  if (reset) {
    writeLines(text = "0",runcon)
    close(runcon)
    cat("Current run ID: ",0,"\n")
    return(0)
  }
  runid = as.numeric(readLines(con = runcon))
  if (length(runid) == 0) {
    runid = 0
  }

  if (increment) {
    runid = runid + 1
    writeLines(as.character(runid), runcon)
  }
  close(runcon)
  cat("Current run ID: ",runid,"\n")
  return(runid)
}

# ================================================================================
# AddRunIDtoOutputFilters
#' @export
AddRunIDtoOutputFilters = function(output_filter, runid) {
  output_filter[names(output_filter) == "filter"] = lapply(output_filter[names(output_filter) == "filter"],
                                                           function(X,Y){X$output$filename = paste0(X$output$filename,"_RunID",Y); return(X)}, runid)
  return(output_filter)
}

#' @export
find_runID = function(out_dir) {
  allcsv = list.files(out_dir, ".csv")
  if (all(grepl("RunID\\d+",allcsv))) {
    runids = stringr::str_extract(allcsv,"RunID\\d+")
    if (length(unique(runids)) > 1) {
      cat("\nMore than 1 runid in  the output for some reason this won't work.\n")
    } else {
      cat("\nRun ID:",unique(runids),"\n")
      return(unique(runids))
    }
  } else {
    cat("\nCouldn't find run ID in the output file names\n")
    return(NA)
  }
}

#' @export
ExtractRunID = function(out_dir) {
  allfiles = list.files(out_dir)
  allrun = stringr::str_extract(allfiles,"RunID\\d+")
  run = unique(allrun[!is.na(allrun)])
  if (length(run) > 1) {
    stop(paste0("More than 1 run ID found:",run))
  }
  return(run)
}

#
# remove_common_parts <- function(char_vec) {
#   if (length(char_vec) <= 1) {
#     return(char_vec)
#   }
#
#   split_strings <- strsplit(char_vec, "")
#   min_length <- min(sapply(split_strings, length))
#
#   common_prefix <- ""
#   common_suffix <- ""
#
#   # Find common prefix
#   for (i in 1:min_length) {
#     current_chars <- sapply(split_strings, `[`, i)
#     if (length(unique(current_chars)) == 1) {
#       common_prefix <- paste0(common_prefix, current_chars[1])
#     } else {
#       break
#     }
#   }
#
#   # Find common suffix
#   for (i in 1:min_length) {
#     current_chars <- sapply(split_strings, function(x) x[length(x) - i + 1])
#     if (length(unique(current_chars)) == 1) {
#       common_suffix <- paste0(current_chars[1], common_suffix)
#     } else {
#       break
#     }
#   }
#
#   common_prefix_len <- nchar(common_prefix)
#   common_suffix_len <- nchar(common_suffix)
#
#   # Remove common prefix and suffix
#   stripped_strings <- sapply(char_vec, function(x) {
#     substr(x, common_prefix_len + 1, nchar(x) - common_suffix_len)
#   })
#
#   return(stripped_strings)
# }

#' @export
find_open_most_recent <- function(dir, pattern) {
  files = list.files(path = dir, pattern = pattern, full.names = T)
  file_info = file.info(files)
  most_recent_file = rownames(file_info[which.max(file_info$ctime), ])
  cat("Opening most recent file:", most_recent_file, "\n")
  browseURL(most_recent_file)
  return(most_recent_file)
}