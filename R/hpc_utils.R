# hpc utils - DEPRECATED. These functions are superseded by hpc_transfer.R (rh_out_list_flatten,
# build_rh_input_file_list, test_hpc_ssh, transfer_to_hpc, getHPC) and will be removed in a future version.

# #' Format RHESSys run commands for HPC submission
# #'
# #' @param rhout Character vector of RHESSys commands
# #' @param name Label to include in the output filename
# #' @param rh_bin_replace Optional replacement RHESSys binary path
# #' @param prefix_cmd Optional command prefix
# #' @export
# rhout_write_for_hpc = function(rhout, name, rh_bin_replace = NULL, prefix_cmd = NULL) {
#   #---- for hpc and copying files
#   rhout_str = unlist(rhout)
#   # remove leading 'wsl bash -lc ' if present
#   rhout_str = gsub("^wsl bash -lc ","",rhout_str)
#   # remove leading 'wsl ' if present
#   rhout_str = gsub("^wsl ", "",rhout_str)
#   # remove escaped quotes
#   rhout_str = gsub('\\"',"",rhout_str)
#   # replace rhessys binary path if desired
#   if (!is.null(rh_bin_replace)) {
#     rhout_str = gsub("^.*rhessys\\S+\\s+", paste0(trimws(rh_bin_replace), " "), rhout_str)
#     # rhout_str = gsub("../bin/rhessys\\d{1}.\\d{1}", "/RHESSys/rhessys7.5",rhout_str)
#   }

#   # this is to move the -par digit to end of string, otherwise weird errors can happen
#   if (any(!grepl("-par \\d+",rhout_str))) {
#     rhout_str_par = gsub(" -par \\d+","",rhout_str) # get rid of any -pars
#     rhout_str_par = paste0(rhout_str_par," -par ",seq_along(rhout_str_par))
#   } else {
#     # Extract the par digit
#     x_value <- gsub("-par\\s", "", regmatches(rhout_str, regexpr("-par\\s\\d+", rhout_str)))
#     # Extract "-par x" from the input string
#     rhout_str <- gsub("-par\\s\\d+ ", "", rhout_str)
#     # Concatenate the modified input string with "-par x" at the end
#     rhout_str_par <- paste0(rhout_str, " -par ", x_value)
#   }
#   # if theres a prefix command, use bash -c and single quotes to wrap both commands
#   if (!is.null(prefix_cmd)) {
#     rhout_str_par = paste0("bash -c '", trimws(prefix_cmd), " && ", rhout_str_par, "'")
#   }

#   outfile = paste0("scripts/",name,"_runcmds_",format(Sys.time(), "%Y-%m-%d--%H-%M-%S"),".txt" )
#   writeLines(text = rhout_str_par,con = outfile)
#   cat("Wrote RHESSys cmds to:", outfile)
#   return(outfile)
# }

#' List RHESSys input files required for a run
#'
#' @param input_rhessys RHESSys inputs list
#' @param input_hdr Header list of definition files
#' @param input_def_pars Definition parameter list
#' @param input_tec_data Tec events data frame
#' @param output_filter Output filter list
#' @export
list_rh_input_files = function(input_rhessys, input_hdr, input_def_pars = NULL, input_tec_data, output_filter) {
  .Deprecated("build_rh_input_file_list", package = "rhutils", msg = "list_rh_input_files() is deprecated (hpc_utils.R is being retired) - use build_rh_input_file_list().")

  outlist = list()
  outlist$all_files = NA
  tmp = read.table(input_hdr$base_stations)
  opts = c(".rain", ".tmin", ".tmax", ".tavg", ".dayl", ".daytime_rain_duration",
           ".LAI_scalar", ".Ldown", ".Kdown_direct", ".Kdown_diffuse", ".ndep_NO3", ".ndep_NH4",
           ".PAR_direct", ".PAR_diffuse", ".relative_humidity", ".tday", ".tnightmax", ".tsoil",
           ".vpd", ".wind", ".CO2", ".lapse_rate_tmin", ".lapse_rate_tmax",".tdewpoint")
  clim_base = tmp$V1[tmp$V2 == "climate_prefix"]
  outlist$clim_files = paste0(clim_base,opts)[file.exists(paste0(clim_base,opts))]
  outlist$worldfile = input_rhessys$world_file
  outlist$flow_file = input_rhessys$flow_file
  outlist$tec_files = input_rhessys$tec_file

  outlist$redefs = NULL
  if (any(grepl("redefine",input_tec_data$name))) {
    tec_redef = input_tec_data[grepl("redefine",input_tec_data$name),]
    redefs = paste0(input_rhessys$world_file, ".Y",tec_redef$year,"M", tec_redef$month,"D", tec_redef$day,"H",tec_redef$hour)
    outlist$redefs = redefs
  }

  # dumb version for now?
  runids = seq_along(input_def_pars[[1]][[3]])
  runsuffix = paste0("_",runids)
  if (length(runids) == 1) {
    runsuffix = NULL
  }

  if (!is.null(input_def_pars) ) {
    if (!is.null(input_rhessys$world_hdr_path)) {
      outlist$hdr_files = paste0("worldfiles/",input_rhessys$world_hdr_path,"/",input_rhessys$world_hdr_prefix,runsuffix,".hdr")
    } else {
      outlist$hdr_files = paste0("worldfiles/",input_rhessys$world_hdr_prefix,"/",input_rhessys$world_hdr_prefix,runsuffix,".hdr")
    }
    # defs - assumes everything included in input defs is changing which i think is true
    defs_chg = unlist(unique(lapply(input_def_pars,"[",1)))
    defs_static = unname(unlist(input_hdr))[!unname(unlist(input_hdr)) %in% defs_chg]

    defs_chg_list = list()
    for (i in seq_along(defs_chg)) {
      defs_chg_list[[i]] = file.path("defs",gsub(".def","",basename(defs_chg[i])), paste0(gsub(".def","",basename(defs_chg[i])), runsuffix,".def"))
    }
    outlist$def_files = c(unlist(defs_chg_list), defs_static)
    outlist$outputfilters = paste0(output_filter$file_name,runsuffix)

  } else {
    outlist$hdr_files = paste0("worldfiles/",ifelse(is.null(input_rhessys$world_hdr_path), input_rhessys$world_hdr_prefix, input_rhessys$world_hdr_path) ,"/",input_rhessys$world_hdr_prefix,".hdr")
    outlist$def_files = unname(unlist(input_hdr))
    outlist$outputfilters = paste0(output_filter$file_name)
  }
  if ("fire_grid_prefix" %in% names(input_hdr)) {
      fire_grids = paste(unname(unlist(input_hdr$fire_grid_prefix)), c(".dem", ".hillslope", ".patch",".zone"), sep = "")
      outlist$def_files = outlist$def_files[-which(outlist$def_files == input_hdr$fire_grid_prefix)]
      outlist$def_files = c(outlist$def_files, fire_grids)
  }

  outlist$static_files = c(outlist$worldfile, outlist$flow_file, outlist$clim_files)
  outlist$run_files = c(outlist$redefs, outlist$hdr_files, outlist$def_files, outlist$outputfilters, outlist$tec_files)
  outlist$all_files = c(outlist$static_files, outlist$run_files)

  cat("Do all the files exist?: ",all(file.exists(outlist$all_files)),"\n")
  if (!all(file.exists(outlist$all_files))) {
    tmp = data.frame(file = outlist$all_files, exists = file.exists(outlist$all_files))
    print(tmp[tmp$exists == FALSE,])
    # print(outlist$all_files[!file.exists(outlist$all_files)])
    message("Some files do not exist - Removing from list.\n")
    outlist$all_files = outlist$all_files[file.exists(outlist$all_files)]
    outlist$static_files = outlist$static_files[file.exists(outlist$static_files)]
    outlist$run_files = outlist$run_files[file.exists(outlist$run_files)]
    outlist$clim_files = outlist$clim_files[file.exists(outlist$clim_files)]
    outlist$hdr_files = outlist$hdr_files[file.exists(outlist$hdr_files)]
    outlist$def_files = outlist$def_files[file.exists(outlist$def_files)]
    outlist$outputfilters = outlist$outputfilters[file.exists(outlist$outputfilters)]
    outlist$tec_files = outlist$tec_files[file.exists(outlist$tec_files)]
  }
  return(outlist)
}

#' Transfer RHESSys inputs and commands to pronghorn HPC
#'
#' @param rhout Character vector of RHESSys commands
#' @param name Label for generated files
#' @param rh_bin_replace Replacement RHESSys binary path on HPC
#' @param dest Remote destination path
#' @param usr Remote user (with hostname)
#' @param input_rhessys RHESSys inputs list
#' @param input_hdr Header list of definition files
#' @param input_def_pars Definition parameter list
#' @param input_tec_data Tec events data frame
#' @param output_filter Output filter list
#' @param filelist Optional precomputed file list
#' @param transfer_method Transfer method (`"scp"` or `"rsync"`)
#' @param rsa_loc Path to SSH key
#' @export
rhessysIO2pronghorn = function(
  rhout,
  name,
  rh_bin_replace = "/RHESSys/rhessys7.5",
  prefix_cmd = NULL,
  dest,
  usr,
  input_rhessys = NULL,
  input_hdr = NULL,
  input_def_pars = NULL,
  input_tec_data = NULL,
  output_filter = NULL,
  filelist = NULL,
  transfer_method = "rsync",
  rsa_loc = "~/rsa_key"
) {
  .Deprecated("transfer_to_hpc", package = "rhutils", msg = "rhessysIO2pronghorn() is deprecated (hpc_utils.R is being retired) - use rh_out_list_flatten(), build_rh_input_file_list(), and transfer_to_hpc().")
  # ----- input checks -----
  transfer_method = tolower(transfer_method)
  if (!transfer_method %in% c("scp", "rsync")) {
    stop("transfer_method must be either 'scp' or 'rsync'.")
  }

  if (!endsWith(usr, ":")) {
    usr = paste0(usr, ":")
  }
  #fix output strings and write to file
  rhout_str = rhout_write_for_hpc(rhout, name, rh_bin_replace = rh_bin_replace, prefix_cmd = prefix_cmd)

  # make list of changed files - list_rh_input_files is backup now
  if (is.null(filelist) & !is.null(input_rhessys)) {
    filelist = list_rh_input_files(
      input_rhessys,
      input_hdr,
      input_def_pars,
      input_tec_data,
      output_filter
    )
  } else {
    files_to_copy = unique(c(
      input_rhessys$world_file,
      input_rhessys$flow_file,
      filelist$tec_files,
      filelist$output_filters,
      filelist$hdr_files,
      filelist$def_files,
      rhout_str
    ))
    files_to_copy = files_to_copy[file.exists(files_to_copy)]
  }

  if (transfer_method == "scp") {
    defstr = c()
    for (i in seq_along(unique(dirname(filelist$def_files)))) {
      defdir = unique(dirname(filelist$def_files))[i]
      defdirfiles = filelist$def_files[dirname(filelist$def_files) == defdir]
      defstr = c(
        defstr,
        paste0(
          "scp -i ",
          rsa_loc,
          " -p ",
          paste(defdirfiles, collapse = " "),
          " ",
          usr,
          dest,
          defdir
        )
      )
    }

    scptxt =
      c(
        paste0(
          "scp -i ",
          rsa_loc,
          " -p ",
          paste(filelist$tec_files, collapse = " "),
          " ",
          usr,
          dest,
          "tecfiles/"
        ),
        paste0(
          "scp -i ",
          rsa_loc,
          " -p ",
          paste(filelist$output_filters, collapse = " "),
          " ",
          usr,
          dest,
          "output/filters"
        ),
        paste0(
          "scp -i ",
          rsa_loc,
          " -p ",
          paste(filelist$hdr_files, collapse = " "),
          " ",
          file.path(usr, dest, unique(dirname(filelist$hdr_files)))
        ),
        defstr,
        paste0("scp -i ", rsa_loc, " -p ", rhout_str, " ", usr, dest, "scripts")
      )

    remote_dirs = paste(unique(dirname(filelist$all_files)), collapse = " ")
    dircmd = paste0(
      "wsl ssh -i ",
      rsa_loc,
      " ",
      gsub(":", "", usr),
      " \"cd ",
      dest,
      "&& mkdir -p ",
      remote_dirs,
      "\""
    )
    cat("\nMaking dirs: ", dircmd, "\n\n")
    system(dircmd)

    cat("\nRunning SCP commands...\n\n")
    scpcon = file("scripts/scpfilelist.sh", "wb")
    writeLines(text = c("#!/bin/bash", scptxt), con = scpcon, sep = "\n")
    close(scpcon)
    system("wsl ./scripts/scpfilelist.sh")
    file.remove("scripts/scpfilelist.sh")

    # scpcmd = paste0("wsl ",paste0(scptxt,collapse = ";"))
    # cat("\nRunning SCP commands...\n\n")
    # system(scpcmd)
    cat("SCP file transfer completed.\n")
    return(rhout_str)
  } else if (transfer_method == "rsync") {
    # Match SCP behavior: transfer tec, output filters, headers, defs, and generated run script.

    # Ensure remote dirs exist (create under dest for the directories being transferred)
    remote_dirs = paste(unique(dirname(files_to_copy)), collapse = " ")
    dircmd = paste0(
      "wsl ssh -i ",
      rsa_loc,
      " ",
      gsub(":", "", usr),
      " \"cd ",
      dest,
      " && mkdir -p ",
      remote_dirs,
      "\""
    )
    cat("\nMaking dirs: ", dircmd, "\n\n")
    dir_status = system(dircmd)
    if (!identical(dir_status, 0L)) {
      stop("Failed to create remote directories for rsync transfer.")
    }

    # Write file list to a temp file to avoid Windows command line length limits
    files_from_tmp = tempfile(fileext = ".txt")
    writeLines(files_to_copy, files_from_tmp)
    # Convert Windows path to WSL path for --files-from
    files_from_wsl = system(
      paste0("wsl wslpath -u \"", files_from_tmp, "\""),
      intern = TRUE
    )

    rsync_cmd = paste0(
      "wsl rsync -aRv --files-from=",
      files_from_wsl,
      " . ",
      "-e \"ssh -i ",
      rsa_loc,
      "\" ",
      usr,
      dest
    )

    cat("\nRunning rsync command...\n\n")
    rsync_status = system(rsync_cmd)
    if (!identical(rsync_status, 0L)) {
      stop("rsync file transfer failed.")
    }

    cat("Rsync file transfer completed.\n")
    return(rhout_str)
  }
}

#' Collapse nested RHESSys input file lists
#'
#' @param filelist Nested list of RHESSys input files
#' @export
collapse_rh_input_files_list = function(filelist) {
  .Deprecated("rh_out_list_flatten", package = "rhutils", msg = "collapse_rh_input_files_list() is deprecated (hpc_utils.R is being retired) - use rh_out_list_flatten() and build_rh_input_file_list().")
  if ((is.list(filelist) & length(filelist)==10) & !is.list(filelist[[1]])) {
    stop("filelist is probably already correctly formatted for hpc functions")
  }
  tmp = unlist(filelist,recursive = F)
  filenames = unique(names(tmp))
  filelist_new = list()
  for (i in seq_along(filenames)) {
    n = which(names(tmp) == filenames[i])
    filelist_new[[filenames[i]]] = unique(unname(unlist(tmp[n])))
  }
  return(filelist_new)
}

#' Test SSH connectivity to pronghorn via WSL
#'
#' @param user Remote user@host string
#' @param key Path to SSH private key
#' @param timeout Connection timeout in seconds
#' @param remote_cmd Command to execute for the test
#' @export
test_pronghorn_ssh <- function(
  user = "wburke@pronghorn.rc.unr.edu",
  key = "~/rsa_key",
  timeout = 10,
  remote_cmd = "echo ok"
) {
  .Deprecated("test_hpc_ssh", package = "rhutils", msg = "test_pronghorn_ssh() is deprecated (hpc_utils.R is being retired) - use test_hpc_ssh().")
  cmd <- "wsl"
  args <- c(
    "ssh",
    "-i",
    key,
    "-o",
    "BatchMode=yes",
    "-o",
    "StrictHostKeyChecking=accept-new",
    "-o",
    sprintf("ConnectTimeout=%d", timeout),
    user,
    remote_cmd
  )
  out <- tryCatch(
    system2(cmd, args, stdout = TRUE, stderr = TRUE),
    error = function(e) structure(character(), status = 1L, class = "try-error")
  )
  status <- attr(out, "status")
  if (is.null(status)) {
    status <- 0L
  }
  list(ok = identical(status, 0L), status = status, stdout = out)
}