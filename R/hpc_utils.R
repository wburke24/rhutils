# hpc utils

#' @export
# reformats for hpc, writes with unique datetime to scipts folder
rhout_write_for_hpc = function(rhout, name, rh_bin_replace = NULL) {
  #---- for hpc and copying files
  rhout_str = unlist(rhout)
  rhout_str = gsub("wsl ", "",rhout_str)
  # rhout_str = gsub("../bin/rhessys7.4", "/RHESSys/rhessys7.5",rhout_str)
  if (!is.null(rh_bin_replace)) {
    rhout_str = gsub("^.*rhessys\\d{1}\\.\\d{1} ", paste0(trimws(rh_bin_replace)," "),rhout_str)
    # rhout_str = gsub("../bin/rhessys\\d{1}.\\d{1}", "/RHESSys/rhessys7.5",rhout_str)
  }

  # this is to move the -par digit to end of string, otherwise weird errors can happen
  if (any(!grepl("-par \\d+",rhout_str))) {
    rhout_str_par = gsub(" -par \\d+","",rhout_str) # get rid of any -pars
    rhout_str_par = paste0(rhout_str_par," -par ",seq_along(rhout_str_par))
  } else {
    # Extract the par digit
    x_value <- gsub("-par\\s", "", regmatches(rhout_str, regexpr("-par\\s\\d+", rhout_str)))
    # Extract "-par x" from the input string
    rhout_str <- gsub("-par\\s\\d+ ", "", rhout_str)
    # Concatenate the modified input string with "-par x" at the end
    rhout_str_par <- paste0(rhout_str, " -par ", x_value)
  }
  outfile = paste0("scripts/",name,"_runcmds_",format(Sys.time(), "%Y-%m-%d--%H-%M-%S"),".txt" )
  writeLines(text = rhout_str_par,con = outfile)
  cat("Wrote RHESSys cmds to:", outfile)
  return(outfile)
}

#' @export
# returns list of all files separated by types
list_rh_input_files = function(input_rhessys, input_hdr, input_def_pars, input_tec_data, output_filter) {

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

  outlist$static_files = c(outlist$worldfile, outlist$flow_file, outlist$clim_files)
  outlist$run_files = c(outlist$redefs, outlist$hdr_files, outlist$def_files, outlist$outputfilters, outlist$tec_files)
  outlist$all_files = c(outlist$static_files, outlist$run_files)

  cat("Do all the files exist?: ",all(file.exists(outlist$all_files)),"\n")
  if (!all(file.exists(outlist$all_files))) {
    print(data.frame(file = outlist$all_files, exists = file.exists(outlist$all_files)))
    stop("Something went wrong, trying to list nonexistent files.\n")
  }
  return(outlist)
}

#' @export
# AIO function for pronghorn using method of choice scp
rhessysIO2pronghorn = function(rhout, name, rh_bin_replace = "/RHESSys/rhessys7.5", dest, usr,
                               input_rhessys = NULL, input_hdr = NULL, input_def_pars = NULL, input_tec_data = NULL, output_filter = NULL,
                               filelist = NULL,
                               transfer_method = "scp") {
  # ----- input checks -----
  if (!endsWith(usr,":") ) {usr = paste0(usr,":")}
  #fix output strings and write to file
  rhout_str = rhout_write_for_hpc(rhout, name, rh_bin_replace = rh_bin_replace)
  # make list of changed files
  if (is.null(filelist) & !is.null(input_rhessys)) {
    filelist = list_rh_input_files(input_rhessys, input_hdr, input_def_pars, input_tec_data, output_filter)
  }

  if (transfer_method == "scp") {
    defstr = c()
    for (i in seq_along(unique(dirname(filelist$def_files)))) {
      defdir = unique(dirname(filelist$def_files))[i]
      defdirfiles = filelist$def_files[dirname(filelist$def_files) == defdir]
      defstr = c(defstr,paste0("scp -i ~/rsa_key -p ",paste(defdirfiles,collapse =" ")," ", usr,dest,defdir))
    }

    scptxt =
      c(
        paste0("scp -i ~/rsa_key -p ",paste(filelist$tec_files,collapse =" ")," ", usr,dest,"tecfiles/"),
        paste0("scp -i ~/rsa_key -p ",paste(filelist$outputfilters,collapse =" ")," ", usr,dest,"output/filters"),
        paste0("scp -i ~/rsa_key -p ",paste(filelist$hdr_files,collapse =" ")," ", file.path(usr,dest,unique(dirname(filelist$hdr_files)))),
        defstr,
        paste0("scp -i ~/rsa_key -p ",rhout_str," ", usr,dest,"scripts")
      )

    remote_dirs = paste(unique(dirname(filelist$all_files)),collapse = " ")
    dircmd = paste0("wsl ssh -i ~/rsa_key ", gsub(":","",usr)," \"cd ",dest,"&& mkdir -p ",remote_dirs,"\"")
    cat("\nMaking dirs: ",dircmd,"\n\n")
    system(dircmd)

    cat("\nRunning SCP commands...\n\n")
    scpcon = file("scripts/scpfilelist.sh","wb")
    writeLines(text = c("#!/bin/bash",scptxt), con = scpcon, sep = "\n")
    close(scpcon)
    system("wsl ./scripts/scpfilelist.sh")
    file.remove("scripts/scpfilelist.sh")

    # scpcmd = paste0("wsl ",paste0(scptxt,collapse = ";"))
    # cat("\nRunning SCP commands...\n\n")
    # system(scpcmd)
    cat("SCP file transfer completed.\n")
    return(rhout_str)

  } else if (transfer_method == "rsync") {
    # copy_cmd1 = paste0("rsync -azPR ", paste0(c(filelist$all_files,rhout_str),collapse = " ") , " ", usr,dest)
    # dput(copy_cmd1)
    # writeLines(text = c(filelist$all_files,rhout_str),con = "scripts/filelist.txt")
    # copy_cmd2 = paste0("rsync -avvvPR --files-from=scripts/filelist.txt ./ ", usr,dest)
    # dput(copy_cmd2)
    cat("This doesn't work right now.")
  }

}

#' @export
# check list of list of files and collapse to remove duplicate sublists and entries (from a loop of runs running list_rh_input_files())
collapse_rh_input_files_list = function(filelist) {
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