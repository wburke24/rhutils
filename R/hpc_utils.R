# hpc utils

#' @export
# reformats for hpc, writes with unique datetime to scipts folder
rhout_write_for_hpc = function(rhout, name) {
  #---- for hpc and copying files
  rhout_str = unlist(rhout)
  rhout_str = gsub("wsl ", "",rhout_str)
  rhout_str = gsub("../bin/rhessys7.4", "/RHESSys/rhessys7.5",rhout_str)

  # rhout_str = gsub(" -g -vmort_off", "",rhout_str)
  # rhout_str = gsub("(\\.yml_\\d+)( -par)", paste0("\\1 ","-g -vmort_off", "\\2"), rhout_str)

  # Extract the par digit
  x_value <- gsub("-par\\s", "", regmatches(rhout_str, regexpr("-par\\s\\d+", rhout_str)))
  # Extract "-par x" from the input string
  rhout_str <- gsub("-par\\s\\d+ ", "", rhout_str)
  # Concatenate the modified input string with "-par x" at the end
  rhout_str_par <- paste0(rhout_str, " -par ", x_value)
  outfile = paste0("scripts/",name,"_runcmds_",format(Sys.time(), "%Y-%m-%d--%H-%M-%S"),".txt" )
  writeLines(text = rhout_str_par,con = outfile)
  cat("Wrote RHESSys cmds to:", outfile)
  return(rhout_str_par)
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

  # dumb version for now?
  runids = seq_along(input_def_pars[[1]][[3]])
  outlist$hdr_files = paste0("worldfiles/",input_rhessys$world_hdr_prefix,"/",input_rhessys$world_hdr_prefix,"_",runids,".hdr")
  # defs
  defs_chg = unlist(unique(lapply(input_def_pars,"[",1)))
  defs_static = unname(unlist(input_hdr))[!unname(unlist(input_hdr)) %in% defs_chg]
  defs_chg_list = list()
  for (i in seq_along(defs_chg)) {
    defs_chg_list[[i]] = file.path("defs",gsub(".def","",basename(defs_chg[i])), paste0(gsub(".def","",basename(defs_chg[i])), "_",runids,".def"))
  }
  outlist$def_files = c(unlist(defs_chg_list), defs_static)
  outlist$outputfilters = paste0(output_filter$file_name,"_", runids)
  outlist$tec_files = input_rhessys$tec_file

  outlist$static_files = c(outlist$worldfile, outlist$flow_file, outlist$clim_files)
  outlist$run_files = c(outlist$hdr_files, outlist$def_files, outlist$outputfilters, outlist$tec_files)
  outlist$all_files = c(outlist$static_files, outlist$run_files)
  cat("Do all the files exist?: ",all(file.exists(outlist$all_files)))
  return(outlist)
}
