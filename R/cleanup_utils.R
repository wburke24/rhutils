# cleanup utils

# ================================================================================
#' @export
cleanup_wmfire = function(dir) {
  pat = "ETGridYear|FireFailedIterGridYear|FireSizes|FireSpreadIterGridYear|FireSpreadPropGridYear|LoadGridYear|PDefGridYear|PETGridYear|PLoadGridYear|PSlopeGridYear|PWindGridYear|RelDefGridYear|SoilMoistGridYear|UnderETGridYear|UnderPETGridYear|VegLoadGridYear|CWDGridYear"
  files = list.files(path = dir, pattern = pat, full.names = F)
  dirname = paste0("fire_grids_out_", gsub( ":", ".", sub( " ", "_", Sys.time())))
  dir.create(path = file.path(dir, dirname))
  cat("Created directory '",file.path(dir, dirname),"'\n", sep = "")
  shh = file.rename(from = file.path(dir, files), to = file.path(dir,dirname,files))
  cat("Moved fire grid output files to new directory\n")
}

# ================================================================================
#' @export
collect_csvs = function(dir, dir_base = "rh_out_") {
  csv_files = list.files(path = dir, pattern = "*\\.csv")
  # make a folder with a unique name -  rh_out_date+time
  if (length(csv_files) > 0) {
    dirname = paste0(dir_base, gsub( ":", ".", sub( " ", "_", Sys.time())))
    dir.create(path = file.path(dir, dirname))
    cat("Created directory '",file.path(dir, dirname),"'\n", sep = "")
    shh = file.rename(from = file.path(dir, csv_files), to = file.path(dir,dirname,csv_files))
    cat("Moved RHESSys output files to new directory\n")
    return(file.path(dir, dirname))
  } else {
    cat("No csvs at specified directory.")
  }
}

# ================================================================================
#' @export
collect_params = function(dir) {
  params_files = list.files(path = dir, pattern = "*\\.params")
  # make a folder with a unique name -  rh_out_date+time
  if (length(params_files) > 0) {
    dirname = paste0("params_", gsub( ":", ".", sub( " ", "_", Sys.time())))
    dir.create(path = file.path(dir,"params", dirname))
    cat("Created directory '",file.path(dir,"params", dirname),"'\n", sep = "")
    shh = file.rename(from = file.path(dir, params_files), to = file.path(dir, "params", dirname,params_files))
    cat("Moved RHESSys params files to new directory\n")
  } else {
    cat("No param files to move at specified directory.")
  }
}
