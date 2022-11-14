#' collect_output
#'
#' Collects the output files data and parameter files from a rhessys run and auto-generates a folder to place them in
#' @import data.table
#' @import ggplot2
#' @export

collect_output = function(source_dir = "./", basename = "rh_out_", output_dir = "output") {
  # check that output folder exists
  if (!dir.exists(output_dir)) {
    stop ("Destination path '",output_dir,"' does not exist.")
  }
  # find csv and param files
  csv_files = list.files(path = output_dir, pattern = "*\\.csv")
  params_files = list.files(path = source_dir, pattern = "*\\.params")
  # check if theres at least either csv or params
  if (length(csv_files) == 0 & length(params_files) == 0) {
    cat("No csvs or params at specified directory.\n")
  } else {
    # make date+time unique folder in output folder
    dirname = paste0(basename, gsub( ":", "-", sub( " ", "--", Sys.time())))
    dir.create(path = file.path(source_dir, output_dir, dirname))
    cat("Created directory '",file.path(source_dir, output_dir, dirname),"'\n", sep = "")
    # move params
    if (length(params_files) > 0) {
      dir.create(path = file.path(source_dir, output_dir, dirname, "params"))
      shh = file.rename(from = file.path(source_dir, params_files), to = file.path(source_dir, output_dir, dirname, "params", params_files))
      cat("Moved RHESSys params files to new directory\n")
    } else {
      cat("No param files to move at specified directory.\n")
    }
    # run metadata
    runmeta = list.files(path = output_dir, pattern = "run_metadata_.*\\.txt")
    if (length(runmeta) > 1) {
      cat("Multiple metadata files found")
      dtstr = gsub(".txt","", gsub("run_metadata_","",runmeta))
      datetime = as.POSIXct(dtstr, format = "%Y-%m-%d--%H-%M-%OS")
      runmeta = runmeta[which.max(datetime)]
    }
    if (length(runmeta) == 1) {
      shh = file.rename(from = file.path(source_dir, output_dir, runmeta), to = file.path(source_dir, output_dir, dirname,runmeta))
      cat("Moved RHESSys run metadata files to new directory\n")
    }

    # moves csvs
    if (length(csv_files) > 0) {
      shh = file.rename(from = file.path(source_dir, output_dir, csv_files), to = file.path(source_dir, output_dir, dirname,csv_files))
      cat("Moved RHESSys output files to new directory.\n")
      return(file.path(source_dir, output_dir, dirname))
    } else {
      cat("No csvs at specified directory.\n")
    }

  }

}

