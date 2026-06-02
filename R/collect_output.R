#' @title Collect RHESSys run outputs
#' @description
#' collect_output organizes the outputs of a RHESSys model run by creating a
#' unique, timestamped directory (default prefix \"rh_out_\") under the
#' specified output directory and moving CSV result files, any .params files,
#' and a single run metadata file into that directory. If multiple metadata
#' files matching the pattern are present, the most recent is selected.
#'
#' @param basename character(1). Prefix for the created timestamped folder.
#'   Defaults to \"rh_out_\".
#' @param source_dir character(1). Directory where source files (params and the
#'   output directory) live. Defaults to \"./\".
#' @param output_dir character(1). Relative path (inside source_dir) to the
#'   directory that contains RHESSys outputs (CSV files and run metadata).
#'   Defaults to \"output\". This directory must already exist.
#' @param pattern character(1) or NULL. If provided, only files matching this pattern will be moved. If NULL (default), all files are considered.
#' @param alert logical(1). If TRUE and CSV files are moved, calls
#'   sim_alert() after moving CSVs. Defaults to TRUE.
#' @param quiet logical(1). If TRUE, suppresses messages and alerts. Defaults to FALSE.
#'
#' @return character(1) Path to the newly created timestamped directory
#'   (invisibly) where files were moved. If there were no CSVs or params the
#'   function still creates the timestamped folder and returns its path. The
#'   function is called for its side-effects (creating directories and moving
#'   files).
#'
#' @details
#' - The function checks that output_dir exists; if it does not, the function
#'   stops with an error.
#'
#' - Parameter files (pattern \"*.params\") are moved into a \"params\" folder
#'   inside the timestamped directory.
#'
#' - Run metadata files are matched with pattern \"run_metadata_*.txt\"; if
#'   multiple matches are found the file with the latest timestamp inferred
#'   from its filename is moved.
#'
#' - CSV files in the output_dir are moved into the created folder. If none
#'   are found a message is printed and the created directory path is returned.
#'
#' @seealso sim_alert
#' @examples
#' \dontrun{
#' # Collect outputs into output/rh_out_<timestamp> and do not trigger alert
#' collect_output(
#'  basename = "run_",
#'  source_dir = "~/projects/my_run",
#'  output_dir = "output",
#'  alert = TRUE,
#'  quiet = FALSE
#' )
#'
#'
#' # Use current working directory and default names
#' collect_output()
#' }
#' @export
collect_output = function(basename = "rh_out_",
                          source_dir = "./",
                          output_dir = "output",
                          pattern = NULL,
                          alert = T,
                          quiet = F,
                          banner = T) {
  
  # ========== check that output folder exists ==========
  if (!dir.exists(file.path(source_dir, output_dir))) {
    if (!quiet) {for (i in 1:4) {beepr::beep(10)}}
    stop("Destination path '", file.path(source_dir, output_dir), "' does not exist.") 
  }
  # ========== find csv and param files ==========
  if (!is.null(pattern)) {
    csv_files = list.files(path = file.path(source_dir, output_dir), pattern = pattern)
  } else {
    csv_files = list.files(path = file.path(source_dir, output_dir), pattern = "*\\.csv")
  }

  params_files = list.files(path = source_dir, pattern = "*\\.params")
  # check if theres at least either csv or params
  if (length(csv_files) == 0 & length(params_files) == 0) {
    if (!quiet) {for (i in 1:4) {beepr::beep(10)}}
    sim_alert(txt = "No CSV or params files found to collect", fg = 231, bg = 130, quiet = T)
    # cat("No csvs or params at specified directory.\n")
    return(NULL)
  } else {
    # make date+time unique folder in output folder
    dirname = paste0(basename, format(Sys.time(), "%Y-%m-%d--%H-%M-%S"))
    dir.create(path = file.path(source_dir, output_dir, dirname))
    cat("Created directory '", file.path(source_dir, output_dir, dirname), "'\n", sep = "")
    # move params
    if (length(params_files) > 0) {
      dir.create(path = file.path(source_dir, output_dir, dirname, "params"))
      shh = file.rename(from = file.path(source_dir, params_files), to = file.path(source_dir, output_dir, dirname, "params", params_files))
      cat("Moved RHESSys params files to new directory\n")
    } else {
      cat("No param files to move at specified directory.\n")
    }
    # run metadata
    runmeta = list.files(path = file.path(source_dir, output_dir), pattern = "run_metadata_.*\\.txt")
    if (length(runmeta) > 1) {
      cat("Multiple metadata files found\n")
      dtstr = gsub(".txt", "", gsub("run_metadata_", "", runmeta))
      datetime = as.POSIXct(dtstr, format = "%Y-%m-%d--%H-%M-%OS")
      runmeta = runmeta[which.max(datetime)]
    }
    if (length(runmeta) == 1) {
      shh = file.rename(from = file.path(source_dir, output_dir, runmeta), to = file.path(source_dir, output_dir, dirname, runmeta))
      cat("Moved RHESSys run metadata files to new directory\n")
    }

    # moves csvs
    if (length(csv_files) > 0) {
      shh = file.rename(from = file.path(source_dir, output_dir, csv_files), to = file.path(source_dir, output_dir, dirname, csv_files))
      cat("Moved RHESSys output files to new directory.\n")
      if (alert) {
        if (banner) {
          if (Sys.info()['sysname'] == "Windows") {
            win_banner(txt = "RHESSys Run Completed")
          } else {
             cat("No banner for non-Windows systems currently\n") # for non-windows
          }
        }
        sim_alert(txt = paste0("RHESSys Run Completed\n", format(Sys.time(), "%Y-%m-%d %I:%M:%S %p")), fg = 231, bg = 55, quiet = quiet)
      }
      return(file.path(source_dir, output_dir, dirname))
    } else {
      # no CSVs 
      if (!quiet) {for (i in 1:4) {beepr::beep(10)}}
      sim_alert(txt = "No CSV files found at specified directory", fg = 231, bg = 130, quiet = T)
      return(file.path(source_dir, output_dir, dirname))
    }
  }
}
