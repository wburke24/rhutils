# missing packages
workflow_missing_packages = function(folder_path = "R") {
  # Set the folder where your R scripts are
  # folder_path <- "R"

  # Get all .R files in the folder (recursively)
  r_files <- list.files(folder_path, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)

  # Function to extract package names from a file
  # extract_packages <- function(file) {
  #   lines <- readLines(file, warn = FALSE)
  #   pkgs <- unlist(regmatches(lines, gregexpr("(?<=library\\(|require\\()[\"']?\\w+[\"']?", lines, perl = TRUE)))
  #   pkgs <- gsub("[\"')]", "", pkgs)  # Remove quotes or closing parentheses
  #   return(pkgs)
  # }
  extract_packages <- function(file) {
  lines <- readLines(file, warn = FALSE)
  # Match library(...) or require(...) calls, capturing quoted or unquoted package names
  matches <- regmatches(
    lines,
    gregexpr("(?<=library\\(|require\\()[\"']?([a-zA-Z0-9._]+)[\"']?", lines, perl = TRUE)
  )
  pkgs <- unlist(matches)
  # Remove any trailing characters like parentheses or quotes
  pkgs <- gsub("[\"')]", "", pkgs)
  return(pkgs)
}


  # Get all packages from all files
  all_pkgs <- unique(unlist(lapply(r_files, extract_packages)))
  all_pkgs = all_pkgs[all_pkgs != "..."]

  # Filter out base packages if desired (optional)
  base_pkgs <- rownames(installed.packages(priority = "base"))
  needed_pkgs <- setdiff(all_pkgs, base_pkgs)
  return(needed_pkgs)
}



