# ------------------------------------------------------------
# R Workflow README 
# Will Burke
# 7/7/2025
# 
# Workflow covers setup, spinup, calibration, and basic runs
# Uses RHESSysPreprocessing, RHESSysIOinR, my less polished rhutils package, and others
# Run code below to attempt to install needed packages
# Some packages may not be available particularly the data retreival ones.
# Some steps may need to be done manually instead.
# ------------------------------------------------------------
source("R/fun_find_needed_packages.R")

needed_pkgs = workflow_missing_packages()
installed <- rownames(installed.packages())
to_install <- setdiff(needed_pkgs, installed)
if (length(to_install) > 0) install.packages(to_install)
   
needed_pkgs = workflow_missing_packages()
installed <- rownames(installed.packages())
to_install <- setdiff(needed_pkgs, installed)
if ("XPolaris"  %in% to_install) {
  # install.packages("devtools")
  devtools::install_github("lhmrosso/XPolaris")
}

# ---------- NOT REQUIRED ----------
# might be good to update packages, but skip if its a hassle
update.packages(ask="graphics")
