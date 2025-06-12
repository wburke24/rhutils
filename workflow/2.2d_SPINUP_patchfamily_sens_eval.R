library(RHESSysIOinR)
library(rhutils)
library(tidyverse)
library(sensitivity)
# ============================== Inputs ==============================
out_dir = ".//output/rh_out_2025-06-02--17-57-54/"

plotpdf_allvars(out_dir, out_name = "spinsoils_ID8_SENS", step = "yearly", pdfwidth = 10)

# input_def_pars
load("robj/SoilSpin_SENS_ID8_RunID26.RData")



senslist = pars_sens(out_dir = out_dir, input_def_pars = input_def_pars)




# out_name = paste0("spinsoils_ID8_SENS_",ExtractRunID(out_dir))
# step = "yearly"
# pdfwidth = 10
# hide_legend = T

# out_path = "plots"
# pattern = "_basin"
# pdfheight = 7
# summary_plots = F
# run_limit = 25
# auto_vars = T
# runs = NULL
