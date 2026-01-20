# output aliase
#soils spinup
output_vars_soils = c("patch.soil_cs.totalc", "patch.soil_cs.DOC", "patch.soil_ns.totaln", "patch.soil_ns.DON", "stratum.cs.net_psn", 
                      "patch.sat_deficit", "patch.lai", "patch.totalc")

output_vars_soils_debug = c("patch.soil_cs.totalc", "patch.soil_cs.DOC", "patch.soil_ns.totaln", "patch.soil_ns.DON", "stratum.cs.net_psn", "patch.sat_deficit", "patch.lai", 
                            "patch.totalc", "patch.evaporation","patch.evaporation_surf", "patch.transpiration_unsat_zone","patch.transpiration_sat_zone",  "stratum.cs.net_psn",
                            "patch.snowpack.water_equivalent_depth", "patch.rootzone.depth", "patch.gw_drainage", "patch.rz_storage", "patch.unsat_storage")

# streamflow calibration
output_vars_streamflowcal = c("patch.evaporation", "patch.transpiration_unsat_zone", "patch.transpiration_sat_zone", "patch.streamflow",
                              "patch.snowpack.water_equivalent_depth", "patch.sat_deficit", "patch.lai", "stratum.epv.height", "patch.rootzone.depth" )

# water balance +
output_vars_waterbal = c("patch.evaporation","patch.evaporation_surf", "patch.exfiltration_sat_zone", "patch.exfiltration_unsat_zone",
                         "patch.transpiration_unsat_zone","patch.transpiration_sat_zone", "patch.streamflow",
                         "patch.totalc", "stratum.cs.net_psn", "stratum.cdf.psn_to_cpool", "patch.snowpack.water_equivalent_depth", "hill.gw.Qout",
                         "patch.rootzone.depth", "patch.sat_deficit", "patch.gw_drainage", "patch.lai","patch.rz_storage",
                         "patch.rz_transfer", "patch.unsat_storage", "patch.unsat_transfer", "patch.detention_store",
                         "stratum.Kstar_potential_both", "patch.total_water_in", "patch.litter.rain_stored", "hill.gw.storage","patch.canopy_rain_stored",
                         "patch.canopy_snow_stored")

output_vars_minimal = c("patch.soil_cs.totalc", "patch.soil_ns.totaln", "stratum.cs.net_psn", "patch.lai", "patch.totalc", "patch.evaporation", "patch.streamflow", "patch.snowpack.water_equivalent_depth", "patch.rootzone.depth", "patch.transpiration_unsat_zone", "patch.transpiration_sat_zone")

output_vars_interest = c("stratum.cs.net_psn", "patch.sat_deficit", "patch.lai", "patch.totalc", "patch.evaporation", "patch.streamflow")

output_cpools = c("stratum.cs.totalc", "stratum.cs.leafc", "stratum.cs.cpool", "stratum.cs.live_stemc", "stratum.cs.dead_stemc", "stratum.cs.live_crootc", "stratum.cs.dead_crootc", "stratum.cs.frootc", "stratum.cs.cwdc", "stratum.cs.cwdc_bg", "stratum.cs.gresp_transfer","stratum.cs.gresp_store")

output_carbon_all = c(output_cpools, "patch.litter_cs.totalc")

output_doc = c("patch.streamflow","patch.PET", "patch.snowpack.water_equivalent_depth", "hill.base_flow", "patch.surface_DOC", "patch.surface_DOC_Qin", "patch.surface_DOC_Qout", "patch.soil_cs.DOC","patch.soil_cs.DOC_Qin","patch.soil_cs.DOC_Qout", "patch.streamflow_DOC", "patch.cdf.DOC_to_gw", "hill.gw.DOC")

output_doc_select = c("patch.soil_cs.totalc", "patch.soil_ns.totaln","patch.lai", "patch.totalc", "patch.streamflow","patch.PET", "patch.snowpack.water_equivalent_depth", "hill.base_flow", "patch.surface_DOC", "patch.soil_cs.DOC","patch.streamflow_DOC", "patch.cdf.DOC_to_gw", "hill.gw.DOC" )

output_doc_carbon_many = c(
  "patch.soil_cs.totalc", "patch.soil_ns.totaln","patch.lai", "patch.totalc","patch.total_stemc","stratum.cs.leafc", "stratum.cs.cpool", "patch.litter_cs.totalc", "patch.litter_ns.totaln","stratum.cs.live_crootc", "stratum.cs.dead_crootc", "stratum.cs.cwdc", "patch.streamflow","patch.PET", "patch.snowpack.water_equivalent_depth", "hill.base_flow", "patch.surface_DOC", "patch.soil_cs.DOC","patch.streamflow_DOC", "patch.cdf.DOC_to_gw", "hill.gw.DOC","patch.surface_DON", "patch.soil_ns.DON", "patch.streamflow_DON", "hill.gw.DON"
)
# "patch.cdf.do_soil1c_loss", "patch.cdf.do_soil2c_loss", "patch.cdf.do_soil3c_loss", "patch.cdf.do_soil4c_loss", "patch.cdf.do_litr1c_loss_bg", "patch.cdf.do_litr2c_loss_bg", "patch.cdf.do_litr3c_loss_bg", "patch.cdf.do_litr4c_loss_bg", "patch.ndf.do_litr1n_loss", "patch.ndf.pmnf_l1s1"

output_vars_set1 = c("patch.soil_cs.totalc", "patch.soil_ns.totaln", "patch.lai", "patch.rootzone.depth", "patch.rz_storage","patch.unsat_storage", "stratum.cs.totalc", "stratum.cs.leafc", "stratum.cs.cpool", "stratum.cs.live_stemc", "stratum.cs.dead_stemc", "patch.evaporation","patch.evaporation_surf","patch.transpiration_unsat_zone","patch.transpiration_sat_zone","stratum.epv.height")

cat("Defined output variable aliases:\n")
cat("output_vars_soils\n")
cat("output_vars_streamflowcal\n")
cat("output_vars_waterbal\n")
cat("output_vars_minimal\n")
cat("output_vars_interest\n")
cat("output_cpools\n")
cat("output_carbon_all\n")
cat("output_doc\n")
cat("output_doc_select\n")
cat("output_vars_set1\n")


# cat("  Soils spinup vars:", paste(output_vars_soils, collapse = ", "), "\n")
# cat("  Streamflow calibration vars:", paste(output_vars_streamflowcal, collapse = ", "), "\n")
# cat("  Water balance vars:", paste(output_vars_waterbal, collapse = ", "), "\n")
# cat("  Minimal vars:", paste(output_vars_minimal, collapse = ", "), "\n")
# cat("  Interest vars:", paste(output_vars_interest, collapse = ", "), "\n")
# cat("  Carbon pool vars:", paste(output_cpools, collapse = ", "), "\n")
# cat("  All carbon vars:", paste(output_carbon_all, collapse = ", "), "\n")
# cat("  DOC vars:", paste(output_doc, collapse = ", "), "\n")
# cat("  Selected DOC vars:", paste(output_doc_select, collapse = ", "), "\n")
# cat("  Set1 vars:", paste(output_vars_set1, collapse = ", "), "\n")
