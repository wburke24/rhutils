#' Write RHESSys rules file from generic source cover data
#'
#' @param rules Data frame of unique rules and cover fractions
#' @param output_rules_file Path to write the rules file
#' @param veg_cols Vector of column names in rules that correspond to vegetation cover fractions
#' @param veg_ids Vector of vegetation IDs to use for each selected veg column in rules
#' @param strata_ct Optional vector of strata counts per rule
#' @export
write_rules_file = function(rules, output_rules_file, veg_cols, veg_ids, strata_ct = NULL) {
  
  fcon = file(output_rules_file, open = "wt")
  vals = as.data.frame(rules)[veg_cols]
    
  if (is.null(strata_ct)) {
    strata_ct = rep.int(1, length(veg_cols))
  }
  
  for (i in 1:nrow(rules)) {
    inc_cols = which(vals[i, ] != 0)
    text_out = c(paste0("ID\t", rules[i, "rule"]),
                 paste0("subpatch_count\t", length(inc_cols)),
                 paste0("_patch"),
                 paste0("\tpct_family_area\tvalue\t", paste(vals[i, inc_cols] / 100, collapse = " | ")),
                 paste0("\t_canopy_strata\t", paste(strata_ct[inc_cols], collapse = " | ")),
                 paste0("\t\tveg_parm_ID\t", paste(veg_ids[inc_cols], collapse = " | ")),
                 ""
    )
    writeLines(text = text_out, con = fcon)
  }
  close(fcon)
}
