#devtools::install_github("wburke24/rhutils")

# lpc_data_path = "../data/Landcover/2019/" 
# lpc_pattern = "h003v008"
# mask_map_path = "preprocessing/spatial90m/basin.tif"
# round_value = 10
# output_rules_map = "preprocessing/spatial90m/rules_90m.tif"
# output_rules_file = "preprocessing/rules/LPC_90m.rules"

# lpc_extract
#' Extract and prep Landfire/LPC tiles
#'
#' @param lpc_data_path Directory containing LPC tiles
#' @param lpc_pattern Vector of tile patterns
#' @param mask_map_path Path to mask raster
#' @param plots Logical; show diagnostic plots
#' @export
lpc_extract = function(lpc_data_path, lpc_pattern, mask_map_path, plots = F) {
  lpc_filepaths = lapply(lpc_pattern, list.files, path = lpc_data_path, full.names = T)
  # lpc_filepaths = list.files(path = lpc_data_path, pattern = lpc_pattern, full.names = T)
  tc_inlist = list()
  sc_inlist = list()
  hc_inlist = list()
  oc_inlist = list()
  for (i in seq_along(lpc_pattern)) {
    # tc tree cover, sc shrub cover, hc herb cover, oc other cover
    tc_inlist[[i]] = rast(lpc_filepaths[[i]][grepl("_TC_|_tc_",lpc_filepaths[[i]])])
    sc_inlist[[i]] = rast(lpc_filepaths[[i]][grepl("_SC_|_sc_",lpc_filepaths[[i]])])
    hc_inlist[[i]] = rast(lpc_filepaths[[i]][grepl("_HC_|_hc_",lpc_filepaths[[i]])])
    oc_inlist[[i]] = rast(lpc_filepaths[[i]][grepl("_OC_|_oc_",lpc_filepaths[[i]])])
  }
  tc_in = terra::merge(sprc(tc_inlist))
  sc_in = terra::merge(sprc(sc_inlist))
  hc_in = terra::merge(sprc(hc_inlist))
  oc_in = terra::merge(sprc(oc_inlist))
  mask_map = rast(mask_map_path)
  mask_vect = as.polygons(mask_map)
  mask_proj = project(mask_vect, tc_in)
  # xmin West  xmax East  ymin North  ymax South
  if (any(ext(tc_in[[1]])[1] > ext(mask_proj)[1], ext(tc_in[[1]])[2] < ext(mask_proj)[2], 
          ext(tc_in[[1]])[3] > ext(mask_proj)[3], ext(tc_in[[1]])[4] < ext(mask_proj)[4])) {
    plot(tc_in[[1]])
    plot(mask_proj, add=T)
    plot(ext(mask_proj))
    plot(ext(tc_in[[1]]), add=T)
    stop("Extent of mask not fully encompassed by LPC tile")
  }
  
  if (plots) {
    plot(tc_in[[1]])
    plot(mask_proj, add=T)
  }
  # project
  tc_proj = project(tc_in, mask_map)
  sc_proj = project(sc_in, mask_map)
  hc_proj = project(hc_in, mask_map)
  oc_proj = project(oc_in, mask_map)
  # crop 
  tc_crop = crop(tc_proj, mask_map)
  sc_crop = crop(sc_proj, mask_map)
  hc_crop = crop(hc_proj, mask_map)
  oc_crop = crop(oc_proj, mask_map)
  # mask everything
  tc = mask(tc_crop, mask_map)
  sc = mask(sc_crop, mask_map)
  hc = mask(hc_crop, mask_map)
  oc = mask(oc_crop, mask_map)
  
  lpc_map_list = list(tc = tc,sc = sc,hc = hc,oc = oc)
  return(lpc_map_list)
}
# lpc_map_list = lpc_extract(lpc_data_path = lpc_data_path, lpc_pattern = lpc_pattern, mask_map_path = mask_map_path)

# lpc maps to dataframe
#' Convert LPC rasters to data frame
#'
#' @param lpc_map_list List of LPC raster layers
#' @param plots Logical; show histograms
#' @export
lpc_as_df = function(lpc_map_list, plots = F) {
  
  mean_dt = data.table(values(lpc_map_list$tc[[1]])[!is.na(values(lpc_map_list$tc[[1]]))],
                       values(lpc_map_list$sc[[1]])[!is.na(values(lpc_map_list$sc[[1]]))],
                       values(lpc_map_list$hc[[1]])[!is.na(values(lpc_map_list$hc[[1]]))],
                       values(lpc_map_list$oc[[1]])[!is.na(values(lpc_map_list$oc[[1]]))])
  names(mean_dt) = c("Tree","Shrub","Herb","Other")
  
  mean_dt$id = 1:nrow(mean_dt)
  mean_dt$sum = mean_dt$Tree + mean_dt$Shrub + mean_dt$Herb + mean_dt$Other
  
  if (plots) {
    par(mfrow = c(2,2))
    hist(mean_dt$Tree)
    hist(mean_dt$Shrub)
    hist(mean_dt$Herb)
    hist(mean_dt$Other)
  }
  
  return(mean_dt)
}
# lpc_df = lpc_as_df(lpc_map_list)

#lpc_bin
#' Bin LPC percentages
#'
#' @param lpc_df LPC data frame
#' @param round_value Rounding increment
#' @export
lpc_bin = function(lpc_df, round_value) {
  # ------------------------------ BINNING FOR MSR RULES ------------------------------
  # binning - tree, binned to nearest 10
  # lpc_df$Tree_rnd = round(lpc_df$Tree, digits = -1)
  # lpc_df$Shrub_rnd = round(lpc_df$Shrub, digits = -1)
  # lpc_df$Herb_rnd = round(lpc_df$Herb, digits = -1)
  # lpc_df$Other_rnd = round(lpc_df$Other, digits = -1)

  lpc_df$Tree_rnd = round(lpc_df$Tree/round_value)*round_value
  lpc_df$Shrub_rnd = round(lpc_df$Shrub/round_value)*round_value
  lpc_df$Herb_rnd = round(lpc_df$Herb/round_value)*round_value
  lpc_df$Other_rnd = round(lpc_df$Other/round_value)*round_value
  
  # IF just tree + shrub + herb > 100%, remove from herb
  lpc_df$Herb_rnd_crt =  ifelse(lpc_df$Tree_rnd + lpc_df$Shrub_rnd + lpc_df$Herb_rnd > 100, 100 - (lpc_df$Tree_rnd + lpc_df$Shrub_rnd),lpc_df$Herb_rnd)
  # summary(lpc_df$Tree_rnd + lpc_df$Shrub_rnd + lpc_df$Herb_rnd)
  # summary(lpc_df$Tree_rnd + lpc_df$Shrub_rnd + lpc_df$Herb_rnd_crt)
  
  # IF tree+shrub+herb+other > 100% remove from other
  lpc_df$Other_rnd_crt =  ifelse(lpc_df$Tree_rnd + lpc_df$Shrub_rnd + lpc_df$Herb_rnd_crt + lpc_df$Other_rnd > 100, 
                                  100 - (lpc_df$Tree_rnd + lpc_df$Shrub_rnd + lpc_df$Herb_rnd_crt), lpc_df$Other_rnd)
  # IF < 100%, ADD to other
  lpc_df$Other_rnd_crt =  ifelse(lpc_df$Tree_rnd + lpc_df$Shrub_rnd + lpc_df$Herb_rnd_crt + lpc_df$Other_rnd < 100, 
                                  100 - (lpc_df$Tree_rnd + lpc_df$Shrub_rnd + lpc_df$Herb_rnd_crt), lpc_df$Other_rnd_crt)
  
  # summary(lpc_df$Tree_rnd + lpc_df$Shrub_rnd + lpc_df$Herb_rnd_crt + lpc_df$Other_rnd_crt)
  
  lpc_bin_df = lpc_df[,c("id", "Tree_rnd","Shrub_rnd","Herb_rnd_crt","Other_rnd_crt")]
  
  return(lpc_bin_df)
}
# lpc_bin_df = lpc_bin(lpc_df = lpc_df, round_value = round_value)

# make_unique_rules
#' Make standard rule table
#'
#' @param lpc_bin_df Binned LPC data frame
#' @export
make_std_rules = function(lpc_bin_df) {
  # ------------------------------ MAKE MSR RULE ------------------------------
  unique_rules = unique(lpc_bin_df[,-"id"])
  unique_rules$rule = 1:nrow(unique_rules)
  # merge using data table 
  lpc_rules_df = lpc_bin_df[unique_rules, on = .(Tree_rnd, Shrub_rnd, Herb_rnd_crt, Other_rnd_crt),][order(id)]
  return(list(lpc_rules_df = lpc_rules_df, unique_rules = unique_rules))
}
# outlist = make_std_rules(lpc_bin_df)
# lpc_rules_df = outlist[[1]]
# unique_rules = outlist[[2]]

# make_rules_map
#' Create rules map raster
#'
#' @param lpc_rules_df Rules data frame
#' @param mask_map_path Path to mask raster
#' @param lpc_map_list List of LPC rasters
#' @export
make_rules_map = function(lpc_rules_df, mask_map_path, lpc_map_list) {
  Mode <- function(x) {
    ux <- unique(x[!is.na(x)])
    ux[which.max(tabulate(match(x, ux)))]
  }
  fill.nan <- function(x, i=5) {
    if(is.nan(x)[i]) {
      # cat("values in:", x, "\n")
      # cat("mode: ",Mode(x),"\n")
      return(Mode(x))
    } else {
      return(x[i])
    }
  }
  
  mask_map = rast(mask_map_path)
  
  rules_map = lpc_map_list$tc[[1]]
  values(rules_map)[!is.na(values(rules_map))] <- lpc_rules_df$rule
  
  # IF THERES MISSING DATA:
  # fill in data missing from lpc (which is present in mask) with mode of surrounding rules
  if (sum(is.na(values(rules_map)) & !is.na(values(mask_map))) > 0) {
    cat("Number of non-masked NA pixels:", sum(is.na(values(rules_map)) & !is.na(values(mask_map))),"\n")
    rules_map_filled = focal(rules_map, w = 3, fun = fill.nan, na.policy = "only")
    cat("Missing cells filled via mode of neighbors.\n") 
    cat("Remaining number of non-masked NA pixels:", sum(is.na(values(rules_map_filled)) & !is.na(values(mask_map))),"\n" )
  } else {
    rules_map_filled = rules_map
  }
  
  return(rules_map_filled)
}
# rules_map_std = make_rules_map(lpc_rules_df, mask_map_path, lpc_map_list)
# writeRaster(rules_map_std, output_rules_map, overwrite = T)

# LPC2std_veg_cover
#' Convert LPC to standard vegetation cover IDs
#'
#' @param lpc_df LPC data frame with cover percentages
#' @param mask_map_path Path to mask raster
#' @param lpc_map_list List of LPC rasters
#' @param vegid_tree Veg ID for tree class
#' @param vegid_shrub Veg ID for shrub class
#' @param vegid_herb Veg ID for herb class
#' @param vegid_other Veg ID for other class
#' @export
LPC2std_veg_cover = function(lpc_df, mask_map_path, lpc_map_list, vegid_tree = 1, vegid_shrub = 5, vegid_herb = 3, vegid_other = 4) {
  Mode <- function(x) {
    ux <- unique(x[!is.na(x)])
    ux[which.max(tabulate(match(x, ux)))]
  }
  fill.nan <- function(x, i=5) {
    if(is.nan(x)[i]) {
      # cat("values in:", x, "\n")
      # cat("mode: ",Mode(x),"\n")
      return(Mode(x))
    } else {
      return(x[i])
    }
  }
  
  max_i = apply(lpc_df[,1:4], 1, which.max)
  max_name = names(lpc_df)[max_i]
  lpc_df$max_ID = lpc_df$id
  lpc_df$max_ID[max_name == "Tree"] = vegid_tree
  lpc_df$max_ID[max_name == "Shrub"] = vegid_shrub
  lpc_df$max_ID[max_name == "Herb"] = vegid_herb
  lpc_df$max_ID[max_name == "Other"] = vegid_other
  
  mask_map = rast(mask_map_path)
  
  veg_cover = lpc_map_list$tc[[1]]
  
  # length(lpc_df$max_ID) == length(values(veg_cover)[!is.na(values(veg_cover))])
  values(veg_cover)[!is.na(values(veg_cover))] <- lpc_df$max_ID
  
  # IF THERES MISSING DATA:
  # fill in data missing from lpc (which is present in mask) with mode of surrounding rules
  if (sum(is.na(values(veg_cover)) & !is.na(values(mask_map))) > 0 ) {
    cat("Number of non-masked NA pixels:", sum(is.na(values(veg_cover)) & !is.na(values(mask_map))),"\n")
    veg_cover = focal(veg_cover, w = 3, fun = fill.nan, na.policy = "only")
    cat("Missing cells filled via mode of neighbors. Number of non-masked NA pixels:", sum(is.na(values(veg_cover)) & !is.na(values(mask_map))),"\n" )
  }
  
  return(veg_cover)
}

# write_rules
# vegid_XXXX to set the def file id for a given veg type from the LPC data
# MODIFY VEG ID, ADD 20 (PREFIX WITH 2)
#' Write RHESSys rules file from LPC-derived rules
#'
#' @param rules Data frame of rules and cover fractions
#' @param output_rules_file Path to write the rules file
#' @param vegid_tree Definition file ID for trees
#' @param vegid_shrub Definition file ID for shrubs
#' @param vegid_herb Definition file ID for herbs
#' @param vegid_other Definition file ID for other vegetation
#' @param thin_vegid_mod Offset to apply for thinned vegetation IDs
#' @param strata_ct Optional vector of strata counts per rule
#' @export
write_rules_file = function(rules, output_rules_file, vegid_tree = 1, vegid_shrub = 5, vegid_herb = 3, vegid_other = 4, thin_vegid_mod = 0, strata_ct = NULL) {
  
  fcon = file(output_rules_file,open = "wt")
  vals = as.data.frame(rules)[!names(rules) %in% c("id", "rule")]
  
  veg_ids = rep.int(NA, ncol(vals))
  veg_ids[grepl("tree|Tree", names(vals))] = vegid_tree
  veg_ids[grepl("shrub|Shrub", names(vals))] = vegid_shrub
  veg_ids[grepl("herb|Herb", names(vals))] = vegid_herb
  veg_ids[grepl("other|Other", names(vals))] = vegid_other
  
  if (thin_vegid_mod != 0) {
    thintf = grepl("thin", names(vals))
    veg_ids[thintf] = veg_ids[thintf]+thin_vegid_mod
  }
  
  if (is.null(strata_ct)) {
    strata_ct = rep.int(1, ncol(vals))
  }
  
  for (i in 1:nrow(rules)) {
    inc_cols = which(vals[i, ] != 0)
    text_out = c(paste0("ID\t",rules[i,"rule"]),
                 paste0("subpatch_count\t", length(inc_cols)),
                 paste0("_patch"),
                 paste0("\tpct_family_area\tvalue\t", paste(vals[i,inc_cols]/100,collapse = " | ")),
                 paste0("\t_canopy_strata\t",paste(strata_ct[inc_cols],collapse = " | ")),
                 paste0("\t\tveg_parm_ID\t",paste(veg_ids[inc_cols],collapse = " | ")),
                 ""
    )
    writeLines(text = text_out, con = fcon)
  }
  close(fcon)
}

# write_rules_file(
#   unique_rules,
#   output_rules_file,
#   vegid_tree = 1,
#   vegid_shrub = 5,
#   vegid_herb = 3,
#   vegid_other = 4,
#   strata_ct = NULL
# )

# make_thinning_rules
#' Create thinned vegetation rule table
#'
#' @param unique_rules Data frame of unique rules
#' @param tree_thin_pct Proportion of tree cover to thin
#' @param shrub_thin_pct Proportion of shrub cover to thin
#' @export
make_thin_rule = function(unique_rules, tree_thin_pct = 0, shrub_thin_pct = 0) {
  thin_rules = unique_rules
  # KEEP THE KEY VEG NAME  IN THE COL NAME (EG TREE, SHRUB, HERB, OTHER)
  if (tree_thin_pct > 0) {
    # these are modifications of the PERCENT COVER
    thin_rules$Tree_treat_thin = thin_rules$Tree_rnd * tree_thin_pct
    thin_rules$Tree_treat_remain = thin_rules$Tree_rnd * (1-tree_thin_pct)
    if (any(thin_rules$Tree_treat_remain + thin_rules$Tree_treat_thin != thin_rules$Tree_rnd)) {
      stop("something didnt add up when modifying tree cover")
    }
    thin_rules$Tree_rnd = NULL
  }
  if (shrub_thin_pct) {
    thin_rules$Shrub_treat_thin = thin_rules$Shrub_rnd * shrub_thin_pct
    thin_rules$Shrub_treat_remain = thin_rules$Shrub_rnd * (1-shrub_thin_pct)
    if (any(thin_rules$Shrub_treat_remain + thin_rules$Shrub_treat_thin != thin_rules$Shrub_rnd)) {
      stop("something didnt add up when modifying shrub cover")
    }
    thin_rules$Shrub_rnd = NULL
  }

  return(thin_rules)
}
 