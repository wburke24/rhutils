# preprocessing utils

#' @export
# plots the maps that you would input into rhessys preprocessing
input_map_plotpdf <- function(
  map_dir,
  template = NULL,
  out_name = "map_plots",
  dest = "preprocessing",
  map_exts = c(".tif|.tiff"),
  pdfwidth = 7,
  pdfheight = 7
) {
  map_paths <- list.files(path = map_dir, pattern = map_exts, full.names = T)
  map_names = basename(map_paths)
  maps <- lapply(map_paths, rast)

  # maplist = rbind(maplist,c("veg_parm_ID","NLCDv2.tif"))
  if (!is.null(template)) {
    maplist = RHESSysPreprocessing::template_read(template = template)[[5]]
    vegmaps = maplist[maplist[, 1] == "veg_parm_ID", 2]
    soilmap = maplist[maplist[, 1] == "soil_parm_ID", 2]
  }

  # make the pdf of maps
  pdfname <- file.path(
    dest,
    paste0(
      gsub(".pdf", "", out_name),
      "_",
      format(Sys.time(), "%Y-%m-%d"),
      ".pdf"
    )
  )

  pdf(file = pdfname, width = pdfwidth, height = pdfheight)
  for (i in seq_along(map_names)) {
    # if (map_names[i] %in% vegmaps) {
    #   nlcdcols = FedData::nlcd_colors()
    #   vals <- unique(maps[[i]])
    #   df <- as.data.frame(nlcdcols[nlcdcols$ID %in% unlist(vals), ])
    #   names(df)[names(df) == "ID"] = "value"

    #   terra::activeCat(
    #     maps[[i]],
    #     main = map_names[i],
    #     col = df[, c("value", "Color")],
    #     type = "classes"
    #   )
    # } else {
    #   terra::plot(maps[[i]], main = map_names[i])
    # }

    terra::plot(maps[[i]], main = map_names[i])

  }
  suppressMessages(dev.off())

  cat("Wrote pdf of map plots to ", pdfname, "\n")
}

#' @export
# prints a table and extra data on an input template
check_template = function(template) {
  tempin = RHESSysPreprocessing::template_read(template = template)
  mapsin = tempin[[5]]
  
  cat("==================================================\n")
  cat("============ Summary of Template File ============\n")
  cat("==================================================\n")
  
  strata_ct = tempin[[1]][[tempin[[3]][6]]][3]
  
  cat("Number of canopy strata: ",strata_ct,"\n")
  maps_unq = unique(mapsin)
  
  cat("\nTable of Input Maps: ")
  print(knitr::kable(maps_unq))

  if (any(duplicated(maps_unq))) {
    if (length(unique(mapsin[mapsin[,1]=="z",2]))) {
      cat("NOTE: Multiple different maps set for 'z' at different levels: ", unique(mapsin[mapsin[,1]=="z",2]),"\n")
    }
  }
  
  cat("\n==================================================\n")
}


# fill missing map data
#' @export
fill_missing_raster_data = function(target_raster, mask_map, iterations = 1, window = 3, fun = "mean") {
  if (!(fun == "mean" | fun == "mode")) {
    stop("fun can only be mean or mode")
  }

  if (window %%2== 0) {
    stop("window must be odd - its the side length of moving window to do mode over.")
  }

  Mode <- function(x) {
    ux <- unique(x[!is.na(x)])
    ux[which.max(tabulate(match(x, ux)))]
  }

  # fill.nan <- function(x, i=5) {
  #   if(is.nan(x)[i]) {
  #     return(Mode(x))
  #   } else {
  #     return(x[i])
  #   }
  # }

  fill.na <- function(x, i=5) {
    if(is.na(x)[i]) {
      if (fun == "mode"){
        return(Mode(x))
      } else {
        return(mean(x, na.rm=T))
      }
      
    } else {
      return(x[i])
    }
  }

  cat("Masking target raster using mask map.\n")
  r = mask(target_raster, mask_map)

  missing_cells = sum(is.na(values(r)) & !is.na(values(mask_map)))
  i = 0
  # IF THERES MISSING DATA:
  # fill in data missing from target (which is present in mask) with mode of surrounding rules, otionally iterate
  # if (sum(is.na(values(r)) & !is.na(values(mask_map))) > 0)
  # this is just doing it for everywhere thats na, then filling, this could be super slow, should probably just do it for the unmasked sections.

  while (missing_cells > 0 & i < iterations) {
    cat("Number of non-masked NA pixels:", missing_cells,"\n")
    r = focal(r, w = window, fun = fill.na, na.policy = "only",i = ceiling((window^2)/2))
    missing_cells = sum(is.na(values(r)) & !is.na(values(mask_map)))
    i = i+1

    cat("Missing cells filled via mode of neighbors.\n") 
    cat("Remaining number of non-masked NA pixels:", missing_cells,"\n" )
    cat("Remaining iterations: ",iterations-i,"\n")
  }
  r = mask(r, mask_map)
  missing_cells = sum(is.na(values(r)) & !is.na(values(mask_map)))
  cat("Re masking by mask map\n")
  cat("Double check missing cells: ", missing_cells,"\n")
  return(r)
}

# Convert basin raster to utm grid id
#' @export
basin2utm = function(basin) {
  if (!is.lonlat(basin)) {
    cat("Converting basin to lonlat assuming WGS84 proj\n")
    basin = project(basin, "EPSG:4326")
  }
  long = ext(basin)[1] + (ext(basin)[2] - ext(basin)[1])/2
  utm = unname((floor((long + 180)/6) %% 60) + 1)
  return(utm)
}