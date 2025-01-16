# preprocessing utils

#' @export
# plots the maps that you would input into rhessys preprocessing
input_map_plotpdf <- function(map_dir,
  template = NULL,
  out_name = "map_plots", 
  dest = "preprocessing", 
  map_exts = c(".tif|.tiff"),
  pdfwidth = 7, 
  pdfheight = 7) {

map_paths <- list.files(path = map_dir, pattern = map_exts, full.names = T)
map_names = basename(map_paths)
maps <- lapply(map_paths, rast)

# maplist = rbind(maplist,c("veg_parm_ID","NLCDv2.tif"))

if (!is.null(template)) {
maplist = RHESSysPreprocessing::template_read(template = template)[[5]]
vegmaps = maplist[maplist[,1] == "veg_parm_ID",2]
soilmap = maplist[maplist[,1] == "soil_parm_ID",2]
}

# make the pdf of maps
pdfname <- file.path(dest, paste0(
gsub(".pdf", "", out_name),"_",
format(Sys.time(), "%Y-%m-%d"), 
".pdf"
))

pdf(file = pdfname, width = pdfwidth, height = pdfheight)
for (i in seq_along(map_names)) {

if (map_names[i] %in% vegmaps) {
nlcdcols = FedData::nlcd_colors()
vals<-unique(maps[[i]])
df<-as.data.frame(nlcdcols[nlcdcols$ID %in% unlist(vals),])
names(df)[names(df)=="ID"] = "value"

plot(maps[[i]], main = map_names[i], col = df[,c("value","Color")], type="classes")
} else {
plot(maps[[i]], main = map_names[i])
}    
}
suppressMessages(dev.off())

cat("Wrote pdf of map plots to ", pdfname,"\n")

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