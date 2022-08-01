# check_worldfile

check_worldfile = function(worldfile) {
  world = read_world(worldfile)
  
  format_ids = function(ids) {
    if (length(ids) > 20) {
      return(paste0("[",length(ids)," total] ", paste0(ids[1:20], collapse = " "),"..." ) )
    } else {
      return(paste0("[",length(ids)," total] ", paste0(ids, collapse = " ")))
    }
  }
  
  basins = unique(world$ID[world$level=="basin"])
  hills = unique(world$ID[world$level=="hillslope"])
  zones = unique(world$ID[world$level=="zone"])
  patches = unique(world$ID[world$level=="patch"])
  
  patchsizes = unique(world$values[world$vars == "area" & world$level=="patch"])
  psizes = summary(factor(world$values[world$vars == "area" & world$level=="patch"]))
  psizesdf = data.frame(area_sq_m = names(psizes), count = unname(psizes))
  psizesdf = psizesdf[order(psizesdf$count, decreasing = T),]
  rownames(psizesdf) = seq_along(psizesdf$area)
  if (nrow(psizesdf) > 20) {
    psizesdf = psizesdf[1:20,]
  }
  
  famIDs = world$values[world$vars == "family_ID" & world$level=="patch"]
  if (length(famIDs) > 0) {
    famct = table(factor(famIDs))
    famidct = data.frame(table(factor(famct)))
    names(famidct) = c("Patches in family","Count")
    families = unique(famIDs)
  }
  
  # horizon check
  whors = world$values[world$vars == "w_horizon"]
  ehors = world$values[world$vars == "e_horizon"]
  
  # hour= 1 | cos_sza= -0.801208
  # hour= 2 | cos_sza= -0.721610
  # hour= 3 | cos_sza= -0.594985
  # hour= 4 | cos_sza= -0.429962
  # hour= 5 | cos_sza= -0.237787
  # hour= 6 | cos_sza= -0.031557
  # hour= 7 | cos_sza= 0.174673
  # hour= 8 | cos_sza= 0.366850
  # hour= 9 | cos_sza= 0.531876
  # hour= 10 | cos_sza= 0.658505
  # hour= 11 | cos_sza= 0.738108
  # hour= 12 | cos_sza= 0.765259
  # hour= 13 | cos_sza= 0.738108
  # hour= 14 | cos_sza= 0.658505
  # hour= 15 | cos_sza= 0.531876
  # hour= 16 | cos_sza= 0.366850
  # hour= 17 | cos_sza= 0.174673
  # hour= 18 | cos_sza= -0.031557
  # hour= 19 | cos_sza= -0.237787
  # hour= 20 | cos_sza= -0.429962
  # hour= 21 | cos_sza= -0.594985
  # hour= 22 | cos_sza= -0.721610
  # hour= 23 | cos_sza= -0.801208
  # hour= 24 | cos_sza= -0.828354
  
  
  # basin[0].hourly[0].hour_angle = (current_date.hour*3600-43200)*0.0041667*DtoR;
  # hr_angle_deg = (c(0:24)*3600-43200)*0.0041667
  # hr_angle = (c(0:24)*3600-43200)*0.0041667*0.01745329
  # 
  # hist(as.numeric(whors))
  # 
  # ang = seq(0,90,10)
  # 
  # rads = (pi/180)*ang
  # 
  # cos(rads)
  # sin(rads)
  # 
  #output
  cat("====================\n")
  cat("Worldfile Info\n")
  cat("====================\n")
  cat("File: ",worldfile,"\n")
  
  cat("Basins: ", format_ids(basins),"\n")
  cat("Hillslopes: ", format_ids(hills),"\n")
  cat("Zones: ", format_ids(zones),"\n")
  cat("Patches: ", format_ids(patches),"\n")
  
  cat("\n")
  if (length(famIDs) > 0) {
    cat("MSR IS being used.\n")
    cat("Patch Families: ", format_ids(families),"\n")
    print(famidct)
  } else {
    cat("MSR IS NOT being used. ")
  }
  
  cat("\nPatch Sizes (20 most common): \n")
  print(psizesdf)

  
}

