# plotting helpers

avg_basin_output = function(file_in) {
  files = list.files(path = dirname(file_in), pattern = basename(file_in), full.names = T)
  data_list = lapply(files, fread)
  DT = rbindlist(data_list)[,lapply(.SD,mean), list(day, month, year, basinID)]
  return(DT)
}

basin_output_agg = function(DT) {
  DT$date = lubridate::ymd(paste(DT$year, DT$month, DT$day, sep = "-"))
  DT[, evaporation_total:=evaporation+evaporation_surf+exfiltration_sat_zone+exfiltration_unsat_zone]
  DT[, transpiration_total:=transpiration_sat_zone+transpiration_unsat_zone]
  DT[,c("evaporation","evaporation_surf", "exfiltration_sat_zone", "exfiltration_unsat_zone", "transpiration_sat_zone", "transpiration_unsat_zone"):=NULL]
}

# autoplot = function(DT, name = "basin_plots", dir = "plots", bitmap = F) {
#   vars = names(DT)[!names(DT) %in% c("day", "month", "year", "BasinID", "hillID", "zoneID", "patchID", "date")]
# }

basin_quadplots = function(DT, name = "basin_plots", dir = "plots", bitmap = F) {
  
  rzstor = DT %>% ggplot() + aes(x = date, y = rz_storage) + geom_line()
  rztrans = DT %>% ggplot() + aes(x = date, y = rz_transfer) + geom_line()
  satdef = DT %>% ggplot() + aes(x = date, y = sat_deficit) + geom_line()
  rzdepth = DT %>% ggplot() + aes(x = date, y = rootzone.depth) + geom_line()
  plot1 = plot_grid(rzstor,rztrans,satdef,rzdepth)
  
  totalc = DT %>% ggplot() + aes(x = date, y = totalc) + geom_line()
  netpsn = DT %>% ggplot() + aes(x = date, y = cs.net_psn) + geom_line()
  evap = DT %>% ggplot() + aes(x = date, y = evaporation_total) + geom_line()
  trans = DT %>% ggplot() + aes(x = date, y = transpiration_total) + geom_line()
  plot2 = plot_grid(totalc, netpsn,evap,trans)
  
  Qin = DT %>% ggplot() + aes(x = date, y = Qin) + geom_line()
  Qout = DT %>% ggplot() + aes(x = date, y = Qout) + geom_line()
  streamflow = DT %>% ggplot() + aes(x = date, y = streamflow) + geom_line()
  snowpack = DT %>% ggplot() + aes(x = date, y = snowpack.water_equivalent_depth) + geom_line()
  plot3 = plot_grid(Qin, Qout, streamflow, snowpack)
  
  outname = paste0(name, gsub( ":", ".", sub( " ", "_", Sys.time())),".pdf")
  
  if (!bitmap) {
    # vector but slow in pdf
    pdf(file.path(dir,outname))
    plot(plot1)
    plot(plot2)
    plot(plot3)
    dev.off()
  } else {
    
    png(filename = "plot1.png", width = 1000, height = 1000, res = 150)
    plot(plot1)
    dev.off()
    png(filename = "plot2.png", width = 1000, height = 1000, res = 150)
    plot(plot2)
    dev.off()
    png(filename = "plot3.png", width = 1000, height = 1000, res = 150)
    plot(plot3)
    dev.off()
    
    pdf(file.path(dir,outname))
    par(mar=rep(0,4))
    plot(c(0,1),c(0,1),type="n")
    rasterImage(readPNG("plot1.png"),0,0,1,1)
    plot(c(0,1),c(0,1),type="n")
    rasterImage(readPNG("plot2.png"),0,0,1,1)
    plot(c(0,1),c(0,1),type="n")
    rasterImage(readPNG("plot3.png"),0,0,1,1)
    dev.off()
    
    file.remove("plot1.png", "plot2.png", "plot3.png")
    
  }
  
}

aggfun = function(X, Y) {
  X[, sID := stratumID %% 10]
  out = X[, lapply(.SD, mean), by=c("year", "month", "sID"), .SDcols = vars]
  out$run = Y
  out$year_month = zoo::as.yearmon(paste0(out$year,"-", out$month))
  out$ym_ind = as.numeric(out$year_month)
  return(out)
}

avg_stratum_plots = function(file_in, name_out, dir = "plots", bitmap = T) {
  
  # read in data - this could be big but I think data.table should manage it fine
  DT_l = lapply(file_in, fread)
  vars = names(DT_l[[1]])[!names(DT_l[[1]]) %in% c("day", "month", "year", "basinID", "hillID", "zoneID", "patchID", "stratumID", "date", "sID", "run")]
  
  # aggregate asap
  run = seq_along(file_in)
  aggfun = function(X, Y) {
    X[, sID := stratumID %% 10]
    out = X[, lapply(.SD, mean), by=c("year", "month", "sID"), .SDcols = vars]
    out$run = Y
    out$year_month = zoo::as.yearmon(paste0(out$year,"-", out$month))
    out$ym_ind = as.numeric(out$year_month)
    return(out)
  }
  DT_l = mapply(aggfun, DT_l, run, SIMPLIFY = F)
  DT = rbindlist(DT_l)
  outname = paste0(name_out, gsub( ":", ".", sub( " ", "_", Sys.time())),".pdf")
  
  if (bitmap) {
    for (i in seq_along(vars)) {
      #tmpplot = DTavg %>% ggplot() + aes(x = date, color = as.factor(run), linetype =as.factor(sID)) + aes_string(y = vars[i]) + geom_line()
      tmpplot = DT %>% ggplot() + aes(x = year_month, color = as.factor(run), linetype =as.factor(sID)) + aes_string(y = vars[i]) + geom_line()
      png(filename = paste0(vars[i],"_tmp.png"), width = 1000, height = 1000, res = 150)
      plot(tmpplot)
      dev.off()
    }
    pdf(file.path(dir,outname))
    par(mar=rep(0,4))
    for (i in seq_along(vars)) {
      plot(c(0,1),c(0,1),type="n")
      rasterImage(readPNG(paste0(vars[i],"_tmp.png")),0,0,1,1)
    }
    dev.off()
    file.remove(paste0(vars,"_tmp.png"))
  }
}



plotpdf_allvars_strata = function(out_dir, name_out, plot_dir = "plots") {
  files_in = list.files(path = out_dir, pattern = ".*_stratum", full.names = T)
  names = gsub("stratum.csv", "", basename(files_in))
  DT_l = lapply(files_in, fread)
  vars = names(DT_l[[1]])[!names(DT_l[[1]]) %in% c("day", "month", "year", "basinID", "hillID", "zoneID", "patchID", "stratumID", "date", "sID", "run")]
  #run = seq_along(file_in)
  run = names
  aggfun = function(X, Y) {
    X[, sID := stratumID %% 10]
    out = X[, lapply(.SD, mean), by=c("year", "month", "sID"), .SDcols = vars]
    out$run = Y
    out$year_month = zoo::as.yearmon(paste0(out$year,"-", out$month))
    out$ym_ind = as.numeric(out$year_month)
    return(out)
  }
  DT_l = mapply(aggfun, DT_l, run, SIMPLIFY = F)
  DT = rbindlist(DT_l)
  vars = names(DT)[!names(DT) %in% c("day", "month", "year", "basinID", "hillID", "zoneID", "patchID", "stratumID", "date", "sID", "run", "ym_ind", "year_month")]
  
  pdfname = file.path(plot_dir,paste0(name_out, gsub( ":", ".", sub( " ", "_", Sys.time()))))
  if (!endsWith(pdfname, ".pdf")) {pdfname = paste0(pdfname, ".pdf")}
  
  pdf(pdfname)
  for (i in seq_along(vars)) {
    tmpplot = DT %>% ggplot() + aes(x = year_month, color = as.factor(run), linetype =as.factor(sID)) + aes_string(y = vars[i]) + geom_line()
    plot(tmpplot)
  }
  dev.off()
}


plotpdf_allvars_basin = function(out_dir, out_name, step = "monthly") {
  files_in = list.files(path = out_dir, pattern = ".*_basin", full.names = T)
  names = gsub("_basin.csv", "",basename(files_in))
  DT_l = lapply(files_in, fread)
  vars = names(DT_l[[1]])[!names(DT_l[[1]]) %in% c("day", "month", "year", "basinID", "hillID", "zoneID", "patchID", "stratumID", "date", "sID", "run")]
  
  if (step == "monthly") {
    aggfun = function(X, Y) {
      out = X[, lapply(.SD, mean), by=c("year", "month"), .SDcols = vars]
      out$run = Y
      out$year_month = zoo::as.yearmon(paste0(out$year,"-", out$month))
      out$ym_ind = as.numeric(out$year_month)
      return(out)
    }
    DT_l = mapply(aggfun, DT_l, names, SIMPLIFY = F)
  }
  DT = rbindlist(DT_l)
  pdfname = file.path("plots", paste0(gsub(".pdf","", out_name), gsub( ":", ".", sub( " ", "_", Sys.time())), ".pdf"  ))
  pdf(pdfname)
  for (i in seq_along(vars)) {
    tmpplot = DT %>% ggplot() + aes(x = year_month, color = as.factor(run), linetype =as.factor(run)) + aes_string(y = vars[i]) + geom_line() + ggtitle(vars[i])
    plot(tmpplot)
  }
  dev.off()
}
