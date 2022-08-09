# utils

library(data.table)
library(tidyverse)
library(cowplot)
library(png)
library(beepr)
library(stringr)
library(tictoc)
library(zoo)

options(scipen = 999)



# output rhessys formatted range of dates only including complete water years
wy_range = function(dates) {
  datetxt = as.Date(dates)
  df <- data.frame(date = datetxt,
                   year = as.numeric(format(datetxt, format = "%Y")),
                   month = as.numeric(format(datetxt, format = "%m")),
                   day = as.numeric(format(datetxt, format = "%d")))
  st = df[min(which(df$month == 10 & df$day == 1)),]
  ed = df[max(which(df$month == 9 & df$day == 30)),]
  date_out = c(paste(paste(st[,c(2:4)],collapse = " "), "01" ),
               paste(paste(ed[,c(2:4)],collapse = " "), "24" ))
  return(date_out)
}


clim_repeat = function(clim, file, n, units = "years") {

  if (is.character(clim)) {
    climdf = RHESSysIOinR::read_clim(clim_in = clim)
  } else if (is.data.frame(clim)) {
    climdf = clim
  }
  # trim to get only whole wy
  climdf = climdf[min(which(climdf$month == 10 & climdf$day == 1)):max(which(climdf$month == 9 & climdf$day == 30)),]
  # if years, make it a little long and trim at end, otherwise it's fine
  if (units %in% c("years", "year","Years", "Year")) {
    nyears = ceiling(lubridate::time_length(difftime(max(climdf$date), min(climdf$date)), "years"))
    nrep = ceiling(n/nyears)
  } else {
    nrep = n
  }
  dfout = do.call("rbind", replicate(nrep, climdf, simplify = FALSE))
  dfout$date = seq.POSIXt(from = min(dfout$date), by = "DSTday", length.out = length(dfout$date))
  dfout$date = as.POSIXlt(dfout$date)
  dfout$year = dfout$date$year + 1900
  dfout$month = dfout$date$mon + 1
  dfout$day = dfout$date$mday
  dfout$date = as.POSIXct(dfout$date)
  dfout$wy = data.table::fifelse(dfout$month >= 10, dfout$year + 1, dfout$year)
  if (units %in% c("years", "year","Years", "Year")) {
    dfout = dfout[1:which(dfout$month == 9 & dfout$day == 30 & dfout$wy == (min(dfout$wy) + n - 1)),]
  }

  header = paste(dfout$year[1], dfout$month[1], dfout$day[1], 1)
  out_suf = names(dfout)[!names(dfout) %in% c("date", "year", "month", "day", "wy", "yd", "wyd")]
  for (i in out_suf) {
    fname = paste0(file, ".", i)
    write(header, file = fname)
    data.table::fwrite(as.data.frame(dfout[,i]), file = fname, append = T)
  }

  if (is.character(clim)) {
    base_in = readLines(paste0(clim,".base"))
    base_out = gsub(clim, file, base_in)
    writeLines(base_out,con = paste0(file, ".base"))
  } else if (is.data.frame(clim)) {
    warning("Not enough info to make a basestation file, copy and update an existing one.")
  }

}

rm_dupe = function(def_list) {
  pars = paste0(sapply(def_list, "[[",1), "-", sapply(def_list, "[[",2))
  pars[duplicated(pars)]
  def_list[duplicated(pars)] = NULL
  return(def_list)
}

cleanup_wmfire = function(dir) {
  pat = "ETGridYear|FireFailedIterGridYear|FireSizes|FireSpreadIterGridYear|FireSpreadPropGridYear|LoadGridYear|PDefGridYear|
  PETGridYear|PLoadGridYear|PSlopeGridYear|PWindGridYear|RelDefGridYear|SoilMoistGridYear|UnderETGridYear|UnderPETGridYear|VegLoadGridYear"
  files = list.files(path = dir, pattern = pat, full.names = F)
  dirname = paste0("fire_grids_out_", gsub( ":", ".", sub( " ", "_", Sys.time())))
  dir.create(path = file.path(dir, dirname))
  cat("Created directory '",file.path(dir, dirname),"'\n", sep = "")
  shh = file.rename(from = file.path(dir, files), to = file.path(dir,dirname,files))
  cat("Moved fire grid output files to new directory\n")
}

collect_csvs = function(dir, dir_base = "rh_out_") {
  csv_files = list.files(path = dir, pattern = "*\\.csv")
  # make a folder with a unique name -  rh_out_date+time
  if (length(csv_files) > 0) {
    dirname = paste0(dir_base, gsub( ":", ".", sub( " ", "_", Sys.time())))
    dir.create(path = file.path(dir, dirname))
    cat("Created directory '",file.path(dir, dirname),"'\n", sep = "")
    shh = file.rename(from = file.path(dir, csv_files), to = file.path(dir,dirname,csv_files))
    cat("Moved RHESSys output files to new directory\n")
    return(file.path(dir, dirname))
  } else {
    cat("No csvs at specified directory.")
  }
}

collect_params = function(dir) {
  params_files = list.files(path = dir, pattern = "*\\.params")
  # make a folder with a unique name -  rh_out_date+time
  if (length(params_files) > 0) {
    dirname = paste0("params_", gsub( ":", ".", sub( " ", "_", Sys.time())))
    dir.create(path = file.path(dir,"params", dirname))
    cat("Created directory '",file.path(dir,"params", dirname),"'\n", sep = "")
    shh = file.rename(from = file.path(dir, params_files), to = file.path(dir, "params", dirname,params_files))
    cat("Moved RHESSys params files to new directory\n")
  } else {
    cat("No param files to move at specified directory.")
  }
}

meanpars = function(X) {
  if (is.numeric(X[[3]])) {
    X[[3]] = mean(X[[3]])
  } else {
    X[[3]] = X[[3]][1]
  }
  return(X)
}

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


tec_copy_redef = function(input_redef, redef_date, worldfile, overwrite = F) {
  if (is.character(redef_date) & length(redef_date) == 1) {
    redef_date = unlist(unname(strsplit(redef_date, "\\s+")))
  }
  filename = paste0(worldfile, ".Y", redef_date[1], "M", redef_date[2], "D", redef_date[3], "H", redef_date[4])
  file.copy(input_redef, filename, overwrite = overwrite)
  return(redef_date)
}

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}




# make def pars table from list
get_def_table = function(def_pars_list) {
  df = unname(as.data.frame(lapply(def_pars_list, "[[", 3)))
  names(df) = sapply(def_pars_list, function(X) {paste0(X[[1]],"_",X[[2]])})
  return(df)
}

worldstate = function(worldfile) {
  world_state = list.files(path = dirname(worldfile), pattern = paste0(basename(worldfile), ".*\\.state"), full.names = T)
  if (length(world_state) > 1) {
    warning("More than one worldfile .state files found:", world_state)
    return(NULL)
  } else if (length(world_state) == 0) {
    warning("No worldfile .state files found")
    return(NULL)
  } else {
    return(world_state)
  }
}


watbal_basin_of_multi = function(out_dir) {
  daily_dates = function(X, Y) {
    out = X
    out$run = Y
    out$year_month = zoo::as.yearmon(paste0(out$year,"-", out$month))
    out$ym_ind = as.numeric(out$year_month)
    return(out)
  }

  basinfiles = list.files(out_dir, "basin.csv", full.names = T)
  run_names = gsub("_basin.csv", "", basename(basinfiles))
  basin_list = lapply(basinfiles, fread)
  vars_in_basin = names(basin_list[[1]])[!names(basin_list[[1]]) %in%
                                           c("day", "month", "year", "basinID", "hillID", "zoneID", "patchID", "stratumID", "date", "run")]
  watbal = list()
  for (i in seq_along(basin_list)) {
    watbal[[i]] = watbal_basin_of(basin_list[[i]])
  }

  names(watbal) = run_names
  return(watbal)

}

runif_sample = function(X, n) {
  if (is.character(X[[3]]) | length(X[[3]]) == 1) {
    X[[3]] = rep.int(X[[3]], n)
    return(X)
  } else if (length(X[[3]]) == 2) {
    X[[3]] = runif(n, min = min(X[[3]]), max = max(X[[3]]))
    return(X)
  } else {
    print("There's too many values need another method here")
  }
}

unif_sample_all = function(par_ranges, n_pars_each) {
  npars = lapply(par_ranges, function(X) {length(X[[3]])})
  pars_mult = sapply(par_ranges[npars>1], "[[", 3)
  pars_seq = mapply(seq, from = pars_mult[1,], to = pars_mult[2,], length.out = n_pars_each, SIMPLIFY = F)
  pars_comb = expand.grid(pars_seq)
  par_ranges[npars>1] = mapply(function(X, Y) {X[[3]] = Y; return(X)}, par_ranges[npars>1], pars_comb, SIMPLIFY = F)
  par_ranges[npars==1] = lapply(par_ranges[npars==1], function(X, Y) {X[[3]] = rep.int(X[[3]], Y); return(X)}, length(pars_comb[[1]]))
  cat("Output def pars length: ", length(pars_comb[[1]]))
  return(par_ranges)
}


write_param_table = function(input_def_pars) {
  vars_defs = data.frame(variable = sapply(input_def_pars, "[[", 2), def_file = sapply(input_def_pars, "[[", 1))
  param_table = cbind(vars_defs, t(sapply(input_def_pars, "[[", 3)))
  names(param_table)[3:length(param_table[1,])] = paste0("run_",c(1:(length(param_table[1,])-2)))
  write_csv(param_table, file = paste0("all_def_changes_" ,gsub( ":", ".", sub( " ", "_", Sys.time())), ".params"  ) )
}

get_param_table = function(input_def_pars) {
  vars_defs = data.frame(variable = sapply(input_def_pars, "[[", 2), def_file = sapply(input_def_pars, "[[", 1))
  if (length(input_def_pars[[1]][3]) == 1) {
    param_table = cbind(vars_defs, sapply(input_def_pars, "[[", 3))
  } else {
    param_table = cbind(vars_defs, t(sapply(input_def_pars, "[[", 3)))
  }
  names(param_table)[3:length(param_table[1,])] = paste0("run_",c(1:(length(param_table[1,])-2)))
  return(param_table)
}


copy_param_by_var = function(pars, copy_var, replace_var) {
  tmp = pars[[which(lapply(pars,"[[",2 ) == copy_var)]]
  tmp[[2]] = replace_var
  if (tmp[[2]] %in% lapply(pars,"[[",2 )) {
    pars[[which(lapply(pars,"[[",2 ) == tmp[[2]])]] = tmp
  } else {
    pars[[length(pars)+1]] = tmp
  }
  return(pars)
}


# date2rhdate = function(date_in) {
#   if(class(date_in) == "Date") {
#     date_out = paste0(format(date_in, "%Y %m %d"), " 01")
#     return(date_out)
#   } else if (is.chron(date_in)) {
#     date_in
#
#   } else {
#     cat("Must be Date format")
#     return(NULL)
#   }
# }

write_world = function(world, path) {
  # world input should be output from read_world
  spacing = recode(world$level, "world" = "", "basin" = "\t", "hillslope" = "\t\t", "zone" = "\t\t\t", "patch" = "\t\t\t\t", "canopy_strata" = "\t\t\t\t\t")
  world_str = paste0(spacing, world$values, "\t\t\t" , world$vars)
  writeLines(text = world_str, con = path)
}


build_redefine2 = function(worldfile, out_file = NULL, vars = NULL, values = NULL, std_thin = NULL, patchID = NULL, strataID = NULL, veg_parm_ID = NULL) {
  # ---------- Check Arguments ----------
  #if (!file.exists(worldfile)) { stop(noquote(paste0("No file found at: ", worldfile))) }
  # if (file.exists(out_file)) {cat("File:",out_file,"will be overwritten.\n")}

  # Either need vars + values or std_thin
  if ((is.null(vars) | is.null(values)) & is.null(std_thin)) {
    stop(cat("Input is required for both `vars` and `values`, or `std_thin`"))
  }
  # if using vars + values - values must either be length of vars or 1
  if ((!is.null(vars) & !is.null(values)) && length(vars) != length(values) && length(values) != 1) {
    stop(cat("Input length mismatch:", length(vars), "input `vars` and", length(values),
             "input `values`. `length(values) == length(vars)` or `length(values) == 1`.\n"))
  }

  # read and parse
  if (is.character(worldfile) & length(worldfile) == 1) {
    # read to be used for final line by line replacement
    read_world = readLines(worldfile, warn = FALSE, encoding = "UTF-8")
    read_world = read_world[nchar(trimws(read_world)) > 0]
  } else {
    read_world = worldfile
  }

  world = read_world(world_path)

  world =  strsplit(trimws(read_world), "\\s+")
  world = data.frame(matrix(unlist(world), nrow = length(world), byrow = T), stringsAsFactors = FALSE)
  names(world) = c("values","vars")

  # ---------- Find Levels----------
  index_all = which(world$vars == "world_ID" | world$vars == "basin_ID" | world$vars == "hillslope_ID" |
                      world$vars == "zone_ID" | world$vars == "patch_ID" | world$vars == "canopy_strata_ID")
  index_names = gsub("_ID", "", x = world$vars[index_all])
  index_max = c(index_all[2:length(index_all)] - 1, length(world$vars))
  world$level = unname(unlist(mapply(rep, index_names, (index_max - index_all) + 1 )))
  world$ID = unname(unlist(mapply(rep, world$values[index_all], (index_max - index_all) + 1 )))

  # get unique ID - useful for queries/parsing later
  world$unique_ID = unname(unlist(mapply(rep, c(1:length(index_names)), (index_max - index_all) + 1 )))

  # get patch ID col that includes stratum
  world$patch_ID = world$ID
  world$patch_ID[world$level == "canopy_strata"] = NA
  world$patch_ID = zoo::na.locf(world$patch_ID)
  world$patch_ID[world$level %in% c("world", "basin", "hillslope", "zone")] = NA

  # thinning vars
  thin_vars =  c(
    "cs.cpool",
    "cs.leafc",
    "cs.dead_leafc",
    "cs.live_stemc",
    "cs.dead_stemc",
    "cs.live_crootc",
    "cs.dead_crootc",
    "cs.frootc"
  )

  other_thin_vars = c("cover_fraction", "gap_fraction", "cs.stem_density")

  # ---------- Thinning redefine ----------
  redef_index = NULL
  if (!is.null(std_thin)) {

    redef_strata = rep.int(TRUE, length(world$vars))
    redef_veg_strata = rep.int(TRUE, length(world$vars))
    redef_patch = rep.int(TRUE, length(world$vars))

    if (!is.null(patchID)) {
      # this only works if changing patch vars
      redef_patch = world$patch_ID %in% as.character(patchID)

    }
    if (!is.null(strataID)) {
      # functionality to support using just 1 or 2
      # if (all(nchar(strataID) == 1) & all(nchar(unique(world$ID[world$level == "canopy_strata"])) > 1)) {
      #   redef_strata = strata_IDs[substr(strata_IDs, nchar(strata_IDs), nchar(strata_IDs)) == as.character(strataID)]
      # }
      redef_strata = world$level == "canopy_strata" & world$ID %in% as.character(strataID)
    }
    if (!is.null(veg_parm_ID)) {
      redef_veg_strata = world$unique %in% world$unique[world$vars == "veg_parm_ID" & world$values %in% as.character(veg_parm_ID)]
    }

    redef_index = which(redef_patch & redef_strata & redef_veg_strata & (world$vars %in% thin_vars))
    if (length(redef_index) == 0) {redef_index = NULL}
    redef_values_old = world$values[redef_index]

    redef_values = as.character(rep.int(std_thin, length(redef_values_old)))

    if (!is.null(redef_index)) {
      read_world[redef_index] = unname(mapply(sub,paste0(redef_values_old,"[[:blank:]]"),paste0(redef_values,"\t"),read_world[redef_index]))
    }
  }

  # ---------- Find and Replace Vars ----------
  replace_index = NULL
  if (!is.null(vars) & !is.null(values)) {
    if (length(vars) > 1 & length(values) == 1) {
      values = rep.int(values, length(vars))
    }

    for (i in 1:length(vars)) {

      replace_index = which(world$vars == vars[i])
      if (length(replace_index) == 0) {stop(noquote("var to replace can't be found in worldfile.\n")) }

      # if unique values for every instance of var to be replaces were given, do nothing, otherwise repeat to get enough replacement values
      current_value = world$values[replace_index]
      if (length(values[i]) != length(replace_index)) {
        new_value = rep(values[i], length(replace_index)/length(values[i]))
      } else {
        new_value = values[i]
      }

      if (!is.null(replace_index)) {
        read_world[replace_index] = unname(mapply(sub,paste0(current_value,"[[:blank:]]"),paste0(new_value,"\t"),read_world[replace_index]))
      }
    }
  }


  if ( (is.null(redef_index) || all(!redef_index)) & (is.null(replace_index) || all(!replace_index)) ) {
    cat("No vars matched criteria, all set to -9999.\n")
  }

  # ---------- Replace all other values w -9999 ----------
  keep_vars = c(
    "world_ID",
    "basin_ID",
    "hillslope_ID",
    "zone_ID",
    "patch_ID",
    "canopy_strata_ID",
    "num_basins",
    "num_hillslopes",
    "num_zones",
    "num_patches",
    "num_canopy_strata",
    "num_stratum",
    "basin_n_basestations",
    "basin_basestation_ID",
    "hillslope_n_basestations",
    "hillslope_basestation_ID",
    "zone_n_basestations",
    "zone_basestation_ID",
    "patch_n_basestations",
    "patch_basestation_ID",
    "canopy_strata_n_basestations",
    "canopy_strata_basestation_ID"
  )
  keep_index = c(unique(redef_index, replace_index), which(world$vars %in% keep_vars))
  no_change_vars = c(1:length(read_world))[-keep_index]
  no_change_value = world$values[no_change_vars]

  read_world[no_change_vars] = unname(mapply(sub,paste0(no_change_value,"[[:blank:]]"),paste0("-9999","\t"),read_world[no_change_vars]))

  # ---------- Write file ----------
  if (!is.null(out_file)) {
    writeLines(text = read_world,out_file)
    cat("Successfully wrote redefine worldfile to",out_file,"\n")
  } else {
    cat("Successfully modified redefine worldfile and retuned within R\n")
    return(read_world)
  }


}





