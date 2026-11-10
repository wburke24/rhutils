#' plotpdf_allvars
#' 
#' Reads, aggregates, and plots all output variables to a pdf. Assumes output using output filters. Optionally can aggregate, create summary statistics, hide legend. Uses a pattern input to select one or many csv outputs in an output directory. Currently designed to work with basin scale output.
#' @param out_dir Output directory to search for rhessys output files
#' @param out_name Base name of output pdf. Unique timedate is added to this string. If left null 'plots' is used.
#' @param out_path Path to folder where pdf will be written. If doesn't exist, will be created.
#' @param pattern Text pattern to use when searching for output files. Default is '_basin'.
#' @param step Time step for plots. Options are 'daily', 'monthly', or 'yearly'. Default is 'monthly'.
#' @param spatial_agg Optional spatial aggregation level. One of 'basin' (fully aggregated spatially), 'hill', 'zone', 'patch', 'stratum' (no spatial aggregation). If NULL (default) data is aggregated to basin.
#' @param worldfile Optional worldfile for veg ID spatial aggregation, required if spatial_agg is not null and set to veg_parm_ID.
#' @param pdfwidth Width for output pdf in inches.
#' @param pdfheight Height for output pdf in inches.
#' @param hide_legend TRUE/FALSE if legend should be hidden in output plot.
#' @param summary_plots TRUE/FALSE if data should be summarised instead of individual runs. 
#' @param output_pdf TRUE/FALSE if the main pdf should be written. Default is TRUE. Set to FALSE along with output_images = FALSE and return_plots = TRUE to only return the plot list.
#' @param output_images TRUE/FALSE if individual pngs should be output in addition to the pdf. PNGs are written to a folder next to the pdf, named like the pdf with an '_images' suffix. Default is FALSE.
#' @param return_plots TRUE/FALSE if ggplot objects should be returned (invisibly) as a named list, one per variable. Default is FALSE, in which case NULL is returned invisibly.
#' @param run_limit Number of runs which automatically trigger summary statistics instead of individual run plots.
#' @param auto_vars TRUE/FALSE to automatically generate relevent variables: evapotranspiration
#' @param runs Optional vector of run IDs, to subset the total dataset. Defaults to NULL. May not work
#' @param normalize_yearly TRUE/FALSE if yearly data should be normalized. Default is FALSE. Uses 365.25 days per year for normalization. NOTE: add usage of 'length' var in future 
#' 
#' @export

plotpdf_allvars = function(out_dir,
                           out_name = "plots",
                           out_path = "plots",
                           pattern = "_basin",
                           step = "monthly",
                           spatial_agg = NULL,
                           worldfile = NULL, # optional worldfile for veg ID spatial aggregation, required if spatial_agg is not null
                           pdfwidth = 10,
                           pdfheight = 7,
                           hide_legend = FALSE,
                           summary_plots = FALSE,
                           output_pdf = TRUE, # if FALSE, the main pdf is not written
                           output_images = FALSE, # if TRUE, also outputs a foldfer of individual pngs for each variable in addition to the pdf
                           return_plots = FALSE,
                           run_limit = 25,
                           auto_vars = TRUE,
                           runs = NULL,
                           normalize_yearly = FALSE
                          ) {
  
  # =========================== CHECKS ========================================
  if (!dir.exists(out_dir)) {
    stop("Output directory does not exist: ", out_dir)
  }
  if (!is.null(spatial_agg) && spatial_agg == "veg_parm_ID" && is.null(worldfile)) {
    stop("If spatial_agg is set to 'veg_parm_ID', a worldfile must be provided.")
  }
  if (!is.null(spatial_agg) && spatial_agg == "veg_parm_ID") {
    worldIDs = read_world_strata_veg_parm_IDs(worldfile)
    if (nrow(worldIDs) == 0) {
      stop("No valid worldIDs found in worldfile: ", worldfile)
    }
  }

  # ======================================== FIND OUTPUT FILES ========================================
  cat("Finding all files matching: '",pattern,"' in dir: ",out_dir,"\n", sep="")
  files_in = list.files(path = out_dir, pattern = pattern, full.names = T)
  if (length(files_in) == 0) {
    stop("No files found at specified output directory '",out_dir,"' using pattern '",pattern,"'")
  }
  names = gsub(pattern = pattern, "",basename(files_in))
  names = gsub(".csv","",names)

  if ((output_pdf || output_images) && !dir.exists(out_path)) {
    dir.create(out_path, recursive = TRUE)
    cat("Created output plot folder at path:", out_path,".\n")
  }

  # ======================================== READ + AGGREGATE DATA ========================================
  if (step == "daily") {
    timevars = c("year", "month", "day")
    cat("Using daily data (no temporal aggregation).\n")
  } else if (step == "monthly") {
    timevars = c("year", "month")
    cat("Aggregating to monthly.\n")
  } else if (step == "yearly") {
    timevars = c("year")
    cat("Aggregating to yearly.\n")
  } else {
    stop("Only valid steps are 'daily', 'monthly', or 'yearly'")
  }
  aggvars = timevars
  # Summary plot when runs are too many
  time_var <- if (step == "daily") "date" else if (step == "monthly") "year_month" else "year"
  time_var_nice <- if (step == "daily") "Date" else if (step == "monthly") "Year - Month" else "Year"
  # spatial aggreagation
  if (!is.null(spatial_agg)) {
    if (length(spatial_agg) != 1 | !any(spatial_agg %in% c("basin", "hill", "zone", "patch", "stratum", "veg_parm_ID"))) {
      stop("spatial_agg must be only one of: 'basin', 'hill', 'zone', 'patch', 'stratum', 'veg_parm_ID'")
    } 
    if (any(spatial_agg %in% c("basin", "hill", "zone", "patch", "stratum"))) {
      sp_arg_all = c("basin", "hill", "zone", "patch", "stratum")
      spag_all = c("basinID", "hillID", "zoneID", "patchID", "stratumID")
      spatial_agg_vars = spag_all[1:which(sp_arg_all == spatial_agg)]
      spatial_IDvar = spag_all[which(sp_arg_all == spatial_agg)]
      spatial_var_nice = paste0(toupper(substring(as.character(spatial_agg), 1, 1)),substring(as.character(spatial_agg), 2))
    } else if (spatial_agg == "veg_parm_ID") {
      spatial_agg_vars = "veg_parm_ID"
      spatial_IDvar = "veg_parm_ID"
      spatial_var_nice = "Vegetation ID"
    }
    cat("Aggregating spatially by:", paste(spatial_agg, collapse = ", "), "\n")
    aggvars = c(aggvars, spatial_agg_vars)
  } else {
    cat("Aggregating spatially to basin level.\n")
  }

  # read data
  DT_l = lapply(files_in, fread)
  vars = names(DT_l[[1]])[!names(DT_l[[1]]) %in% c("day", "month", "year", "basinID", "hillID", "zoneID", "patchID", "stratumID", "veg_parm_ID", "date", "sID", "run")]
  if (!is.null(spatial_agg) && spatial_agg == "veg_parm_ID") {
    DT_l = lapply(DT_l, function(DT) {
      DT = merge(DT, worldIDs, by.x = c("basinID", "hillID", "zoneID", "patchID", "stratumID"), by.y = c("basin_ID", "hillslope_ID", "zone_ID", "patch_ID", "stratum_ID"), all.x = T)
    })
  }
  # uses dynamic aggregation function
  DT_l = mapply(agg_dyn, DT_l, names, MoreArgs = list(aggvars = aggvars, vars = vars), SIMPLIFY = F)
  DT = data.table::rbindlist(DT_l)
  # ==================== AUTOMATICALLY ADD VARS ====================
  if (auto_vars) {
    # ----- EVAPOTRANSPIRATION -----
    etvars =c("evaporation", "evaporation_surf","transpiration_unsat_zone", "transpiration_sat_zone")
    if (all(etvars %in% vars)) {
      DT$evapotranspiration = DT$evaporation +
        DT$evaporation_surf +
        DT$transpiration_unsat_zone +
        DT$transpiration_sat_zone
      vars = c(vars, "evapotranspiration")
    }
    # ----- STREAM SOLUTES -----
    DOCvars = c("streamflow", "streamflow_DOC")
    if (all(DOCvars %in% vars)) {
      DT$stream_DOC_conc_ug_L = (DT$streamflow_DOC * 1000000) / DT$streamflow
      vars = c(vars, "stream_DOC_conc_ug_L")
    }
    DONvars = c("streamflow", "streamflow_DON")
    if (all(DONvars %in% vars)) {
      DT$stream_DON_conc_ug_L = (DT$streamflow_DON * 1000000) / DT$streamflow
      vars = c(vars, "stream_DON_conc_ug_L")
    }
  }
  
  # ==================== SUBSET RUNS ====================
  if (!is.null(runs)) {
    DT = DT[DT$run %in% runs,]
  }

  # get number of runs, do summary plots if too many runs
  run_ct = length(unique(DT$run))

  if (!is.null(spatial_agg)) {
    if (spatial_agg == "stratum") {
      # get last digit of stratumID as canopyID
      DT$canopyID = as.numeric(substr(DT$stratumID, nchar(DT$stratumID), nchar(DT$stratumID)))
      aggvars2= names(DT)[!names(DT) %in% c(vars, "basinID", "hillID", "zoneID", "patchID", "stratumID")]
      DT = DT[, lapply(.SD, mean), by = aggvars2, .SDcols = vars]
    } 
  }

  if (normalize_yearly) {
    if (step != "yearly") {
      warning("normalize_yearly is only valid for yearly step.")
    } else {
      DT = DT[, lapply(.SD, function(x) x / 365.25), by = c("run", spatial_agg_vars,"year"), .SDcols = vars]
    }
  }
  
  # ======================================== PLOT & WRITE TO PDF ========================================
  pdfname = file.path(out_path, paste0(gsub(".pdf","", out_name),"_", format(Sys.time(), "%Y-%m-%d--%H-%M-%S"), ".pdf"  ) )
  if (output_pdf) {
    pdf(pdfname, width = pdfwidth, height = pdfheight)
  }

  # folder for individual pngs (named after the pdf, without extension)
  if (output_images) {
    img_dir = sub("\\.pdf$", "_images", pdfname)
    dir.create(img_dir, recursive = TRUE, showWarnings = FALSE)
  }
  plot_list = list()

  # ======================================== VAR LISTS - FOR CONVERSIONS AND LABELS ========================================
  # hydro vars to convert to mm
  output_vars_hydro = c("evaporation","evaporation_surf", "exfiltration_sat_zone", "exfiltration_unsat_zone", "transpiration_unsat_zone","transpiration_sat_zone", "streamflow", "water_equivalent_depth", "gw.Qout", "rootzone.depth", "sat_deficit", "gw_drainage", "rz_storage", "rz_transfer", "unsat_storage", "unsat_transfer", "detention_store", "total_water_in", "litter.rain_stored", "gw.storage","base_flow", "canopy_rain_stored", "canopy_snow_stored", "evapotranspiration", "ET")
  
  # carbon vars, no conversion, label to kgC/m2
  output_vars_carbon = c(
    # soils
    "soil_cs.totalc", "soil_cs.DOC",
    # patch carbon
    "totalc",
    # growth
    "cs.net_psn", "cdf.psn_to_cpool",
    # strata carbon pools
    "cs.totalc", "cs.leafc", "cs.cpool", "cs.live_stemc", "cs.dead_stemc", "cs.live_crootc", "cs.dead_crootc", "cs.frootc", "cs.cwdc", "cs.cwdc_bg", "cs.gresp_transfer","cs.gresp_store",
    # litter
    "litter_cs.totalc", "litter_cs.totalc","litter_cs.litr1c_bg","litter_cs.litr2c_bg","litter_cs.litr3c_bg","litter_cs.litr4c_bg"
  )
  # nitrogen vars
  output_vars_nitrogen = c(
    "soil_ns.totaln", "soil_ns.DON"
  )
  
  # ======================================== ITERATE THROUGH VARS ========================================
  for (i in seq_along(vars)) {
    cat(paste0("Plotting variable: ",vars[i], " (",i," of ", length(vars),")\n") )

    if (vars[i] %in% output_vars_hydro) {
      ylabel = c("mm")
      DT[, (vars[i]) := get(vars[i]) * 1000]
    } else if (vars[i] %in% output_vars_carbon) {
      ylabel = c("kgC/m2")
    } else if (vars[i] %in% output_vars_nitrogen) {
      ylabel = c("kgN/m2")
    } else {
      ylabel = "M or kg/M2 or other"
    }
    
    # ======================================== SUMMARY STATS PLOTS ========================================
    if (run_ct > run_limit | summary_plots) {
      # ==================== GET SUMMARY STATS ====================
      DT_summary <- DT[,
        .(
          ymin = min(get(vars[i]), na.rm = TRUE),
          ymax = max(get(vars[i]), na.rm = TRUE),
          q25 = quantile(get(vars[i]), 0.25, na.rm = TRUE),
          q75 = quantile(get(vars[i]), 0.75, na.rm = TRUE),
          median = median(get(vars[i]), na.rm = TRUE),
          mean = mean(get(vars[i]), na.rm = TRUE)
        ),
        by = time_var
      ]

      # ==================== GET MIN, MEAN, MAX RUNS ====================
      mean_byrun <- DT[, .(mean = mean(get(vars[i]), na.rm = TRUE)), by = run]

      # tmp = DT[, .(get(vars[i]))]
      # length(tmp[[1]])
      # sum(is.nan(tmp[[1]]))
      # sum(is.infinite(tmp[[1]]))

      mean_vals <- mean_byrun$mean
      finite_idx <- is.finite(mean_vals)
      if (any(finite_idx)) {
        overall_mean <- mean(mean_vals[finite_idx], na.rm = TRUE)
        abs_diff <- abs(mean_vals - overall_mean)
        abs_diff[!is.finite(abs_diff)] <- Inf
        meanrun <- mean_byrun[which.min(abs_diff), "run"]
      } else {
        meanrun <- mean_byrun[1, "run"]
      }
      maxrun = mean_byrun[which.max(mean_byrun$mean) ,"run"]
      minrun = mean_byrun[which.min(mean_byrun$mean) ,"run"]

      meancol = "#64fc64"
      maxcol = "#fc6464"
      mincol = "#6464fc"
      
      # ==================== GGPLOT ====================
      tmpplot = ggplot(DT_summary, aes(x = .data[[time_var]])) +
        geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = "Min-Max"), alpha = 0.5) +
        geom_ribbon(aes(ymin = q25, ymax = q75, fill = "25th-75th Percentile"), alpha = 0.6) +
        geom_line(aes(y = mean, color = "Mean"), linewidth = 1) +
        # geom_line(aes(y = median, color = "Median"), linetype = "dashed", linewidth = 1) +
        scale_fill_manual(name = "Range", values = c("Min-Max" = "grey80", "25th-75th Percentile" = "grey50")) +
        scale_color_manual(name = "Statistic", values = c("Mean" = "black", "Mean Run" = meancol, 
      "Max Run" = maxcol, "Min Run" = mincol)) +
        guides(fill = guide_legend(order = 1), color = guide_legend(order = 2)) +
        geom_line(data = DT[run == meanrun,], aes(x = .data[[time_var]], y = .data[[vars[i]]], color = "Mean Run"), linewidth = 1) +
        geom_line(data = DT[run == maxrun,], aes(x = .data[[time_var]], y = .data[[vars[i]]], color = "Max Run" ), linewidth = 1) +
        geom_line(data = DT[run == minrun,], aes(x = .data[[time_var]], y = .data[[vars[i]]], color = "Min Run" ), linewidth = 1) +
        ggtitle(paste0(vars[i], " (Summary)")) +
        xlab(time_var_nice) +
        ylab(ylabel) + 
        theme_minimal()
      
      # ==================== Create label data.frames ====================
      value_col <- vars[i]
      mean_label <- DT[run == meanrun, .SD[.N], .SDcols = c(time_var, value_col)]
      setnames(mean_label, c(time_var, value_col), c("x", "y"))
      max_label  <- DT[run == maxrun, .SD[.N], .SDcols = c(time_var, value_col)]
      setnames(max_label, c(time_var, value_col), c("x", "y"))
      min_label  <- DT[run == minrun, .SD[.N], .SDcols = c(time_var, value_col)]
      setnames(min_label, c(time_var, value_col), c("x", "y"))
      # nudege labels to look nice
      len = max(DT[[time_var]], na.rm = T) - min(DT[[time_var]], na.rm = T)
      nudge = (len/40)
      lenv = max(DT[[value_col]], na.rm = T) - min(DT[[value_col]], na.rm = T)
      nudgev = lenv/40

      textmean = paste0("Run ",stringr::str_extract(meanrun,"(?<=_|ID)\\d+$") )
      textmax = paste0("Run ",stringr::str_extract(maxrun,"(?<=_|ID)\\d+$") )
      textmin = paste0("Run ",stringr::str_extract(minrun,"(?<=_|ID)\\d+$") )

      # add labels to plot
      tmpplot = tmpplot +
        geom_text(data = as.data.frame(mean_label), aes(x = x, y = y, label = textmean), hjust = 0, vjust = "bottom", color = meancol, inherit.aes = FALSE, nudge_x = -nudge, nudge_y = nudgev*2) +
        geom_text(data =  as.data.frame(max_label), aes(x = x, y = y, label = textmax), hjust = 0, vjust = "bottom", color = maxcol, inherit.aes = FALSE, nudge_x = -nudge, nudge_y = nudgev) +
        geom_text(data =  as.data.frame(min_label), aes(x = x, y = y, label = textmin), hjust = 0, vjust = "top", color = mincol, inherit.aes = FALSE, nudge_x = -nudge, nudge_y = -nudgev)
      
      # tmpplot

    } else {
      # ======================================== STANDARD INDIVIDUAL RUN PLOTS ========================================
      if (!is.null(spatial_agg) && spatial_agg == "stratum") {
        tmpplot = ggplot(DT) +
          aes(
            x = .data[[time_var]],
            y = .data[[vars[i]]],
            color = as.factor(run),
            linetype = as.factor(canopyID)
          ) +
          geom_line() +
          ggtitle(vars[i]) +
          xlab(time_var_nice) +
          ylab(ylabel) +
          labs(color = "Run", linetype = "Canopy ID")
      } else if (!is.null(spatial_agg) && spatial_agg == "veg_parm_ID") {
        tmpplot = ggplot(DT) +
          aes(
            x = .data[[time_var]],
            y = .data[[vars[i]]],
            color = as.factor(run),
            linetype = as.factor(veg_parm_ID)
          ) +
          geom_line() +
          ggtitle(vars[i]) +
          xlab(time_var_nice) +
          ylab(ylabel) +
          labs(color = "Run", linetype = "Veg Parm ID")
      } else {
      tmpplot = ggplot(DT) +
        aes(
          x = .data[[time_var]],
          y = .data[[vars[i]]],
          color = as.factor(run)
        ) +
        geom_line() +
        ggtitle(vars[i]) +
        xlab(time_var_nice) +
        ylab(ylabel) +
        labs(color = "Run")
      }
    }

    # tmpplot
    if (hide_legend) {
      tmpplot = tmpplot + theme(legend.position = "none")
    }

    # ==================== PLOT TO PDF ====================
    if (output_pdf) {
      plot(tmpplot)
    }

    # ==================== OPTIONAL PNG OUTPUT ====================
    if (output_images) {
      safe_name = gsub("[^A-Za-z0-9._-]", "_", vars[i])
      ggplot2::ggsave(
        filename = file.path(img_dir, paste0(safe_name, ".png")),
        plot = tmpplot,
        width = pdfwidth,
        height = pdfheight,
        dpi = 150,
        bg = "white"
      )
    }

    # ==================== OPTIONAL RETURN LIST ====================
    if (return_plots) {
      plot_list[[vars[i]]] = tmpplot
    }
  }

  # ==================== STOP GRAPHICAL OUTPUT TO PDF ====================
  if (output_pdf) {
    suppressMessages(dev.off())
    cat("Wrote plots to PDF file: ",pdfname,"\n")
  }
  if (output_images) {
    cat("Wrote individual pngs to folder: ", img_dir, "\n")
  }

  if (return_plots) {
    return(invisible(plot_list))
  }
  invisible(NULL)
}
  