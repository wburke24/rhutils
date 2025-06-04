# def pars utils

#  Read - Write ================================================================================

#' @export
read_def = function(def_file) {
  def_read = readLines(def_file, warn = FALSE)
  def_read = def_read[nchar(def_read) > 0]
  def_table_list =  strsplit(trimws(def_read), "\\s+")
  def_table <- as.data.frame(do.call(rbind, lapply(def_table_list, `length<-`, 2)), stringsAsFactors = FALSE)
  names(def_table)[1:2] = c("pars", "names")
  comments = lapply(def_table_list, function(x)ifelse(length(x) > 2, paste(x[3:length(x)], collapse = " "), NA ) )
  def_table_out = data.frame(def_table, comments = unlist(comments))
  return(def_table_out)
}

# ================================================================================
#' @export
read_pars_table = function(out_dir) {
  parstable = list.files(paste0(out_dir,"/params/"),pattern = "all_def_changes",full.names = T )
  if (length(parstable) == 0) {
    stop("Couldn't find any pars tables")
  } else if (length(parstable) > 1) {
    stop("More than 1 pars table found")
  }
  inputpars = read.csv(parstable)
  if (names(inputpars)[1] == "X") {
    inputpars = inputpars[,-1]
  }
  return(inputpars)
}

# ================================================================================
#' @export
defpars_csv2list = function(defpar_csv) {
  df = read.csv(defpar_csv)
  vcol = which(names(df) == "Variable")
  dcol = which(names(df) == "Def_file")
  r1col = max(vcol, dcol) + 1
  def_list = apply(df, 1, FUN = function(X){list(Def_file = unname(X[dcol]), Variable = unname(X[vcol]), Value = unname(X[r1col:length(X)]))} )
  return(def_list)
}

# ================================================================================
#' @export
write_param_table = function(input_def_pars, outfile_basename = "all_def_changes") {
  vars_defs = data.frame(Variable = sapply(input_def_pars, "[[", 2), Def_file = sapply(input_def_pars, "[[", 1))
  param_table = cbind(vars_defs, t(sapply(input_def_pars, "[[", 3)))
  names(param_table)[3:length(param_table[1,])] = paste0("Run_",c(1:(length(param_table[1,])-2)))
  outname = paste0(outfile_basename,"_" ,gsub( ":", ".", sub( " ", "_", Sys.time())), ".params"  )
  write.csv(param_table, file = outname)
  cat("Wrote table of parameter changes to: ", outname)
}
IOin_def_pars_simple()
# ================================================================================
#only works for single def pars
#' @export
write_updated_def_files = function(input_def_pars, input_hdr, filename_ext = NULL) {
  def_pars_df = data.frame(matrix(unlist(input_def_pars), nrow = length(input_def_pars), byrow = T))
  for (f in unique(def_pars_df$X1)) {
    # subset def file pars and put in format expected by the change_def_file function
    def_par_subset = data.frame(t(def_pars_df[def_pars_df$X1 == f,3]))
    names(def_par_subset) = def_pars_df[def_pars_df$X1 == f,2]
    new_file = change_def_file(def_file = f, par_sets = def_par_subset, file_name_ext = filename_ext)
  }
}

#  Transform ================================================================================

# makes a table from list - deprec
# #' @export
# get_param_table = function(input_def_pars) {
#   vars_defs = data.frame(variable = sapply(input_def_pars, "[[", 2), def_file = sapply(input_def_pars, "[[", 1))
#   if (length(input_def_pars[[1]][[3]]) == 1) {
#     param_table = cbind(vars_defs, sapply(input_def_pars, "[[", 3))
#   } else {
#     param_table = cbind(vars_defs, t(sapply(input_def_pars, "[[", 3)))
#   }
#   names(param_table)[3:length(param_table[1,])] = paste0("run_",c(1:(length(param_table[1,])-2)))
#   return(param_table)
# }

# ================================================================================
#' @export
defpars_list2df = function (defpars) {
  df = data.frame(Variable = sapply(defpars, "[[", 2), Def_file = sapply(defpars, "[[", 1))
  df = cbind(df, t( unname(as.data.frame(lapply(defpars, "[[", 3))) ))
  names(df)[3:length(df[1, ])] = paste0("Run_", c(1:(length(df[1, ]) - 2)))
  return(df)
}

# ================================================================================
# transpose the defpar df so parameters are the columns, and can be set to correct type num vs chr
#' @export
defpar_df_t2parcols = function(defpar_df) {
  df_t = as.data.frame(t(defpar_df[, names(defpar_df) != c("Parameter","File")]))
  colnames(df_t) = paste0(defpar_df$Parameter,"__",defpar_df$File)

  # if all are numeric, switch
  validnum = sapply(df_t,function(X) {all(!is.na(suppressWarnings(as.numeric(X)))) } )
  df_t[,validnum]
  df_t[,validnum] = sapply(df_t[,validnum], as.numeric)
  return(df_t)
}

#  Extract/Subset ================================================================================

#' @export
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

# ================================================================================
# remove duplicate def list entry
#' @export
defpar_rm_dup = function(def_list) {
  pars = paste0(sapply(def_list, "[[",1), "-", sapply(def_list, "[[",2))
  pars[duplicated(pars)]
  def_list[duplicated(pars)] = NULL
  return(def_list)
}

# ================================================================================
# fill pars with repeats based on max number of pars
#' @export
fill_rep_pars = function(pars_list) {
  npars = sapply(pars_list, function(X) {length(X[[3]])})
  fillpars = lapply(pars_list, function(X, Y) {if (length(X[[3]]) < Y){X[[3]] = rep_len(X[[3]], Y)}; return(X) }, max(npars))
}

# ================================================================================
# duplicate soil def pars for multiple soils
#' @export
dup_soil_pars = function(input_def_pars, input_hdr) {
  par_df = as.data.frame(t(sapply(input_def_pars, function(X){X[1:2]})))
  names(par_df) = c("Def_file", "Variable")
  par_df$values = lapply(input_def_pars,"[[", 3)

  if (length(input_hdr$soil_def) <= 1) {
    cat("One or less soil definition files listed in input header, cannot duplicate soil def pars.")
    return(input_def_pars)
  }
  if (length(input_hdr$soil_def) > 1 & all(input_hdr$soil_def %in% par_df$Def_file)) {
    cat("Multiple soil definition files already being modified in input definition pars, don't want to overwrite/unclear which to dup.")
    return(input_def_pars)
  }
  soil_i = which(par_df$Def_file %in% input_hdr$soil_def)
  def_cur = input_hdr$soil_def[input_hdr$soil_def %in% par_df$Def_file]
  def2dup = input_hdr$soil_def[!input_hdr$soil_def %in% par_df$Def_file]
  pars_added = lapply(def2dup, function(def2dup, cur_pars) {
    lapply(cur_pars, function(X,Y) {X[[1]] = Y; return(X)},def2dup)
  }, cur_pars = input_def_pars[soil_i])
  new_input_def_pars = c(input_def_pars, unlist(pars_added, recursive = F))
  return(new_input_def_pars)
}
# ================================================================================
#' @export
get_only_varied_defpars_list = function(defpars) {
  num_vals = unlist(lapply(defpars, FUN = function(X){length(X[[3]])}))
  if (all(num_vals == 1)) {
    cat("Only 1 value for all pars")
    return(defpars)
  }
  num_unique = unlist(lapply(defpars, FUN = function(X){length(unique(X[[3]]))}))
  if (all(num_unique == 1)) {
    cat("Def pars are all unique")
    return(defpars)
  }
  varied_pars = defpars[num_unique > 1]
  return(varied_pars)
}

# ================================================================================
# get new parlist based on id/num of existing par list
#' @export
defpar_extract_byrunnum = function(pars_list, runnum) {
  extfun = function(X,Y) {
    X[[3]] = X[[3]][Y]
    return(X)
  }
  newparlist = lapply(pars_list,extfun,runnum)
  return(newparlist)
}

# ================================================================================
# param from def file
#' @export
get_def_par = function(def_file, parameter = NULL) {
  tmp = read_def(def_file)
  if (is.null(parameter)) {
    cat("No parameter indicated, parameters in def file:\n")
    print(tmp$names)
    return(tmp$names)
  }
  if (sum(tmp$names == parameter) == 0) {
    cat("No parameter in file matching '",parameter,"'. Parameters in def file:\n")
    print(tmp$names)
    return(NULL)
  }
  parval = tmp[tmp$names == parameter,"pars"]
  return(parval)
}
# ================================================================================
# list of def file, param, values, from current def file
#' @export
make_par_list_from_def_file = function(def_file, parameters, defaults = NULL) {
  # defaults = "~/Repos/RHESSys-develop/rhessys/init/construct_stratum_defaults.c"
  def = read_def(def_file)

  if (!all(parameters %in% def$names)) {
    cat("Not all parameters found in def file, using defaults if supplied.\n")
    parameters_in_def = parameters[parameters %in% def$names]
    par_vals = mapply(get_def_par, def_file, parameters_in_def, USE.NAMES = F)
    param_df = data.frame(Variable = parameters_in_def, Value = par_vals)

    if (!is.null(defaults)) {
      missing_pars = parameters[!parameters %in% def$names]
      allpars = check_params(rh_file = defaults, def_file = def_file)
      if (!all(missing_pars %in% allpars$Name)) {
        cat("Not all missing parameters found in defaults - likely invalid.\n")
        missing_pars = missing_pars[missing_pars %in% allpars$Name]
      }
      tmp = allpars[allpars$Name %in% missing_pars, ]
      missing_par_df = data.frame(Variable = tmp$Name, Value = tmp$DefaultValue)
      param_df = rbind(param_df, missing_par_df)
    } else {
      cat("No defaults supplied, using only parameters found in def_file.\n")
      # parameters = parameters[parameters %in% def$names]
    }
  } else {
    par_vals = mapply(get_def_par, def_file, parameters, USE.NAMES = F)
    param_df = data.frame(Variable = parameters, Value = par_vals)
  }
  # make into a list for input into ioinR
  param_list = apply(param_df, 1, FUN = function(X,Y){list(Def_file = Y, Variable = unname(X[1]), Value = unname(X[2]))}, def_file )
  return(param_list)
}

#  Statistics/Param Manipulation ================================================================================
# mean of n def pars
#' @export
defpar_mean = function(X) {
  if (is.numeric(X[[3]])) {
    X[[3]] = mean(X[[3]])
  } else {
    X[[3]] = X[[3]][1]
  }
  return(X)
}

# ================================================================================
#' @export
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

# ================================================================================
#' @export
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

# ================================================================================
#' @export
pct_rng = function(X, pct) {
  x1 = (X-X*pct)
  x2 = (X+X*pct)
  return(c(x1, x2))
}

# ================================================================================
# create parameter set based on all combinations of varying input vars
#' @export
def_par_allcomb = function(defpars) {
  npars = lapply(X = defpars, FUN = function(X) {out = length(X[[3]]); return(out)})
  if (!any(npars > 1)) {
    cat("No pars to combine")
    return(defpars)
  }
  pars_mult = lapply(defpars[npars>1], "[[", 3)
  #pars_comb = expand.grid(as.list(unname(data.frame(pars_mult))))
  pars_comb = expand.grid(pars_mult, stringsAsFactors = F)
  defpars[npars>1] = mapply(function(X, Y) {X[[3]] = Y; return(X)}, defpars[npars>1], pars_comb, SIMPLIFY = F)
  defpars[npars==1] = lapply(defpars[npars==1], function(X, Y) {X[[3]] = rep.int(X[[3]], Y); return(X)}, length(pars_comb[[1]]))
  cat("Output def pars length: ", length(pars_comb[[1]]))
  return(defpars)
}

#  Sensetivity ================================================================================
# parameter sensetivity, for each output var
#' @export
pars_sens = function(out_dir, input_def_pars, sortby = "Mean") {

  sortops = c("Mean", "Max", "Min", "Avg Annual Max", "Avg Annual Min")
  if (!sortby %in% sortops) {
    stop(paste0("sortby arg must be one of: ", paste0(sortops, collapse = " ")))
  }
  sortnum = which(sortops == sortby)

  # ============================== GET DEF PARS IN USABLE FORMAT ==============================
  defpar_df = defpars_list2df(input_def_pars)
  defpar_df_t = as.data.frame(t(defpar_df[,c(3:ncol(defpar_df))]))
  names(defpar_df_t) = paste0(defpar_df$Parameter,"--", defpar_df$File)

  # ============================== GET RESPONSE VARS/METRICS ==============================
  sim_DT = get_basin_daily(out_dir)
  vars = names(sim_DT)[!names(sim_DT) %in% c("day", "month", "year", "basinID", "run", "date", "wy", "yd")]
  # Ensure sim_DT is a data.table
  setDT(sim_DT)
  # separate dfs is just easier somehow
  mean = sim_DT[, lapply(.SD, function(x) mean = mean(x, na.rm = TRUE)), by = run, .SDcols = vars]
  min = sim_DT[, lapply(.SD, function(x) min = min(x, na.rm = TRUE)), by = run, .SDcols = vars]
  max = sim_DT[, lapply(.SD, function(x) max = max(x, na.rm = TRUE)), by = run, .SDcols = vars]
  # ASSUME RUN ENDS WITH RUN NUMBER
  mean$run_num = as.numeric(str_extract(mean$run,"(?<=_)\\d+$"))
  mean = mean[order(mean$run_num),]
  min$run_num = as.numeric(str_extract(min$run,"(?<=_)\\d+$"))
  min = min[order(min$run_num),]
  max$run_num = as.numeric(str_extract(max$run,"(?<=_)\\d+$"))
  max = max[order(max$run_num),]
  # Compute annual max and min per run and year
  an_max = sim_DT[, lapply(.SD, function(x) annual_max = max(x, na.rm = TRUE)), by = .(run, year), .SDcols = vars]
  avg_an_max = an_max[, lapply(.SD, function(x) avg_annual_max = mean(x, na.rm = TRUE)), by = .(run), .SDcols = vars]
  an_min = sim_DT[, lapply(.SD, function(x) annual_min = min(x, na.rm = TRUE)), by = .(run, year), .SDcols = vars]
  avg_an_min = an_min[, lapply(.SD, function(x) avg_annual_min = mean(x, na.rm = TRUE)), by = .(run), .SDcols = vars]

  avg_an_max$run_num = as.numeric(str_extract(avg_an_max$run,"(?<=_)\\d+$"))
  avg_an_max = avg_an_max[order(avg_an_max$run_num),]
  avg_an_min$run_num = as.numeric(str_extract(avg_an_min$run,"(?<=_)\\d+$"))
  avg_an_min = avg_an_min[order(avg_an_min$run_num),]

  src_mean = lapply(vars, function(i) {src(X = defpar_df_t, y = mean[[i]])})
  names(src_mean) = vars
  src_min = lapply(vars, function(i) {src(X = defpar_df_t, y = min[[i]])})
  names(src_min) = vars
  src_max = lapply(vars, function(i) {src(X = defpar_df_t, y = max[[i]])})
  names(src_max) = vars
  src_avg_an_max = lapply(vars, function(i) {src(X = defpar_df_t, y = avg_an_max[[i]])})
  names(src_avg_an_max) = vars
  src_avg_an_min = lapply(vars, function(i) {src(X = defpar_df_t, y = avg_an_min[[i]])})
  names(src_avg_an_min) = vars

  out = mapply(
    function(avg, max, min, anmax, anmin, sortnum) {
      avgdf = avg$SRC
      avgdf$name = rownames(avgdf)
      rownames(avgdf) = NULL
      maxdf = max$SRC
      maxdf$name = rownames(maxdf)
      rownames(maxdf) = NULL
      mindf = min$SRC
      mindf$name = rownames(mindf)
      rownames(mindf) = NULL
      anmaxdf = anmax$SRC
      anmaxdf$name = rownames(anmaxdf)
      rownames(anmaxdf) = NULL
      anmindf = anmin$SRC
      anmindf$name = rownames(anmindf)
      rownames(anmindf) = NULL

      outdf = cbind(
        avgdf[order(avgdf$name),c(2,1)],
        maxdf[order(maxdf$name),1],
        mindf[order(mindf$name),1],
        anmaxdf[order(anmaxdf$name),1],
        anmindf[order(anmindf$name),1]
      )
      names(outdf) = c("Parameter", "Mean", "Max", "Min", "Avg Annual Max", "Avg Annual Min")
      outdf = outdf[order(abs(outdf[,sortnum + 1]), decreasing = T ),]
      return(outdf)
    },
    src_mean,
    src_max,
    src_min,
    src_avg_an_max,
    src_avg_an_min,
    sortnum,
    SIMPLIFY = F
  )
  return(out)

}