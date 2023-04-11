# def pars utils

#' @export
write_param_table = function(input_def_pars) {
  vars_defs = data.frame(variable = sapply(input_def_pars, "[[", 2), def_file = sapply(input_def_pars, "[[", 1))
  param_table = cbind(vars_defs, t(sapply(input_def_pars, "[[", 3)))
  names(param_table)[3:length(param_table[1,])] = paste0("run_",c(1:(length(param_table[1,])-2)))
  write.csv(param_table, file = paste0("all_def_changes_" ,gsub( ":", ".", sub( " ", "_", Sys.time())), ".params"  ) )
}

#' @export
get_param_table = function(input_def_pars) {
  vars_defs = data.frame(variable = sapply(input_def_pars, "[[", 2), def_file = sapply(input_def_pars, "[[", 1))
  if (length(input_def_pars[[1]][[3]]) == 1) {
    param_table = cbind(vars_defs, sapply(input_def_pars, "[[", 3))
  } else {
    param_table = cbind(vars_defs, t(sapply(input_def_pars, "[[", 3)))
  }
  names(param_table)[3:length(param_table[1,])] = paste0("run_",c(1:(length(param_table[1,])-2)))
  return(param_table)
}

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

# remove duplicate def list entry
#' @export
defpar_rm_dup = function(def_list) {
  pars = paste0(sapply(def_list, "[[",1), "-", sapply(def_list, "[[",2))
  pars[duplicated(pars)]
  def_list[duplicated(pars)] = NULL
  return(def_list)
}

# fill pars with repeats based on max number of pars
#' @export
fill_rep_pars = function(pars_list) {
  npars = sapply(pars_list, function(X) {length(X[[3]])})
  fillpars = lapply(pars_list, function(X, Y) {if (length(X[[3]]) < Y){X[[3]] = rep_len(X[[3]], Y)}; return(X) }, max(npars))
}

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


# get_def_table = function(def_pars_list) {
#   df = unname(as.data.frame(lapply(def_pars_list, "[[", 3)))
#   names(df) = sapply(def_pars_list, function(X) {paste0(X[[1]],"_",X[[2]])})
#   return(df)
# }

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

#' @export
pct_rng = function(X, pct) {
  x1 = (X-X*pct)
  x2 = (X+X*pct)
  return(c(x1, x2))
}


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


# duplicate soil def pars for multiple soils
#' @export
dup_soil_pars = function(input_def_pars, input_hdr) {
  par_df = as.data.frame(t(sapply(input_def_pars, function(X){X[1:2]})))
  par_df$values = lapply(input_def_pars,"[[", 3)
  if (length(input_hdr$soil_def) <= 1) {
    cat("One or less soil definition files listed in input header, cannot duplicate soil def pars.")
    return(input_def_pars)
  }
  if (length(input_hdr$soil_def) > 1 & all(input_hdr$soil_def %in% par_df$V1)) {
    cat("Multiple soil definition files already being modified in input definition pars, don't want to overwrite/unclear which to dup.")
    return(input_def_pars)
  }
  soil_i = which(par_df$V1 %in% input_hdr$soil_def)
  def_cur = input_hdr$soil_def[input_hdr$soil_def %in% par_df$V1]
  def2dup = input_hdr$soil_def[!input_hdr$soil_def %in% par_df$V1]
  pars_added = lapply(def2dup, function(def2dup, cur_pars) {
    lapply(cur_pars, function(X,Y) {X[[1]] = Y; return(X)},def2dup)
  }, cur_pars = input_def_pars[soil_i])
  new_input_def_pars = c(input_def_pars, unlist(pars_added, recursive = F))
  return(new_input_def_pars)
}


# create parameter set based on all combinations of varying input vars
#' @export
def_par_allcomb = function(defpars) {
  npars = lapply(X = defpars, FUN = function(X) {out = length(X[[3]]); return(out)})
  if (!any(npars > 1)) {
    cat("No pars to combine")
    return(defpars)
  }
  pars_mult = sapply(defpars[npars>1], "[[", 3)
  pars_comb = expand.grid(as.list(unname(data.frame(pars_mult))))
  defpars[npars>1] = mapply(function(X, Y) {X[[3]] = Y; return(X)}, defpars[npars>1], pars_comb, SIMPLIFY = F)
  defpars[npars==1] = lapply(pars_list[npars==1], function(X, Y) {X[[3]] = rep.int(X[[3]], Y); return(X)}, length(pars_comb[[1]]))
  cat("Output def pars length: ", length(pars_comb[[1]]))
  return(defpars)
}
