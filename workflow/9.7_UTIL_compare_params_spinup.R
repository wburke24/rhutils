# compare params

library(RHESSysIOinR)
library(rhutils)

s1 = "defs/stratum_shrub.def"
s2 = "defs/veg_p301_shrub.def"

comp = compare_params(a = s1, b = s2, rh_construct_default_file = "~/Repos/RHESSys-develop/rhessys/init/construct_stratum_defaults.c")

pars = comp[comp$different_pars, ]

pars[is.na(pars[2]),2] = pars[is.na(pars[2]),4]
pars[is.na(pars[3]),3] = pars[is.na(pars[3]),4]

pars$diff = abs(as.numeric(pars[,2]) - as.numeric(pars[,3]))
pars$mean = apply(data.frame(a = as.numeric(pars[,2]), b = as.numeric(pars[,3])), 1, mean)
pars$pct_chg = signif(pars$diff/pars$mean, 2)*100

# print table
library(knitr)
kable(pars[order(pars$pct_chg, decreasing = T),], )


# biomass_carbon_ratio = 2.0
# # defaults
# # stem_density = 0.02
# root_growth_direction = 0.7
# root_distrib_parm = 19
# max_root_depth = 9999

# rootc = 1

# rz_depth = function(rootc, root_growth_direction, root_distrib_parm, biomass_carbon_ratio, max_root_depth) {
#     rootzonedepth = 3.0 *((biomass_carbon_ratio*rootc)^root_growth_direction) / root_distrib_parm
#     rootzonedepth = min(rootzonedepth, max_root_depth);
#     #rootzonedepth = min(rootzonedepth, soil_depth-0.0001);
#     rootzonedepth = max(rootzonedepth, 0.0);
#     return(rootzonedepth)
# }

# rz_depth(rootc, root_growth_direction, root_distrib_parm, biomass_carbon_ratio, max_root_depth)


# rzdepths_df = expand.grid(rootc = seq(0,20,0.5), growthdir = c(0.4,0.7, 1), rootdistrib = c(5, 19, 50, 100, 150))

# rzdepths_df$rootdepth = mapply(rz_depth, rzdepths_df$rootc, rzdepths_df$growthdir, rzdepths_df$rootdistrib, biomass_carbon_ratio, max_root_depth)

# library(ggplot2)

# ggplot(rzdepths_df) +
#     aes(x = rootc, y = rootdepth, colour = as.factor(rootdistrib), group = as.factor(rootdistrib)) +
#     geom_line() +
#     theme_minimal() +
#     labs(title = "Root Depth Equation faceted by Growth Direction (exponent)", y = "Root Depth (M)", x = "Root Carbon kg/m2", color = "Root Distrib Parm (divisor)") +
#     facet_wrap(vars(growthdir))

# library(esquisse)
# esquisser(rzdepths_df)
