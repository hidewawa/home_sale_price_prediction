##########################################################################################
##########################################################################################
# This script produces test bed for choosing best set of parameters based on rolling cross
# validation;
##########################################################################################
##########################################################################################
source('data_transformation_v2.R')
source('for_var.R')
source('for_per_hh.R')
source('rolling_testing.R')

# Define parameters to run on command line
args <- commandArgs(trailingOnly = TRUE)
b_year.p <- args[1]
b_month.p <- args[2]
e_year.p <- args[3]
e_month.p <- args[4]
lag.p <- args[5]
type.p <- args[6]
ahead.p <- args[7]
logged_v.p <- args[8]
ns_v.p <- args[9]
nd_v.p <- args[10]
per_hh.p < args[11]
test_var.p <- args[12]
b_yr_train.p <- args[13]
b_mn_train.p <- args[14]
e_yr_train.p <- args[15]
e_mn_train.p <- args[16]
e_yr_hist.p <- args[15]
e_mn_hist.p <- args[16]
test_var.p <- args[17]

# Load specific geo location data from database, default is national level, and create mts object
my_mts <- create_mts()
# run cross validation function to produce 
rt.s <-  rolling_test(
  my_mts,
  test_var = 'hus_sfhs_nar',
  b_yr_train = 2000,
  b_mn_train = 1,
  e_yr_train = 2013,
  e_mn_train = 12,
  e_yr_hist = e_year.p,
  e_mn_hist = e_month.p,
  agg_type = 'sum',
  test_ahead = ahead.p,
  per_hh = per_hh.p,
  norm.list = norm_ls.p,
  endo_v = endo_v.p,
  exo_v = exo_v.p,
  logged_v = logged_v.p,
  ns_v = ns_v.p,
  nd_v = nd_v.p,
  var_params = list(lag = lag.p, type = type.p,ahead = ahead.p)
)
