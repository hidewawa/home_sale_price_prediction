source('various_functions.R')
source('update_data_from_sql.R')

# this function should return a mts_object
create_mts <- function(fips = '00000',
                       level = 'us',
                       use.cl = F) {
  require(xts)
  require(lubridate)
  # create a empty list to store object names
  object_list <<- list()
  prefix <- level # prefix
  h_prefix <- paste0('h', prefix) # historical prefix
  if (level == 'us') {
    suffix <- 'nar'
  } else if (level == 'msa') {
    if (use.cl == T) {
      suffix <- 'deed'
    }
  }
  query_function(
    variable = 'sfhp',
    prefix = prefix,
    suffix = suffix,
    fips = fips
  )
  query_function(
    variable = 'sfhp',
    prefix = h_prefix,
    suffix = suffix,
    fips = fips
  )
  query_function(
    variable = 'sfhs',
    prefix = prefix,
    suffix = suffix,
    fips = fips
  )
  query_function(
    variable = 'sfhs',
    prefix = h_prefix,
    suffix = suffix,
    fips = fips
  )
  query_function(variable = 'gdp',
                 prefix = h_prefix,
                 fips = fips)
  query_function(variable = 'gdp',
                 prefix = prefix,
                 fips = fips)
  query_function(variable = 'laborforce',
                 prefix = h_prefix,
                 fips = fips)
  query_function(variable = 'laborforce',
                 prefix = prefix,
                 fips = fips)
  query_function(variable = 'hh',
                 prefix = prefix,
                 fips = fips)
  query_function(variable = 'hh',
                 prefix = h_prefix,
                 fips = fips)
  query_function(variable = 'population',
                 prefix = prefix,
                 fips = fips)
  query_function(variable = 'med_hh_inc',
                 prefix = prefix,
                 fips = fips)
  query_function(variable = 'med_hh_inc',
                 prefix = h_prefix,
                 fips = fips)
  query_function(variable = 'unemployed',
                 prefix = prefix,
                 fips = fips)
  query_function(variable = 'unemployed',
                 prefix = h_prefix,
                 fips = fips)
  query_function(variable = 'unmr',
                 prefix = prefix,
                 fips = fips)
  query_function(variable = 'unmr',
                 prefix = h_prefix,
                 fips = fips)
  query_function(variable = 'employment',
                 prefix = prefix,
                 fips = fips)
  query_function(variable = 'employment',
                 prefix = h_prefix,
                 fips = fips)
  query_function(variable = 'starts',
                 prefix = prefix,
                 fips = fips)
  query_function(variable = 'starts',
                 prefix = h_prefix,
                 fips = fips)
  query_function(variable = 'interest_rate')
  
  # remove duplicate if there is any
  # object_list is vector of all the names of objects created after the query functions
  object_list <- unique(object_list)
  # run the object list through data_trans function
  data_trans_list <- lapply(object_list, data_trans)
  # result of this step return a xts matrix
  data_trans_matrix <-
    Reduce(function(x, y)
      cbind(x, y), data_trans_list)
  # this part needs to be automated
  #data_trans_matrix$pop_working_age <- with(data_trans_matrix,us_population - us_population_0_4 - us_population_5_9 - us_population_10_14 - us_population_65_plus)
  #data_trans_matrix$nw_pop <- with(data_trans_matrix,pop_working_age - us_laborforce)
  # convert the xts matrix to mts object
  start <- start(data_trans_matrix)
  end <- end(data_trans_matrix)
  start_ts <- c(year(start), month(start))
  end_ts <- c(year(end), month(end))
  mts_object <-
    ts(data_trans_matrix,
       start = c(year(start), month(start)),
       frequency = 12)
  return(mts_object)
}
