require(RPostgreSQL)
if(!exists('con')||!isPostgresqlIdCurrent(con)){
  drv <- dbDriver("PostgreSQL")
  con <-
    dbConnect(drv,
              dbname = "forecast",
              port = 5432,
              user = 'postgres')
  
}
# Set up the database connection, run queries and convert to "xts" time series
#pull records from all different tables
ccx <- function(...)
{
  print(paste0(...))
  return(eval(parse(text=paste0(...))))
}

# query function
query_function <- function(variable = 'sfhp',
                           prefix = 'us',
                           fips = '00000',
                           suffix = '') {
  
  if (length(intersect(prefix, c('us', 'msa', 'county','hus','hmsa')))==0) {
    stop('wrong geographic level')
  } else {
    # meta data (need to update)
    # variable = 'sfhp'
    # prefix = 'us'
    #alt_run = ''
    # fips = '00000'
    #prefix = ''
    #suffix = 'nar'
    #prefix = 'msa'
    #fips = '41940'
    #variable = 'sfhp'
    #suffix <- ""
    if (prefix %in% c('us', 'msa','hus','hmsa')) {
      str_state <<-
        paste0(
          "select date,",
          prefix,
          "_",
          variable,
          ifelse(suffix == '','','_'),
          suffix,
          ", fips from ",
          prefix,
          "_",
          variable,
          ifelse(suffix == '','','_'),
          suffix,
          ' join msa_fips on ',
          prefix,
          "_",
          variable,
          ifelse(suffix == '','','_'),
          suffix,
          ".fips = msa_fips.\"CBSA Code\" where msa_fips.\"CBSA Code\" = '",
          fips,
          "'"
        )
      
    } else if (prefix == 'county') {
      str <-
        paste0(
          "select date,",
          prefix,
          "_",
          variable,
          ifelse(suffix == '','','_'),
          suffix,
          ", fips from ",
          prefix,
          "_",
          variable,
          ifelse(suffix == '','','_'),
          suffix,
          ' join county_fips on ',
          prefix,
          "_",
          variable,
          ifelse(suffix == '','','_'),
          suffix,
          ".fips = county_fips.stcountyfp where county_fips.stcountyfp = '",
          fips,
          "'"
        )
      
    }
    object_name <- paste0(prefix,'_',variable,ifelse(suffix=='','','_'),suffix)
    
    ccx(    
      object_name,
      " <<- ",
      "dbGetQuery(con,str_state)"
    )
    object_list <<- c(object_list,object_name)
    
  }
}







