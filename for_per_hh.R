for_per_hh <- function(mts_object,# 
                       per_hh = F,
                       norm.list,...) {
  # ... can be arguments that pass to for_var function
  if (per_hh == F) {
    f <-
      for_var(mts_object,...)
  } else {
    # predict household using for_var function
    # first need to figure out the historical start and end date for all three variables for household model
    his_start <-
      start(na.omit(mts_object[, c('hus_hh', 'hus_unemployed', 'us_population')]))
    his_end <-
      end(na.omit(mts_object[, c('hus_hh', 'hus_unemployed', 'us_population')]))
    # predict household
    hh_pred <-
      for_var(
        mts_object,
        endo_v = c('hus_hh', 'hus_unemployed', 'us_population'),
        b_year = his_start[1],
        b_month = his_start[2],
        e_year = his_end[1],
        e_month = his_end[2],
        var_params = list(
          lag = 12,
          type = 'both',
          ahead = 36
        )
      )[, 'hus_hh']
    # extend the series so that hh_pred will have the same length as original mts_object object
    hh_pred.ext <-
      window(
        hh_pred,
        start = start(mts_object),
        end = end(mts_object),
        extend = TRUE
      )
    mts_object[, 'hus_hh'] <- hh_pred.ext
    # normalize the attributes
    mts_object[, norm.list] <-
      mts_object[, norm.list] / mts_object[, 'hus_hh']
    # run the VAR model
    f <-
      for_var(
        mts_object,...
      )
    # return original series
    # household denominator, need to be as same length as f
    hh_denom <-
      window(hh_pred.ext,
             start = start(f),
             end = end(f))
    f[, norm.list] <- f[, norm.list] * hh_denom
  }
  return(f)
}