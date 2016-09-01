
# this is rolling test to do rolling period prediction and test prediction results based on the rest of the series
rolling_test <- function(mts_object,
                         b_yr_train = 2000,
                         b_mn_train = 1,
                         e_yr_train = 2013,
                         e_mn_train = 12,
                         e_yr_hist = 2015,
                         e_mn_hist = 12,
                         test_var,
                         agg_type = 'sum',
                         test_ahead = 12,
                         ...)
{
  # ... pass through arguments to for_per_hh
  # mts_object <- my_mts
  # b_yr_train = 2000
  # b_mn_train = 1
  # test_ahead = 12
  # e_yr_train = 2013
  # e_mn_train = 12
  # e_yr_hist <- 2015
  # e_mn_hist <- 12
  # agg_type = 'sum'
  # i = 1
  # tot is total number of obs
  tot_m <-
    e_yr_hist * 12 + e_mn_hist - 12 * b_yr_train - b_mn_train + 1
  # k is the minimun months needed to build the model.normally it's 80% of the whole historical series
  # it also can be divided by 12
  k <-
    e_yr_train * 12 + e_mn_train - 12 * b_yr_train - b_mn_train + 1
  # i_tot is total number of tests are needed
  i_tot <- tot_m - k - test_ahead + 1
  true_list <- c()
  pred_list <- c()
  # begin of the test period
  for (i in 1:i_tot) {
    print(i)
    # create a test
    e_yr_train_i <-
      months.plus(e_yr_train, e_mn_train, p = i - 1)[['year']]
    e_mn_train_i <-
      months.plus(e_yr_train, e_mn_train, p = i - 1)[['month']]
    # train model with e_yr_train,e_mn_train period
    f <-
      for_per_hh(mts_object,
                 b_year = b_yr_train,
                 b_month = b_mn_train,
                 e_year = e_yr_train_i,
                 e_month = e_mn_train_i,
                 ...)
    pred_start <-
      months.plus(e_yr_train_i, e_mn_train_i, p = 1)
    pred_end <-
      months.plus(e_yr_train_i, e_mn_train_i, p = 12)
    pred_value <-
      window(
        f[, test_var],
        start = c(pred_start[['year']], pred_start[['month']]),
        end = c(pred_end[['year']], pred_end[['month']]),
        frequency = frequency(f)
      )
    true_value <-
      window(
        mts_object[, test_var],
        start = c(pred_start[['year']], pred_start[['month']]),
        end = c(pred_end[['year']], pred_end[['month']]),
        frequency = frequency(f)
      )
    pred_value.agg <- agg_f(pred_value, agg_type)
    true_value.agg <- agg_f(true_value, agg_type)
    
    # compare the 12 month ahead predicted result with historical result
    true_list[paste0(e_yr_train_i, "-", e_mn_train_i)] <-
      true_value.agg
    pred_list[paste0(e_yr_train_i, "-", e_mn_train_i)] <-
      pred_value.agg
  }
  ## calculate three errors based on the RMSE,MAE,MPE
  mae <- mean(abs(true_list - pred_list))
  rmse <- mean((pred_list - true_list) ^ 2) ^ (1 / 2)
  mape <- mean(100 * abs(pred_list - true_list) / true_list)
  
  # output a vector include all the input vectors plus errors
  out <-
    c(
      test_variable = test_var,
      pred = pred_list,
      true = true_list,
      mae = mae,
      rmse = rmse,
      mape = mape,
      b_yr_train = b_yr_train,
      b_mn_train = b_mn_train,
      e_yr_train = e_yr_train,
      e_mn_train = e_mn_train,
      e_yr_hist = e_yr_hist,
      e_mn_hist = e_mn_hist,
      agg_type = agg_type,
      test_ahead = test_ahead,
      per_hh = per_hh.p,
      norm.list = norm_ls.p,
      endo_v = endo_v.p,
      exo_v = exo_v.p,
      logged_v = logged_v.p,
      ns_v = ns_v.p,
      nd_v = nd_v.p,
      lag = lag.p,
      type = type.p,
      ahead = ahead.p
    )
  return(out)
}
