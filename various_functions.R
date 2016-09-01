# log difference function
# metrics is a numeric vector. logged,ns,nd can be manually chosen, otherwise it will return by automatic test
# add the growth calculation,
log_diff_f <- function(metrics,
                       logged = NA,
                       ns = NA,
                       nd = NA) {
  require(lmtest)
  require(tseries)
  # metrics need to be a numeric vector
  x <- metrics
  # test heteroskedasticity p.value  < 0.05
  if (is.na(logged)) {
    if (gqtest(x ~ 1)$p.value <= 0.1) {
      logged <- TRUE
    } else{
      logged <- FALSE
    }
  }
  if (logged == TRUE) {
    xstar <- log(x)
    xi.ns <- xstar
  } else{
    xstar <- x
    xi.ns <- x
  }
  # test seasonality
  if (is.na(ns)) {
    ns <- nsdiffs(xstar, m = frequency(x), max.D = 1)
  }
  if (ns > 0) {
    xstar <- diff(xstar,
                  lag = frequency(x),
                  differences = ns)
    xi.nd <- xstar
  } else {
    xi.nd <- xi.ns
  }
  # test unit root
  if (is.na(nd)) {
    nd <- ndiffs(xstar, max.d = 1, test = 'adf')
  }
  if (nd > 0) {
    xstar <- diff(xstar, differences = nd)
  }
  # store all the operations have been done
  ops <- c(logged, ns, nd)
  return(list(
    xstar = xstar,
    logged = logged,
    ns = ns,
    nd = nd,
    xi.nd = xi.nd,
    xi.ns = xi.ns
  ))
}
# normalize function
# the function is used to calculate the normalized value by some other matrix
normalized <- function(obj.mt, num.ch, denom.ch) {
  obj.mt[, num.ch] <<- obj.mt[, num.ch] / obj.mt[, denom.ch]
}
# reverse normalized value
reverse_norm <- function(obj.mt, norm.ch, denom.ch) {
  obj.mt <- with(obj.mt, norm.ch = norm.ch * denom.ch)
  return(obj.mt)
}


# reverse differencing
# diff.mts is a differenced mts object, xi.ls store the original value for each var
reverse_diff <- function(diff.mts, var, ns, nd, logged, xi.ls) {
  #var <- 'hus_sfhp_nar'
  diff.series <- diff.mts[, var]
  # nd = 1
  #ns = 1
  #logged = F
  #xi.ls = diff_ls
  # start <- '2001-02/'
  if (nd > 0) {
    y <-
      diffinv(
        na.omit(diff.series),
        differences = nd,
        xi = xi.ls[[var]]$xi.nd[nd]
      )
  } else {
    y <- diff.series
  }
  if (ns > 0) {
    y <-
      diffinv(
        na.omit(y),
        differences = ns,
        lag = 12,
        xi = xi.ls[[var]]$xi.ns[1:12]
      )
  } else {
    y <- y
  }
  if (logged == TRUE) {
    y <- exp(y)
  } else {
    y <- y
  }
  y
}
#
# reverse_diff(diff_matrix,'us_sfhp_nar',start = '2001-02/',ns = 1,nd = 1,logged = F)
# reverse_diff(diff_matrix,'hus_unemployed',start = '2001-02/',ns = 1,nd = 1,logged = TRUE)
# reverse_diff(diff_matrix,'us_sfhs_nar',start = '2001-02/',ns = 1,nd = 1,logged = F)


# cbind a list of ts objects into mts
cbts <- function(list_ts) {
  ists <- sapply(list_ts, is.ts)
  if (!all(ists))
    stop("argument ", which(!ists), " is not a ts object")
  do.call(cbind, unlist(lapply(list_ts, as.list), recursive = F))
}

# return the year and date of p month ahead of y and m
months.plus <- function(y, m, p) {
  months <- (y * 12) + m
  mp <- (y * 12) + m + p
  yr <- mp %/% 12
  mn <- mp %% 12
  year <- yr
  if (mn == 0) {
    mn <- 12
    year <- year - 1
  }
  return(list(year = year, month = mn))
}

# function to switch between error types
error_cal <- function(pred_v, true_v, type) {
  switch(
    type,
    mae = mean(abs(pred_v - true_v)),
    rmse = (mean((
      pred_v - true_v
    ) ^ 2)) ^ (1 / 2),
    mpe = mean(100 * abs(pred_v - true_v) / true_v)
    
  )
}

# switch month from numeric to character
mmm <- function(mm) {
  mnm <-
    switch(
      mm,
      "1" = "Jan",
      "2" = "Feb",
      "3" = "Mar",
      "4" = "Apr",
      "5" = "May",
      "6" = "Jun",
      "7" = "Jul",
      "8" = "Aug",
      "9" = "Sep",
      "10" = "Oct",
      "11" = "Nov",
      "12" = "Dec"
    )
  
  return(mnm)
}
mmmm <- function(mm) {
  mnm <-
    switch(
      mm,
      "1" = "January",
      "2" = "February",
      "3" = "March",
      "4" = "April",
      "5" = "May",
      "6" = "June",
      "7" = "July",
      "8" = "August",
      "9" = "September",
      "10" = "October",
      "11" = "November",
      "12" = "December"
    )
  
  return(mnm)
}

## Function to concat and run as a command
ccx <- function(...)
{
  return(eval(parse(text = paste0(...))))
}

## Function to debug by showing or prining the command syntax to be run
cc <- function(...)
{
  print(paste0(...))
}
# add function to switch type of aggregation
agg_f <- function(x, type) {
  switch(type,
         sum = sum(x),
         mean = mean(x))
}

# take in a mts object, convert each column of mts to a growth rate
grr_f <- function(mts_object) {
  grr_ls <- lapply(mts_object, function(x) {
    # x_ts is the growth rate mts
    x_ts <- round(diff(x) / x[-length(x)], 6)
    # xi is the first NA value in the original mts_object for each of the object
    return(x_ts)
  })
  names(grr_ls) <- colnames(mts_object)
  return(grr_ls)
}

reverse_grr_f <- function(origin.mts, grr.mts) {
  # the first observation are normal numbers,
  o_start <- start(origin.mts)
  o_end <- end(grr.mts)
  stopifnot(ncol(origin.mts) == ncol(grr.mts))
  grr.mts <- sapply(grr.mts, function(x)
    1 + x)
  grr.mts <- rbind(origin.mts, grr.mts)
  cpr.mts <- sapply(ts(grr.mts), function(x)
    cumprod(x))
  cpr.mts <-
    ts(
      cpr.mts,
      start = o_start,
      end = o_end,
      frequency = frequency(origin.mts)
    )
  cpr.mts
  
}

data_trans <- function(object_str, date_format = '%Y-%m-%d',reseason = FALSE,digits = 1) {
  require(xts)
  # object_str is character variable,so first need to convert it to an object
  #object <- as.name(object_list[1])
  #date_format = '%Y-%m-%d'
  #digits <- 10000
  # attribute name
  object <- as.name(object_str)
  attr_name <-
    colnames(eval(object))[-which(colnames(eval(object)) %in% c('fips', 'date'))]
  obj_xts <-
    xts(as.numeric(eval(object)[, attr_name]), 
        order.by = as.yearmon(eval(object)$date,format = date_format))
  obj_xts <- obj_xts * digits
  if(reseason == TRUE) {
    obj_xts <- reseason(obj_xts)
  }
  names(obj_xts) <- attr_name
  return(obj_xts)
}

# ############# reseasoning function to add seasonality
# reseason <- function(v) {
#         # Please note that this function is dependant on data acquired by the SQL code below.
#         ccx("h", v, ".ts <<- ts(as.numeric(h", v, "$h", v, "), start=first(as.yearmon(h", v, "$date)), frequency=12)")
#         ccx(v, ".x <<- xts(", v, ", order.by = as.yearmon(", v, "$date))")
#         ccx('lh', v, '.ts <<- log(h', v, '.ts)')
#         ccx('lh', v, '.ts.stl <<- stl(lh', v, '.ts, s.window="periodic")')
#         ccx('h', v, '.ts.stl.seas <<- exp(lh', v, '.ts.stl$time.series[,1])')
#         ccx('h', v, '.seas.x <<- as.xts(h', v, '.ts.stl.seas)')
#         ccx('indexTZ(h', v, '.seas.x) <<- "UTC"')
#         ccx("if(last(month(as.yearmon(time(h", v, ".ts)))) == 12) {
#        year_to_use <<- last(year(as.yearmon(time(h", v, ".ts))))
#      } else {
#        year_to_use <<- last(year(as.yearmon(time(h", v, ".ts)))) -1
#      }")
#         ccx('h', v, '.seas.factors.x <<- h', v, '.seas.x["', year_to_use, '"]')
#         ccx('indexTZ(h', v, '.seas.factors.x) <<- "UTC"')
#         ccx(v, '.x$month <<- month(', v, '.x)')
#         ccx(v, '.x$year <<- year(', v, '.x)')
#         ccx('for(i in 1:nrow(', v, '.x)) {', v, '.x$', v, '_nsa[i] <<- as.numeric(as.numeric(', v, '.x$', v , '[i]) * h', v, '.seas.factors.x[month(', v, '.x$', v, '[i])]) }')
#         ccx(v, '.x$date <<- as.character(time(', v, '.x))')
# }
