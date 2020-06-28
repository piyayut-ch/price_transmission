### Setup ###
if (!require("pacman")) install.packages("pacman")

pkgs = c('plotly', 'ggthemes', 'hrbrthemes', 'IRdisplay', 
         'lubridate', 'xts', 'tsbox', 'imputeTS',
         'urca', 'uroot', 'vars', 'forecast', 'dynlm', 'tsDyn',
         'kableExtra', 'gt', 'reshape2', 'readxl', 'xlsx', 'tidyverse'
        )

pacman::p_load(pkgs, character.only=TRUE)

set_figsize = function (width, height){
  options(
	repr.plot.width=width,
	repr.plot.height=height
	)
}

### Data Manipluation ###
months_th = c('ม.ค.' = 1, 
              'ก.พ.' = 2, 
              'มี.ค.' = 3, 
              'เม.ย.'= 4, 
              'พ.ค.' = 5, 
              'มิ.ย.' = 6,
              'ก.ค.' = 7, 
              'ส.ค.' = 8, 
              'ก.ย.' = 9, 
              'ต.ค.' = 10, 
              'พ.ย.' = 11, 
              'ธ.ค.' = 12
             )

read_price_rice_fg = function(root, filename) {
  
  path = paste0(root, filename)
  sheets = excel_sheets(path)
  data = data.frame()

  for (i in sheets){
    dt =
      read_excel(path, skip=3, sheet=paste(i), na = c("-","",0)) %>%
      pivot_longer(-1, names_to='month_th', values_to='amount') %>%
      filter(month_th %in% names(months_th)) %>%
      mutate(year = as.numeric(i) - 543) %>%
      mutate(month = months_th[month_th]) %>%
      mutate(date = as.Date(paste("1", 
                                  as.character(month), 
                                  as.character(year), 
                                  sep="/"),
                            format='%d/%m/%Y')
            )
      data = rbind(data, dt)
  }
  
  names(data)[1] = 'place'
  
  return(data)
}


read_price_rice_ws = function(year) {
  root = '../data/rice/wholesale/ws'
  path = paste0(root, year, '.xls')
  sheets = tolower(month.abb)
  data = data.frame()
  
  for (i in seq_along(sheets)) {
    dt = 
      suppressMessages(read_excel(path, skip=1, 
                                  sheet=sheets[i], 
                                  na = c("-","",0)
                                 )
                      ) %>%
      rename_at(1, ~"price_id") %>%
      rename_at(2, ~"price_name") %>%
      pivot_longer(-(1:2), 
                   names_to = 'day', 
                   values_to = 'amount'
                  ) %>%
      filter(day %in% as.character(1:31)) %>%
      mutate(year = year - 543) %>%
      mutate(month = i) %>%
      mutate(amount = as.numeric(amount)) %>%
      drop_na(amount, day) %>%
      filter(amount>0) %>%
      mutate(date = as.Date(paste(day, 
                                  as.character(month), 
                                  as.character(year), 
                                  sep="/"),
                            format='%d/%m/%Y')
            )
    data = rbind(data, dt)
  }
  
  data = data %>%
      mutate(place = 'bangkok') %>%
      select(date, price_id, place, amount)
  
  return(data)
}


get_friday = function(year){
  first_day = ymd(paste0(year, "-01-01"))
  last_day = ymd(paste0(year, "-12-31"))
  x = seq(first_day, last_day, by="1 day")
  
  fridays = data.frame(date = x[wday(x)==6]) %>%
    mutate(month = month(date)) %>%
    mutate(year = year(date)) %>%
    group_by(month, year) %>%
    mutate(week_month = seq(n())) %>% ungroup()

  return(fridays)
}


read_price_rice_ex = function(year){
  path = paste0('../data/rice/export_th/ex', year, '.xlsx')
  sheets = tolower(month.abb)
  data = data.frame()
  fridays = get_friday(year-543)
  
  for (i in seq_along(sheets)){
    df =
      suppressMessages(read_excel(path, skip=2, 
                                  sheet=sheets[i], 
                                  na = c("-","",0))
                      ) %>%
      rename_at(1, ~"price_id") %>%
      rename_at(2, ~"price_name") %>%
      pivot_longer(-(1:2),
                   names_to='week_month', 
                   values_to='amount'
                  ) %>%
      mutate(week_month = as.numeric(week_month)) %>%
      drop_na(price_id, amount, week_month) %>%
      filter(amount>0) %>%
      mutate(year = year-543) %>%
      mutate(month = i)

    data = rbind(data, df)
  }
  
  data = data %>%
    mutate(place='thailand') %>% 
    left_join(fridays, by=c("week_month", "month", "year")) %>%
    select(date, price_id, place, amount)
  
  return(data)
}


lookup = function(x, table, key, val=NULL, alt=NULL) {
  
  x_exact = paste0("^", x, "$")
  val = if (is.null(val)) key else val
  key = table[[key]]
  alt = table[[alt]]
  val = table[[val]]
  
  cond = sum(grepl(x_exact, key))
  if (cond==1) {
    res = val[grepl(x_exact, key)]
  } else if (cond==0) {
    res = val[grepl(x, alt)]
    res = if (length(res)==0) "" else res
  } else {
    stop("error message")
  }
  
  return(res)
}


# ### Unit Root Test ###
# compare_cval = function (tstat, cval) {
#   rounded = format(tstat, digits = 2, nsmall=2)
#   x = case_when(
#    abs(tstat) >= abs(cval['cv01']) ~ paste0(rounded, '***'),
#    abs(tstat) >= abs(cval['cv05']) ~ paste0(rounded, '** '),
#    abs(tstat) >= abs(cval['cv10']) ~ paste0(rounded, '*  '),
#    TRUE ~ paste0(rounded, '   ') 
#   )
#   return(x)
# }


# star_tstat = function (tstat, cval=NULL, pval=NULL) {
#   rounded = format(tstat, digits = 2, nsmall=2)
  
#   if (!is.null(cval)) {
#     x = case_when(
#      abs(tstat) >= abs(cval['cv01']) ~ paste0(rounded, '***'),
#      abs(tstat) >= abs(cval['cv05']) ~ paste0(rounded, '** '),
#      abs(tstat) >= abs(cval['cv10']) ~ paste0(rounded, '*  '),
#      TRUE ~ paste0(rounded, '   ') 
#     )
#   } else if (!is.null(pval)) {
#     x = case_when(
#       pval <= 0.01 ~ paste0(rounded, '***'),
#       pval <= 0.05 ~ paste0(rounded, '** '),
#       pval <= 0.10 ~ paste0(rounded, '*  '),
#       TRUE ~ paste0(rounded, '   ')
#     )
#   }
  
#   return(x)
# }

# ur_test = function(y, transform = "log", difference = "d0", spec = "trend", n_lag = 6) {
  
#   y = if (transform == "log") log(y) else y
#   y = if (difference == "d1") diff(y) else y
#   y = y %>% na.omit()
  
#   adf_spec  = if (spec=='trend') 'trend' else 'drift'
#   kpss_spec = if (spec=='trend') 'tau' else 'mu'
  
#   test_adf  = urca::ur.df(  y, type = adf_spec, lags = n_lag, selectlags = "AIC")
#   test_ers  = urca::ur.ers( y, type = "DF-GLS", model = spec, lag.max = n_lag)
#   test_pp   = urca::ur.pp(  y, type = "Z-tau", model = spec, lags = "short")
#   test_kpss = urca::ur.kpss(y, type = kpss_spec, lags = "short")
  
#   res_adf  = test_adf  %>% ur_adf()
#   res_ers  = test_ers  %>% ur_ers()
#   res_pp   = test_pp   %>% ur_pp()
#   res_kpss = test_kpss %>% ur_kpss()

#   res = list(res_adf, res_ers, res_pp, res_kpss) %>% reduce(merge)
#   res['data'] = transform
#   res['level'] = difference
#   res['spec'] = spec
#   res = res[, c(5:7, 1:4)]
#   return(res)
# }


# ur_report = function(ys) {
#   transforms = c('none')
#   differences = c('d0', 'd1')
#   specs = c('trend', 'constant')
  
#   df = data.frame()
#   for (y_name in colnames(ys)) {
#     y = ys[, y_name]
#     df_y = data.frame()
#     for (transform in transforms) {
#       for (difference in differences) {
#         for (spec in specs) {
#           if (difference == "d1" && spec == "trend") next
#           res = ur_test(y, 
#                         transform = transform, 
#                         difference = difference, 
#                         spec = spec
#                        )
#           df_y = df_y %>% rbind(res)
#         }
#       }
#     }
#     df_y['variable'] = y_name 
#     df_y = df_y[, c(8, 1:7)]
#     df = df %>% rbind(df_y)
#   }
  
#   return(df)
# }