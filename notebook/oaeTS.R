### Setup ###
if (!require("pacman")) install.packages("pacman")

pkgs = c('plotly', 'ggthemes', 'hrbrthemes', 
         'lubridate', 'xts', 'urca',
         'vars', 'forecast', 'imputeTS',
         'reshape2', 'readxl', 'tidyverse'
        )

pacman::p_load(pkgs, character.only=TRUE)




### Data Manipluation ###

months_th = c('ม.ค.'=1, 
              'ก.พ.'=2, 
              'มี.ค.'=3, 
              'เม.ย.'=4, 
              'พ.ค.'=5, 
              'มิ.ย.'=6,
              'ก.ค.'=7, 
              'ส.ค.'=8, 
              'ก.ย.'=9, 
              'ต.ค.'=10, 
              'พ.ย.'=11, 
              'ธ.ค.'=12
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


### Unit Root Test ###
adf_report = function(obj) {
  lag = (obj %>% slot("lags"))
  tstat = (t(obj %>% slot("teststat"))) %>% as.data.frame()
  colnames(tstat) = "tstat"
  cval = (obj %>% slot("cval")) %>% as.data.frame()
  colnames(cval) = c('cv01', 'cv05', 'cv10')
  res = data.frame(lag, tstat, cval)
  res['test'] = "ADF"
  res = res[1,]
  rownames(res) = NULL
  return(res)
}

kpss_report = function(obj) {
  lag = (obj %>% slot("lag"))
  tstat = obj %>% slot("teststat")
  cval = (obj %>% slot("cval")) %>% as.data.frame() %>% select(-3)
  colnames(cval) = c('cv10', 'cv05', 'cv01')
  res = data.frame(lag, tstat, cval)
  res['test'] = "KPSS"
  rownames(res) = NULL
  return(res)
}

unit_test = function(data, type=c("drift", "trend"), difference=FALSE) {
  kpss_map = list(drift = "mu", trend = "tau")
  col_names = colnames(data)
  res_1 = data.frame()
  res_2 = data.frame()
  
  for( i in seq_along(col_names) ) {
    ser = data[[col_names[i]]]
    ser = if (difference) diff(ser) else ser 
    level = if (difference) "d1" else "d0"
      
    adf = ur.df(ser, type = type, selectlags = "AIC")
    test_res = adf_report(adf)
    test_res['col_name']  = col_names[i]
    res_1 = rbind(res_1, test_res)
    
    kpss = ur.kpss(ser, type = kpss_map[[type]], lags = "long")
    test_res = kpss_report(kpss)
    test_res['col_name']  = col_names[i]
    res_2 = rbind(res_2, test_res)
    
    res = bind_rows(res_1, res_2)
    res['level'] = level
    res['type'] = type
  }
  return(res)
}

unit_test_report = function(data) {
  res1 = unit_test(data, type = "drift", difference = FALSE)
  res2 = unit_test(data, type = "trend", difference = FALSE)
  res3 = unit_test(data, type = "drift", difference = TRUE)
  res = bind_rows(res1, res2, res3)
  result = res %>%
    gather("key", "value", -col_name, -test, -level, -type) %>%
    mutate(key = paste(test, key, sep = "_")) %>%
    select(-test) %>%
    spread(key, value) %>%
    mutate(stationary_ADF  = ADF_tstat < ADF_cv05) %>%
    mutate(stationary_KPSS = KPSS_tstat < KPSS_cv05)  
  
  return(result)
}

set_figsize = function(height=4, width=8) {
  options(repr.plot.width=width, repr.plot.height=height)
}