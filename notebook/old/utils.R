if (!require("pacman")) install.packages("pacman")

pkgs = c(
  'plotly', 'ggthemes', 'hrbrthemes', 'ggsci', 'scales', 'extrafont',
  'lubridate', 'xts', 'tsbox', 'imputeTS', 'timetk',
  'urca', 'uroot', 'vars', 'forecast', 'dynlm', 'tsDyn',
  'kableExtra', 'gt', 'reshape2', 'readxl', 'xlsx', 'tidyverse'
)

pacman::p_load(pkgs, character.only=TRUE)

set_figsize = function(width, height) {
  options(
    repr.plot.width=width,
    repr.plot.height=height
  )
}

get_objname = function(x) deparse(substitute(x))

ur_adf = function(y, n_diff = 0, varname = NULL, ...) {
  
  varname = if (is.null(varname)) deparse(substitute(y)) else varname
  y = if(n_diff == 0) y else diff(y, n_diff)
  
  ur.trend = ur.df(y, type='trend', selectlags = "AIC", ...)
  ur.drift = ur.df(y, type='drift', selectlags = "AIC", ...)
  ur.none  = ur.df(y, type='none', selectlags = "AIC", ...)

  tstat.trend = ur.trend@teststat
  tstat.drift = ur.drift@teststat
  tstat.none  = ur.none@teststat

  cv.trend = ur.trend@cval
  cv.drift = ur.drift@cval
  cv.none  = ur.none@cval

  df_test = rbind(
    cbind(t(tstat.trend), cv.trend),
    cbind(t(tstat.drift), cv.drift),
    cbind(t(tstat.none) , cv.none)
  ) %>% 
  as.data.frame() %>%
  rownames_to_column("hypo") %>%
  mutate(
    result = ifelse(abs(statistic) >= abs(`5pct`), 'Reject', 'Accept'),
    variable = varname,
    level = paste0('d', n_diff),
    statistic = round(statistic, 2),
  ) %>%
  filter(str_starts(hypo, 'tau')) %>%
  select(variable, hypo, level, everything())
  
  return(df_test)
}