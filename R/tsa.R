# tsa_ur     all unit root test things
# tsa_var    all vars things
# tsa_vecm   all vecn things

library(magrittr)

# get object name as a string
obj_name <- function(x) deparse(substitute(x))


# unit root test for a time series
tsa_ur_adf <- function(y, n_diff = 0, varname = NULL, specs = c('trend', 'drift', 'none'), ...) {
  
  varname <- if (is.null(varname)) deparse(substitute(y)) else varname
  y <- if (tibble::is_tibble(y)) y %>% select(varname) %>% tsbox::ts_ts() else y
  y <- if(n_diff == 0) y else diff(y, n_diff)
  
  specs <- specs %>% rlang::set_names(specs)
  specs_seq <- seq_along(specs) %>% rlang::set_names(specs)
  res <- list()
  res$ur <- purrr::map(specs, function(.x) {
    urca::ur.df(y, type = .x, lags = 10, selectlags = "BIC", ...)
  })
  res$tstat <- purrr::map(specs_seq, ~res$ur[[.x]]@teststat)
  res$cv    <- purrr::map(specs_seq, ~res$ur[[.x]]@cval)
  res$lags  <- purrr::map(specs_seq, ~dim(res$ur[[.x]]@testreg$coefficients)[1] - (4-.x))
  
  purrr::map(specs_seq, ~cbind(t(res$tstat[[.x]]), res$cv[[.x]], res$lags[[.x]])) %>%
    purrr::reduce(rbind) %>%
    as.data.frame() %>%
    dplyr::rename("lag" = 5) %>%
    tibble::rownames_to_column("hypo") %>%
    dplyr::filter(str_starts(hypo, 'tau')) %>%
    dplyr::mutate(
      result = ifelse(abs(statistic) >= abs(`5pct`), 'Reject', 'Accept'),
      variable = varname,
      level = paste0('d', n_diff),
      star = dplyr::case_when(
        abs(statistic) > abs(`1pct`) ~ "***",
        abs(statistic) > abs(`5pct`) ~ "** ",
        abs(statistic) > abs(`10pct`) ~ "*  ",
        TRUE ~ "   "
      ),
      hypo = dplyr::case_when(
        hypo == "tau3" ~ "trend",
        hypo == "tau2" ~ "constant",
        hypo == "tau1" ~ "none"
      ),
      statistic = format(round(statistic, 3), nsmall = 3) %>% 
        as.character %>% stringr::str_pad(6, "left")
    ) %>%
    select(variable, hypo, level, everything())
}

# tsa_ur_adf(AirPassengers, n_diff = 1, specs = c('trend', 'drift'))


# unit root test in a table format
tsa_report_adf <- function(adf_test) {
  adf_test %>% 
    dplyr::mutate(
      stat_star = glue::glue(
        "{statistic}{star}[{lag}]"
      ) %>% as.character()
    ) %>%
    tidyr::pivot_wider(
      id_cols = c("variable"),
      names_from = c("level", "hypo"),
      values_from = c("stat_star")
    )
}

# tsa_ur_adf(AirPassengers, n_diff = 1, specs = c('trend', 'drift')) %>%
#   tsa_report_adf()


# diagnostic test for VAR model
tsa_VAR_diag <- function(var_obj) {
  res <- list()
  res$serial <- vars::serial.test(var_obj, lags.pt = 12, type = 'PT.asymptotic')
  res$arch   <- vars::arch.test(var_obj, lags.multi = 12)
  res$norm   <- vars::normality.test(var_obj)
  
  serial <- res$serial$serial
  arch   <- res$arch$arch.mul
  jb     <- res$norm$jb.mul$JB
  skew   <- res$norm$jb.mul$Skewness
  kurt   <- res$norm$jb.mul$Kurtosis
  
  list(serial, arch, jb, skew, kurt) %>% 
    purrr::map(~ unclass(.x) %>% data.frame()) %>% 
    purrr::reduce(bind_rows) %>% dplyr::as_tibble() %>%
    dplyr::select(method, "Chi-sq" = statistic, "df" = parameter, p.value)
}



# toda yamamoto approach for granger causality test
tsa_gc_ty <- function(data, p = 1, m = 0, ...) {
  
  # get information from input
  k <- ncol(data) # number of endo variables
  endo_names <- colnames(data) # name of endo variables
  
  # initialize a tibble containing x = causing variable and y = effect variables
  res <- expand.grid(x = endo_names, y = endo_names, stringsAsFactors = FALSE) %>%
    dplyr::filter(x != y) %>% tidyr::as_tibble()
  
  # fit VAR model with p+m lag
  fitted <- vars::VAR(data, p = p + m, ...)
  
  # compute index indicating position of coefficients associated with its own variable
  coef_idx <- purrr::set_names(1:k, endo_names) %>% 
    purrr::imap( ~ k*(1:p)-(k-.x) )

  res <- res %>% dplyr::mutate(
    b     = purrr::map(y, ~ fitted$varresult[[.]] %>% coef), # coefficient
    Sigma = purrr::map(y, ~ fitted$varresult[[.]] %>% vcov), # covariance
    Terms = coef_idx[x] %>% unname, # position for restricting values to 0
    wald  = purrr::pmap(list(b, Sigma, Terms), 
                        ~ aod::wald.test(b = ..1, Sigma = ..2, Terms = ..3)
                       ),
    chi2_stat = purrr::map_dbl(wald, ~ .$result[['chi2']][['chi2']]),
    chi2_df   = purrr::map_dbl(wald, ~ .$result[['chi2']][['df']]),
    chi2_pval = purrr::map_dbl(wald, ~ .$result[['chi2']][['P']]),
  ) %>% dplyr::select(-(b:Terms))
  attr(res, 'fitted') <- fitted
  return(res)
}

# tsa_gc_ty(Canada, p = 2, m = 1) %>% select(-wald)