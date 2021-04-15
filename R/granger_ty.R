library(urca)
library(aod)
library(vars)
library(tidyverse)

granger_ty <- function(data, p = 1, m = 0, ...) {
  
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

# granger_ty(Canada, p = 2, m = 1) %>% select(-wald)