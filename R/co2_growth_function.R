#' CO2 change mean % since 1997
#'
#' @param dataf 
#' @param countryname 
#'
#' @return
#' @export
#'
#' @examples
co2_growth_function <- function(countryname) {
  b <- Greenhouse_Gas_Emissions %>%
    filter(year >= 1997) %>%
    group_by(country) %>%
    summarise(across(
      .cols = co2_growth_prct,
      .fns = list(Mean = mean), na.rm = TRUE, 
      .names = "mean_growth_co2")) %>%
    filter(country == countryname)
  cat("\n Mean % of annual increase or decrease of CO2 since 2010 in", countryname ,"\n")
  print(b)
}
