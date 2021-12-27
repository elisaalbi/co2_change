#' Title
#'
#' @param datafr 
#' @param countryname 
#' @param gases 
#'
#' @return
#' @export
#'
#' @examples
function_model_gas <- function(countryname, gases) {
  gas_since_1997 <- Greenhouse_Gas_Emissions %>%
    select(country, year, co2, methane, nitrous_oxide) %>%
    filter(year >= 1997 & country == "China") %>%
    pivot_longer(co2:nitrous_oxide)
  dat <- gas_since_1997 %>%
    filter(name == gases)
  model <- lm(value ~ year, dat)
  p_plot <- plot(dat$year, dat$value, type = "l")
  new <- data.frame(cbind(year=seq(2021,2030,1)), row.names = seq(2021,2030,1))
  modelpredict <- predict(model, new)
  cat("\n This is the summary table of the linear model adjusted for the country",countryname,"for the gas",gases," \n")
  cat("\n Also shown a plot of the increase or decrease of the gas during this time \n")
  cat("\n And a prediciton for the gas for the next 10 years \n \n")
  return(list(summary_model = summary(model), plot = p_plot, prediction_next_decade = modelpredict))
}