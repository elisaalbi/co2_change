#' Prediction for decades 90', 00', 10' for greenhouse gases
#'
#' @param pais 
#'
#' @return
#' @export
#'
#' @examples
prediccion_modelo_lineal <- function(pais){
  nombre_columna3 <- c("1990", "2000", "2010")
  nombre_columna4 <- c("1990", "2000", "2010", "2020")
  array_predict_co2 <- array(,c(length(pais), 4))
  array_predict_methane <- array(,c(length(pais), 3))
  array_predict_nitrous_oxide <- array(,c(length(pais), 3))
  rownames(array_predict_co2) <- pais
  colnames(array_predict_co2) <- nombre_columna4
  rownames(array_predict_methane) <- pais
  colnames(array_predict_methane) <- nombre_columna3
  rownames(array_predict_nitrous_oxide) <- pais
  colnames(array_predict_nitrous_oxide) <- nombre_columna3
  for(i in 1:length(pais)){
    array_predict_co2[i,] <- predict(lm(green_avg$co2_Mean[green_avg$country == pais[i]] ~ green_avg$decade[green_avg$country == pais[i]], green_avg))
    array_predict_methane[i,] <- predict(lm(green_avg$methane_Mean[green_avg$country == pais[i]] ~ green_avg$decade[green_avg$country == pais[i]], green_avg))
    array_predict_nitrous_oxide[i,] <- predict(lm(green_avg$nitrous_oxide_Mean[green_avg$country == pais[i]] ~ green_avg$decade[green_avg$country == pais[i]], green_avg))
  }
  cat("La prediccion de CO2 es:", "\n")
  print(array_predict_co2) 
  cat("\n","La prediccion de metano es:", "\n")
  print(array_predict_methane) 
  cat("\n","La prediccion de oxido nitroso es:", "\n")
  print(array_predict_nitrous_oxide)
}