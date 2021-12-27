#' Title
#'
#' @param decada 
#' @param pais 
#'
#' @return
#' @export
#'
#' @examples
percentage_variation_decade <- function(decada, pais){
  
  decada_nueva_co2 <- c()
  decada_anterior_co2 <- c()
  incremento_co2 <- c()
  decada_nueva_methane <- c()
  decada_anterior_methane <- c()
  incremento_methane <- c()
  decada_nueva_nitrous_oxide <- c()
  decada_anterior_nitrous_oxide <- c()
  incremento_nitrous_oxide <- c()
  resultados <- list()
  resultados_matriz = matrix(,nrow=length(pais), ncol=3)
  rownames(resultados_matriz) <- pais
  colnames(resultados_matriz) <- c("CO2", "Metano", "Oxido nitroso")
  for(i in 1:length(pais)){
    if(decada == 1990){
      decada_nueva_co2[i] <- green_avg$co2_Mean[green_avg$country == pais[i] & green_avg$decade == 2000]
      decada_anterior_co2[i] <- green_avg$co2_Mean[green_avg$country == pais[i] & green_avg$decade == 1990]
      incremento_co2[i] <- ((decada_nueva_co2[i]-decada_anterior_co2[i])/decada_anterior_co2[i])*100
      decada_nueva_methane[i] <- green_avg$methane_Mean[green_avg$country == pais[i] & green_avg$decade == 2000]
      decada_anterior_methane[i] <- green_avg$methane_Mean[green_avg$country == pais[i] & green_avg$decade == 1990]
      incremento_methane[i] <- ((decada_nueva_methane[i]-decada_anterior_methane[i])/decada_anterior_methane[i])*100
      decada_nueva_nitrous_oxide[i] <- green_avg$nitrous_oxide_Mean[green_avg$country == pais[i] & green_avg$decade == 2000]
      decada_anterior_nitrous_oxide[i] <- green_avg$nitrous_oxide_Mean[green_avg$country == pais[i] & green_avg$decade == 1990]
      incremento_nitrous_oxide[i] <- ((decada_nueva_nitrous_oxide[i]-decada_anterior_nitrous_oxide[i])/decada_anterior_nitrous_oxide[i])*100
      resultados[[i]] <- c(incremento_co2[i], incremento_methane[i], incremento_nitrous_oxide[i])
      resultados_matriz[i,] <- c(incremento_co2[i], incremento_methane[i], incremento_nitrous_oxide[i])
      cat("El incremento porcentual de CO2 del pais", pais[i], "es", resultados[[i]][1], ",el incremento porcentual de metano es", resultados[[i]][2],"y el incremento porcentual de ?xido n?trico es",resultados[[i]][3], "\n")
    }
    else if(decada == 2000){
      decada_nueva_co2[i] <- green_avg$co2_Mean[green_avg$country == pais[i] & green_avg$decade == 2010]
      decada_anterior_co2[i] <- green_avg$co2_Mean[green_avg$country == pais[i] & green_avg$decade == 2000]
      incremento_co2[i] <- ((decada_nueva_co2[i]-decada_anterior_co2[i])/decada_anterior_co2[i])*100
      decada_nueva_methane[i] <- green_avg$methane_Mean[green_avg$country == pais[i] & green_avg$decade == 2010]
      decada_anterior_methane[i] <- green_avg$methane_Mean[green_avg$country == pais[i] & green_avg$decade == 2000]
      incremento_methane[i] <- ((decada_nueva_methane[i]-decada_anterior_methane[i])/decada_anterior_methane[i])*100
      decada_nueva_nitrous_oxide[i] <- green_avg$nitrous_oxide_Mean[green_avg$country == pais[i] & green_avg$decade == 2010]
      decada_anterior_nitrous_oxide[i] <- green_avg$nitrous_oxide_Mean[green_avg$country == pais[i] & green_avg$decade == 2000]
      incremento_nitrous_oxide[i] <- ((decada_nueva_nitrous_oxide[i]-decada_anterior_nitrous_oxide[i])/decada_anterior_nitrous_oxide[i])*100
      resultados[[i]] <- c(incremento_co2[i], incremento_methane[i], incremento_nitrous_oxide[i])
      resultados_matriz[i,] <- c(incremento_co2[i], incremento_methane[i], incremento_nitrous_oxide[i])
      cat("El incremento porcentual de CO2 del pais", pais[i], "es", resultados[[i]][1], ",el incremento porcentual de metano es", resultados[[i]][2],"y el incremento porcentual de ?xido n?trico es",resultados[[i]][3], "\n")
    }
    else if(decada == 2010){
      decada_nueva_co2[i] <- green_avg$co2_Mean[green_avg$country == pais[i] & green_avg$decade == 2020]
      decada_anterior_co2[i] <- green_avg$co2_Mean[green_avg$country == pais[i] & green_avg$decade == 2010]
      incremento_co2[i] <- ((decada_nueva_co2[i]-decada_anterior_co2[i])/decada_anterior_co2[i])*100
      decada_nueva_methane[i] <- green_avg$methane_Mean[green_avg$country == pais[i] & green_avg$decade == 2020]
      decada_anterior_methane[i] <- green_avg$methane_Mean[green_avg$country == pais[i] & green_avg$decade == 2010]
      incremento_methane[i] <- ((decada_nueva_methane[i]-decada_anterior_methane[i])/decada_anterior_methane[i])*100
      decada_nueva_nitrous_oxide[i] <- green_avg$nitrous_oxide_Mean[green_avg$country == pais[i] & green_avg$decade == 2020]
      decada_anterior_nitrous_oxide[i] <- green_avg$nitrous_oxide_Mean[green_avg$country == pais[i] & green_avg$decade == 2010]
      incremento_nitrous_oxide[i] <- ((decada_nueva_nitrous_oxide[i]-decada_anterior_nitrous_oxide[i])/decada_anterior_nitrous_oxide[i])*100
      resultados[[i]] <- c(incremento_co2[i], incremento_methane[i], incremento_nitrous_oxide[i])
      resultados_matriz[i,] <- c(incremento_co2[i], incremento_methane[i], incremento_nitrous_oxide[i])
      cat("El incremento porcentual de CO2 del pais", pais[i], "es", resultados[[i]][1], ",el incremento porcentual de metano es", resultados[[i]][2],"y el incremento porcentual de ?xido n?trico es",resultados[[i]][3], "\n")
    }
    else if(decada == 2020){
      print("No hay datos suficientes como para evaluar la decada del 2020")
    }
    else{
      print("El formato para introducir los parametros no es el adecuado. Recuerde usar comillas para introducir el nombre del pais y, en caso de ser multiples usar un vector, y poner una decada valida (1990,2000,2010 y 2020)")
    }
  }
  print(resultados_matriz)
  grafico_resultados <- barplot(t(resultados_matriz), main=paste(as.character(decada),as.character(decada+10),sep="-"), ylim=c(-100,100), xlab="Pa?s", ylab="% variaci?n entre d?cadas", col=c("red", "green", "blue"), beside=TRUE)
  legend("bottomleft",
         c("CO2","Metano", "Oxido nitroso"),
         fill = c("red","green", "blue")
  )
}