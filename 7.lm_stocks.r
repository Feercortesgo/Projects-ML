library(xlsx)
library(tidyverse)
library(readr)
library(DescTools) 
library(stats)     
library(ggplot2)
library(quantmod) 
library(tfplot)   
library(rollRegres)

#lee el archivo formato excel
D <- read.xlsx('...',1)

#guarda el resultado de filtrar la columna 'Date' en df
resultados                     <- data.frame('Date' = D$Date)

#asinga nulos en todas las filas de las columnas con posición 2 a 14 del df "D" 
resultados[,colnames(D)[2:14]] <- NA

#asinga el valor 52 a ventana
ventana <- 52


#para i en las columnas de 2 a la 14 (omite fecha)
for (i in c(colnames(resultados)[2:14])){
  #asigna una formula para cada item en  columnas
  formula <- paste0(i, ' ~ UST + VIX')
  #print(i)
  #a partir de la fila 52 
  for (j in ventana:nrow(resultados)){
    
    inicio            <- j - (ventana - 1)
    #print(inicio)
    #print(j)
    #solo toma las primeras 52 lineas
    D_short           <- D[inicio:j,]
    #le da tratamiento a nulos asignandolos a cero, aunque se podría hacer otra imputación (la media probablemente)
    D_short[is.na(D_short)] <- 0
    #realiza la función que aproxima los valores de i mediante ust y vix, extrae coeficientes para ust.
    resultados[j,i]   <- lm(formula, data = D_short,)$coefficients['UST']
    
  }
}


write.xlsx(resultados, 
           file = "XXX", 
           sheetName="Resultados", 
           append=FALSE)
