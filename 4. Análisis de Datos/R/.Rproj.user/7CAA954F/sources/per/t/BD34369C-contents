#Instalamos tidyverse para realizar el análisis de datos

install.packages("tidyverse")

#Cargamos los correspondientes paquetes que nos ayudarán a realizar el análisis

library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)

#Cargamos el archivo csv para trabajar. Utilizamos el conjunto de datos de tipo de acceso desglosado

metro_desglosado <- read.csv('Metro_desglosado_transpuesto.csv')

#Echamos un vistazo a los datos contenidos en el archivo csv

head(metro_desglosado)

str(metro_desglosado)

colnames(metro_desglosado)

#Ya que queremos conocer la afluencia total de pasajeros por cada estación, sumaremos los ingresos por boleto, prepago y gratuidad

metro_desglosado <- mutate(metro_desglosado, afluencia = boleto + prepago + gratuidad)

head(metro_desglosado)

#TIPO DE ACCESO DESGLOSADO POR LINEA 

desglose_linea_total <- aggregate(cbind(boleto, prepago, gratuidad, afluencia) ~ linea, data = metro_desglosado, sum)
View(desglose_linea_total)

#Si queremos que estos se agrupen por año, añadimos lo siguiente:

desglose_linea_anio <- aggregate(cbind(boleto, prepago, gratuidad, afluencia) ~ linea + anio, data = metro_desglosado, sum)
View(desglose_linea_anio)

#Ahora graficamos la afluencia de pasajeros por año

desglose_linea_total_long <- tidyr::gather(desglose_linea_total, key = "categoria", value = "valor", -linea)

# Crear el gráfico de barras agrupado
ggplot(desglose_linea_total_long, aes(x = linea, y = valor, fill = categoria)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Tipo de acceso desglosado por línea (2021-2023)",
       x = "Línea",
       y = "Total de pasajeros") +
  scale_fill_manual(values = c("boleto" = "#0D43EC", "prepago" = "#FD7409", "gratuidad" = "#037D33", "afluencia" = "#45BAFC")) +
  theme_minimal()
