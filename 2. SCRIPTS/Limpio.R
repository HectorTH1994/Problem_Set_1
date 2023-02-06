#Scraping de los datos
## Datos de Chunks 6-10
### Carlos, Danna, Héctor, Alexa
#Se prepara el espacio por medio del llamado a los paquetes y librerías: 
library(pacman)
p_load(tidyverse,rvest,writexl)
library(data.table)
library(datasets)
library(dplyr)
#Se realiza el scraping de los Chunks 6 a 10: 
url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_"
data <- data.frame()
for (i in 1:10) {
  url_i <- paste0(url, i, ".html")
  tablas <- url_i %>%
    read_html() %>%
    html_table() %>% .[[1]]
  data <- rbind.data.frame(data, tablas)
}

#Eliminamos la primera columna, la cual es de indices y no se requiere
data<-(data)[-1]

#Se transforma a tipo Tibble para un mejor análisis
Base_datos_final <- as_tibble(data)

#TENGO DUDA SI ESTO VA AQUI, DADO QUE NO SE NECESITA EXPORTAR LA BASE DE DATOS
saveRDS(Base_datos_final, file = "Base_datos_final.rds")

#####################################
###Limpieza de datos#################
#####################################



##De este primer analisis se concluye que existe cero faltantes en edad, por lo cual el primero paso es seleccionar
## que la muestra sea mayor o igual a 18 años.

df<-(data %>%
          filter(age >= 18))

##Con los datos que vamos a trabajar revisamos los datos faltantes para cada columna

max(colSums(is.na(df)))

#Dado que gran existe muchos datos faltantes, vemos cuantas y cuales columnas tiene mayor numero de faltantes
length(which(colSums(is.na(df)) > 10000))

###Seleccionamos las columnas que son de nuestro interes

df[c("sex","age")]

