#Scraping de los datos
## Datos de Chunks 6-10
### Carlos, Danna, Héctor, Alexa
#Se prepara el espacio por medio del llamado a los paquetes y librerías: 
library(pacman)
p_load(tidyverse,rvest,writexl)
library(data.table)
library(datasets)
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
view(data)

data1<-(data)[-1]

Base_datos_final <- as_tibble(data1)
saveRDS(data2, file = "data2,rds")