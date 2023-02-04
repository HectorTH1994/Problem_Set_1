#Se llaman los paquetes necesarios para el Scraping
library(tidyverse)
library(pacman)
library(rvest)
library(data.table)
library(datasets)
#Se procede a llamar el chunk No. 6 de la base de datos GEIH: 
Chunk6 <-read_html('https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_6.html')
