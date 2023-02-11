#Taller No. 1 - Problem Set No. 1
#PUNTO No. 4 - Brecha salarial y género
## Alexandran Rizo, Héctor Tacuán, Danna Bolaños, Carlos Vergara
-------------------------------------------------------------------
# 1- Inicio del proceso:
#Se llaman las librerias y paquetes a ser utilizados: 
  
library(pacman)

p_load("tidyverse","rvest","writexl","stargazer","ggplot2","reshape2", "dplyr","datasets","EnvStats", "skimr","gridExtra", "psych", "PerformanceAnalytics")

library(datasets)

library(data.table)

#Se llaman las bases de datos generadas en el proceso de scraping y limpieza de datos e igualmente se inspeccionan:

df_sin_atipicos <- import("df_sin_atipicos.rds")

df <- import("df.rds")

# 2- Se crean las variables logaritimas de ingreso "y_ingLab_m_ha" y sexo para las bases de dato utilizadas: 

df <-df%>% mutate(age2 = age*age)

df <- df %>% mutate(logingtot=log(y_ingLab_m_ha))

df_sin_atipicos <-df_sin_atipicos %>% mutate(age2 = age*age)

df_sin_atipicos <- df_sin_atipicos %>% mutate(logingtot=log(y_ingLab_m_ha))

#El primer aspecto a realiozar es la estimación de la brecha salarial bajo la formula *
### Log(w)=B1+B2Female+u, donde w hace referencia a wage o salario. 
