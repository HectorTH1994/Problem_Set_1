#Taller No. 1 - Problem Set No. 1
#PUNTO No. 4 - Brecha salarial y género
## Alexandran Rizo, Héctor Tacuán, Danna Bolaños, Carlos Vergara
-------------------------------------------------------------------
# 1- Inicio del proceso:
##Se llaman las librerias y paquetes a ser utilizados: 
  
library(pacman)

p_load("tidyverse","rvest","writexl","stargazer","ggplot2","reshape2", "dplyr","datasets","EnvStats", "skimr","gridExtra", "psych", "PerformanceAnalytics")

library(datasets)

library(data.table)

#Se llaman las bases de datos generadas en el proceso de scraping y limpieza de datos e igualmente se inspeccionan:

df_sin_atipicos <- import("df_sin_atipicos.rds")

df <- import("df.rds")

## Se crean las variables logaritimas de ingreso "y_ingLab_m_ha" y sexo para las bases de dato utilizadas: 

df <-df%>% mutate(age2 = age*age)
df <- df %>% mutate(logingtot=log(y_ingLab_m_ha))
df_sin_atipicos <-df_sin_atipicos %>% mutate(age2 = age*age)
df_sin_atipicos <- df_sin_atipicos %>% mutate(logingtot=log(y_ingLab_m_ha))
df <- df %>% mutate(female = ifelse(sex == 0, 1, 0)) # se crea variable 1= mujer 0= hombre
df_sin_atipicos <- df_sin_atipicos %>% mutate(female = ifelse(sex == 0, 1, 0))

## Se genera un gráfico con las nuevas variables con fundamento en la información de ambas bases: 

g_df<- ggplot(data=df) + 
  geom_histogram(mapping = aes(x=logingtot , group=as.factor(female) , fill=as.factor(female)))

histo_final_df <-g_df + scale_fill_manual(values = c("0"="orange" , "1"="red") , label = c("0"="Hombre" , "1"="Mujer") , name = "Sexo")

g_df_sin_atipicos <- ggplot(data=df_sin_atipicos) + 
  geom_histogram(mapping = aes(x=logingtot , group=as.factor(female) , fill=as.factor(female)))

histo_final_df_sin_atipicos <- g_df_sin_atipicos + scale_fill_manual(values = c("0"="orange" , "1"="red") , label = c("0"="Hombre" , "1"="Mujer") , name = "Sexo")

grid.arrange(histo_final_df, histo_final_df_sin_atipicos, ncol = 2)

#El primer aspecto a realiozar es la estimación de la brecha salarial bajo la formula *
### Log(w)=B1+B2Female+u, donde w hace referencia a wage o salario. 
