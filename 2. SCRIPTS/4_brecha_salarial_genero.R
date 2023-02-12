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

# 2 - Se realiza la regresión inicial en relación con las brechas por el salario y el género: 

reg_df <- lm(logingtot~female, df)

red_df_sin_a <- lm(logingtot~female, df_sin_atipicos)

stargazer(reg_df, type="text", digits=7)

stargazer(red_df_sin_a, type="text", digits=7)

# 3- Salaior igualitario para trabajos iguales: 
## Se realizar el control utilizando el proceso Frish-Waugh-Lovell, (en adelante "FLW")

df_anes <- na.omit(df_sin_atipicos[c("y_ingLab_m_ha","age", "sex", "maxEducLevel")])
df_anes$age_cuadrado <- df_anes$age^2
View(df_anes)

modelonocond = lm(log(y_ingLab_m_ha) ~ sex, 
                  data = df_anes)

####Modelo condicionado

###Obteniendo residuos

modelcondic = lm(log(y_ingLab_m_ha) ~ sex+age+age_cuadrado+maxEducLevel, 
                 data = df_anes)
summary(modelcondic)

resid1 = residuals(lm(log(y_ingLab_m_ha) ~ sex+age+age_cuadrado, 
                      data = df_anes))

####Regresion de residuos con máximo nivel de escolaridad alcanzado

resid2= residuals(lm(maxEducLevel ~ sex+age+age_cuadrado, 
                     data = df_anes))

#####Regresión de los residuos 

coefficients(lm(resid1 ~ resid2))['resid2']

#####Con boostrap

set.seed(123)

###Contenedores para los coeficientes

muestra_intercepto <- NULL
muestra_otros_regresores <- NULL

for (i in 1:1000) {
  muestra_d = df_anes[sample(1:nrow(df_anes), nrow(df_anes), replace = TRUE), ]
  
  #Estimación
  modelo_bootstrap <- lm(log(y_ingLab_m_ha) ~ sex+age+age_cuadrado+maxEducLevel, data = muestra_d)
  
  #Guardando coeficientes
  muestra_intercepto <-
    c(muestra_intercepto, modelo_bootstrap$coefficients[1])
  
  muestra_otros_regresores <-
    c(muestra_otros_regresores, modelo_bootstrap$coefficients[-1])
  
}

# 4- 
 
