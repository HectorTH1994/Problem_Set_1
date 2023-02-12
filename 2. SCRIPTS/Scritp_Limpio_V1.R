#Taller No. 1 - Problem Set-1
###Secript Final
### Alexandra Rizo, Danna Camila Bolaños, Héctor Taicuán, Carlos Vergara
-------------------------------------------------------------------------------------------------------
#Preparación del espacio: 
  
library(pacman)
p_load("tidyverse","rvest","writexl","stargazer","ggplot2","reshape2", "dplyr","datasets", "skimr","gridExtra")
library(data.table)

############## Scraping y Limpieza de datos ###################

# Se realiza el scraping de los Chunks 1 a 10: 

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

#Se salva la base de datos final: 
saveRDS(Base_datos_final, file = "Base_datos_final.rds")
view(data)

# excluimos los datos que nos interesan (mayores de 18 -  empleados)
data<-(data %>%
         filter(age >= 18, dsi == 0))

#Analisamos la estructura de la base 
glimpse(data)

# Analisis na

#Analisis de na por variable
colSums(is.na(data))

#Calculamos el porcentaje de los datos diferentes de NA
sum(data$y_ingLab_m_ha > 0 & !is.na(data$y_ingLab_m_ha) )/length(data$y_ingLab_m_ha)

# se infiere que el 43% de la base presenta datos 

#eliminamos todas las filas con un valor faltante en la columna de nuestra valiable dependiente (y_ingLab_m_ha)
df <- data[!is.na(data$y_ingLab_m_ha), ] %>% as.data.frame()

limite_punto1 <- quantile(x=df$y_ingLab_m_ha)[4]+1.5*IQR(x=df$y_ingLab_m_ha )

#Contamos los valores atipicos

df = df %>% 
  mutate(y_ingLab_m_ha_out = ifelse(test = y_ingLab_m_ha > limite_punto1, 
                                    yes = 1, 
                                    no = 0))
table(df_p$y_ingLab_m_ha_out)

# Ahora vamos a revisar la distribución de nuestra variable a predecir
HistogramWage <- ggplot(df, aes(x = y_ingLab_m_ha)) +
  geom_histogram( fill = "#BFEFFF") +
  labs(x = "Salario por horas", y = "Pesos colombianos") +
  theme_bw()
grid.arrange(HistogramWage)

##Realizamos un analisis exploratorio de valores atipicos para la varaible de interes que es y_ingLab_m_ha

BoxplotWage <- ggplot(df, aes(x = "Salarios", y = y_ingLab_m_ha)) +
  geom_boxplot(fill = "#CFDCEF", color = "#F5D8C0") +
  ggtitle("Diagrama de Cajas de salario por hora") +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(BoxplotWage)

#normalisamos los datos aplicando log
lgwage <-log(df$y_salary_m_hu)
df<-cbind(df,lgwage)

BoxplotWage <- ggplot(df, aes(x = "Salarios", y = y_ingLab_m_ha)) +
  geom_boxplot(fill = "#CFDCEF", color = "#F5D8C0") +
  ggtitle("Diagrama de Cajas de salario por hora") +
  theme(plot.title = element_text(hjust = 0.5))

GrafDispercion <- plot(lgwage,pch=19,col="#FFF6F5")

limite_log <- quantile(x=df$lgwage)[4]+1.5*IQR(x=df$lgwage)

#Contamos los valores atipicos con la variable log:

df_p = df %>% 
  mutate(lgwage_atipicos= ifelse(test = lgwage > limite_log, 
                                    yes = 1, 
                                    no = 0))
table(df_p$lgwage_atipicos)

################ Punto No. 3 - Age-wage Profile #####################



################ Punto No. 4 - Gender earnings gap #####################

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


################ Punto No. 5 - Predicting earnings #####################
