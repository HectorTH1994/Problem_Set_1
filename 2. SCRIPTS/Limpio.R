#Scraping de los datos
## Datos de Chunks 6-10
### Carlos, Danna, Héctor, Alexa
#Se prepara el espacio por medio del llamado a los paquetes y librerías: 
library(pacman)

p_load("tidyverse","rvest","writexl","stargazer","ggplot2","reshape2", "dplyr","datasets")
library(data.table)


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

##Revisamos los datos faltantes para cada columna

max(colSums(is.na(df)))
colSums(is.na(df))

#Dado que gran existe muchos datos faltantes, vemos cuantas y cuales columnas tiene mayor numero de faltantes
length(which(colSums(is.na(df)) > 10000))

###Seleccionamos las columnas que son de nuestro interes
##Importante Si no recibió salario en dinero, escriba 00; 
#si recibió pero no sabe el monto, escriba 98; si no sabe si recibió, escriba 99
#Horas extras p6510s1


sum(is.na(df$y_salary_m_hu))
nrow(df[!is.na(df$y_salary_m_hu),])


sum(df$y_salary_m_hu > 0 & !is.na(df$y_salary_m_hu) )

#Calculamos el porcentaje de los datos diferentes de NA
sum(df$y_salary_m_hu > 0 & !is.na(df$y_salary_m_hu) )/length(df$y_salary_m_hu)



df_anes <- na.omit(df[c("y_salary_m_hu","age")])
df_anes$age_cuadrado <- df_anes$age^2


str(df_anes)

##Calculamos la matriz de correlaciones

corr_matrix <- cor(df_anes)


##Graficamos la matriz de correlaciones
ggplot(melt(corr_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))

##Histograma de las edades y de los salarios
# dividmos el gráfico en dos paneles

par(mfrow=c(1, 2))

# histograma ingresos
hist(df_anes$y_salary_m_hu)

# histograma ingresos anes
hist(df_anes$age)

##Como se observa en el histograma de la izquiera existe un salario que nos altera la muestra
##Grafica de dispersion

ggplot(df_anes, aes(x = age, y = y_salary_m_hu)) +
  geom_point(color = "red", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  theme_classic() +
  labs(x = "Edad", y = "Salario por hora",
       title = "Gráfico de dispersión edad vs salarios",
       caption = "Datos de ejemplo")

#Claramente se observa valores atipicos por lo cual se procede a eliminar los valores aticos


par(mfrow = c(1, 2))
boxplot(df_anes$y_salary_m_hu)
boxplot(df_anes$age)


#se procede a eliminar los valores de salarios que superen  la media +/- 1.5 veces la desviación 