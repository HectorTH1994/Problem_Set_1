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



################ Punto No. 5 - Predicting earnings #####################
