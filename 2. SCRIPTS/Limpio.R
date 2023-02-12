#Scraping de los datos
## Datos de Chunks 6-10
### Carlos, Danna, HÃ©ctor, Alexa
#Se prepara el espacio por medio del llamado a los paquetes y librerÃ­as: 
library(pacman)
p_load("tidyverse","rvest","writexl","stargazer","ggplot2","reshape2", "dplyr","datasets","EnvStats", "skimr","gridExtra", "psych", "PerformanceAnalytics","boot")
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

#Se transforma a tipo Tibble para un mejor anÃ¡lisis
Base_datos_final <- as_tibble(data)

#TENGO DUDA SI ESTO VA AQUI, DADO QUE NO SE NECESITA EXPORTAR LA BASE DE DATOS
saveRDS(Base_datos_final, file = "Base_datos_final.rds")

#####################################
###Limpieza de datos#################
#####################################



##De este primer analisis se concluye que existe cero faltantes en edad, por lo cual el primero paso es seleccionar
## que la muestra sea mayor o igual a 18 aÃ±os.

df<-(data %>%
       filter(age >= 18, dsi == 0))




##Revisamos los datos faltantes para cada columna

max(colSums(is.na(df)))
colSums(is.na(df))

#Dado que gran existe muchos datos faltantes, vemos cuantas y cuales columnas tiene mayor numero de faltantes
length(which(colSums(is.na(df)) > 10000))


#Calculamos el porcentaje de los datos diferentes de NA
sum(df$y_ingLab_m_ha > 0 & !is.na(df$y_ingLab_m_ha) )/length(df$y_ingLab_m_ha)

#quitmos los NA
df <- df[!is.na(df$y_ingLab_m_ha), ]


##Realizamos un analisis exploratorio de valores atipicos para la varaible de interes que es y_ingLab_m_ha
##Grafica de dispersion


histograma_salarios <- ggplot(df, aes(x = y_ingLab_m_ha)) +
  geom_histogram(fill = "blue", color = "black") +
  ggtitle("Histograma de salario por hora") +
  labs(x = "", y = "Ingresos por hora")+
  theme(plot.title = element_text(hjust = 0.5))

boxplot_salarios <- ggplot(df, aes(x = "Salarios", y = y_ingLab_m_ha)) +
  geom_boxplot(fill = "red", color = "black") +
  ggtitle("Diagrama de Cajas de salario por hora") +
  labs(x = "", y = "Ingresos por hora")+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(histograma_salarios, boxplot_salarios, ncol = 2)

stargazer(summary(df$y_ingLab_m_ha), type="latex")


#Se observa un alto numero de valores atipicos, por lo cual los eliminaremos
#se procede a eliminar los valores de salarios que superen  la media +/- 1.5 veces la desviaciÃ³n 
#Valores atÃ­picos = Observaciones> Q3 + 1.5 * IQR

limite_punto1 <- quantile(x=df$y_ingLab_m_ha)[4]+1.5*IQR(x=df$y_ingLab_m_ha )


#Contamos los valores atipicos


df_sin_atipicos<-(df %>%
                    filter(y_ingLab_m_ha <= limite_punto1))

histograma_salarios_sin_at <- ggplot(df_sin_atipicos, aes(x = y_ingLab_m_ha)) +
  geom_histogram(fill = "blue", color = "black") +
  ggtitle("Histograma de salario por hora") +
  labs(x = "", y = "Ingresos por hora")+
  theme(plot.title = element_text(hjust = 0.5))

boxplot_salarios_sin_at <- ggplot(df_sin_atipicos, aes(x = "Salarios", y = y_ingLab_m_ha)) +
  geom_boxplot(fill = "red", color = "black") +
  ggtitle("Diagrama de Cajas de salario por hora") +
  labs(x = "", y = "Ingresos por hora")+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(histograma_salarios_sin_at, boxplot_salarios_sin_at, ncol = 2)



######################################################################
###############PUNTO 1#############################################
##################################################################

df_sin_atipicos<-(df %>%
                    filter(y_ingLab_m_ha <= limite_punto1))

df_anes <- na.omit(df_sin_atipicos[c("y_ingLab_m_ha","age")])
df_anes$age_cuadrado <- df_anes$age^2
df_anes$log_salario <- log(df_anes$y_ingLab_m_ha)
df$age_cuadrado <- df$age^2


##Calculamos la matriz de correlaciones

pairs.panels(df_anes,
             smooth = TRUE,      # Si TRUE, dibuja ajuste suavizados de tipo loess
             scale = FALSE,      # Si TRUE, escala la fuente al grado de correlación
             density = TRUE,     # Si TRUE, añade histogramas y curvas de densidad
             ellipses = TRUE,    # Si TRUE, dibuja elipses
             method = "pearson", # Método de correlación (también "spearman" o "kendall")
             pch = 21,           # Símbolo pch
             lm = FALSE,         # Si TRUE, dibuja un ajuste lineal en lugar de un ajuste LOESS
             cor = TRUE,         # Si TRUE, agrega correlaciones
             jiggle = FALSE,     # Si TRUE, se añade ruido a los datos
             factor = 2,         # Nivel de ruido añadido a los datos
             hist.col = 4,       # Color de los histogramas
             stars = TRUE,       # Si TRUE, agrega el nivel de significación con estrellas
             ci = TRUE)          # Si TRUE, añade intervalos de confianza a los ajustes



##Grafica de dispersion

ggplot(df_anes, aes(x = age, y = log_salario))+
  geom_point(color = "red", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  theme_classic() +
  labs(x = "Edad", y = "log(Salario por hora) [COP]",
       title = "Grafico de dispersión edad vs log(salarios)",
       caption = "Datos de ejemplo")

#Observamos si hay atipicos en los años

# histograma edad
histograma_edad <- ggplot(df_anes, aes(x = age)) +
  geom_histogram(fill = "blue", color = "black") +
  ggtitle("Histograma de edades") +
  labs(x = "Edades", y = "Eventos")+
  theme(plot.title = element_text(hjust = 0.5))

diag_cajas_edad <- ggplot(df_anes, aes(x = "", y = age)) +
  geom_boxplot(fill = "red", color = "black") +
  ggtitle("Diagrama de Cajas de edades") +
  labs(x = "", y = "Edades")+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(histograma_edad, diag_cajas_edad, ncol = 2)

#stargazer(summary(df_anes), type="text")
#####################################################
##########MODELO PUNTO 1 ############################


fit_sin_atipicos_p3<- lm(log(y_ingLab_m_ha) ~ age + age_cuadrado, data = df_anes, x = TRUE)
fit_con_atipicos_log<- lm(log(y_ingLab_m_ha) ~ age + age_cuadrado, data = df, x = TRUE)
stargazer(fit_sin_atipicos_p3,fit_con_atipicos_log, type="text")

##Agregamos una columna con los predictores
df_anes$salario_hat = predict(fit_sin_atipicos_p3)
df$salario_hat_age = predict(fit_con_atipicos_log)


# plot predicted values
summ = df_anes %>%  
  group_by(
    age, age_cuadrado
  ) %>%  
  summarize(
    mean_y = mean(log(y_ingLab_m_ha)),
    yhat_reg = mean(salario_hat), .groups="drop"
  ) 

ggplot(summ) + 
  geom_point(
    aes(x = age, y = mean_y),
    color = "blue", size = 2
  ) + 
  geom_line(
    aes(x = age, y = yhat_reg), 
    color = "green", size = 1.5
  ) + 
  labs(
    title = "Salarios usando como predictor la edad y sin atipicos",
    x = "Edad",
    y = "Salario por hora"
  ) +
  theme_bw()


summ = df %>%  
  group_by(
    age, age_cuadrado
  ) %>%  
  summarize(
    mean_y = mean(log(y_ingLab_m_ha)),
    yhat_reg = mean(salario_hat_age), .groups="drop"
  ) 

ggplot(summ) + 
  geom_point(
    aes(x = age, y = mean_y),
    color = "blue", size = 2
  ) + 
  geom_line(
    aes(x = age, y = yhat_reg), 
    color = "green", size = 1.5
  ) + 
  labs(
    title = "Salarios usando como predictor la edad y con atipicos",
    x = "Edad",
    y = "Salario por hora"
  ) +
  theme_bw()+ 
  scale_y_continuous(limits = c(7, 10))

###############################################################
#################BOOTSTRAP#####################################
###############################################################
# Define la función que se usará para el bootstrap
boot_prediccion <- function(df_anes, indices) {
  modelo_boot <- lm(y_ingLab_m_ha ~ ., data = df_anes[indices,])
  prediccion <- predict(modelo_boot, newdata = tibble(x = 86))[1]
  return(prediccion)
}

# Realiza el bootstrap
resultados_boot <- boot(df_anes, boot_prediccion, R = 100000)

# Calcula los percentiles para construir el intervalo de confianza
alpha <- 0.05
limite_inferior <- quantile(resultados_boot$t, probs = alpha / 2)
limite_superior <- quantile(resultados_boot$t, probs = 1 - alpha / 2)

# Muestra el intervalo de confianza
cat("El intervalo de confianza al", 100 * (1 - alpha), "% para X = 1.5 es: [", limite_inferior, ",", limite_superior, "]")

#########################################################################
DSADSA

