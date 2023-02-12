#Taller No. 1 - Problem Set-1
###Secript Final
### Alexandra Rizo, Danna Camila Bolaños, Héctor Taicuán, Carlos Vergara
-------------------------------------------------------------------------------------------------------
#Preparación del espacio: 
  
library(pacman)
p_load("tidyverse","rvest","writexl","stargazer","ggplot2","reshape2", "dplyr","datasets","EnvStats", "skimr","gridExtra", "psych", "PerformanceAnalytics","boot")
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

#excluimos los datos que nos interesan (mayores de 18 -  empleados)
data<-(data %>%
         filter(age >= 18, dsi == 0))

#Analisamos la estructura de la base 
glimpse(data)

# Analisis na

#Analisis de na por variable
colSums(is.na(data))

#Calculamos el porcentaje de los datos diferentes de NA
sum(data$y_ingLab_m_ha > 0 & !is.na(data$y_ingLab_m_ha) )/length(data$y_ingLab_m_ha)

# se infiere que solo el 43% de la base presenta datos de ineteres

#eliminamos todas las filas con un valor faltante en la columna de nuestra valiable dependiente (y_ingLab_m_ha)
df <- data[!is.na(data$y_ingLab_m_ha), ] %>% as.data.frame()


# Ahora vamos a revisar la distribución de nuestra variable a predecir #EBE9FF
histograma_salarios <- ggplot(df, aes(x = y_ingLab_m_ha)) +
  geom_histogram(fill = "#EBE9FF", color = "#A2AEED") +
  ggtitle("Histograma de salario por hora") +
  labs(x = "", y = "Ingresos por hora")+
  theme(plot.title = element_text(hjust = 0.5))
  
##Realizamos un analisis exploratorio de valores atipicos para la varaible de interes que es y_ingLab_m_ha

boxplot_salarios <- ggplot(df, aes(x = "Salarios", y = y_ingLab_m_ha)) +
  geom_boxplot(fill = "#DBDDFF", color = "#645A9F") +
  ggtitle("Diagrama de Cajas de salario por hora") +
  labs(x = "", y = "Ingresos por hora")+
  theme(plot.title = element_text(hjust = 0.5))
grid.arrange(histograma_salarios, boxplot_salarios, ncol = 2)

#Contamos los valores atipicos
limite_punto1 <- quantile(x=df$y_ingLab_m_ha)[4]+1.5*IQR(x=df$y_ingLab_m_ha )

df_atip= df %>% 
  mutate(y_ingLab_m_ha_out = ifelse(test = y_ingLab_m_ha > limite_punto1, 
                                    yes = 1, 
                                    no = 0))
da1<-table(df_atip$y_ingLab_m_ha_out)

#normalisamos los datos aplicando log
lgwage <-log(df$y_salary_m_hu)
df<-cbind(df,lgwage)

histogramalogsalarios <- ggplot(df, aes(x = y_ingLab_m_ha)) +
  geom_histogram(fill = "#EEF8F0", color = "#388E3C") +
  ggtitle("Histograma de salario por hora") +
  labs(x = "", y = "Ingresos por hora")+
  theme(plot.title = element_text(hjust = 0.5))

boxplotlogsalarios <- ggplot(df, aes(x = "Salarios", y = lgwage)) +
  geom_boxplot(fill = "#E3F2FD", color = "#30BFDD") +
  labs(x = "", y = "Ingresos por hora")
  ggtitle("Diagrama de Cajas de log salario por hora") +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(histogramalogsalarios, boxplotlogsalarios , ncol = 2)

GrafDispercion <- plot(lgwage,pch=19,col="#FFF6F5")

limite_log <- quantile(x=df$lgwage)[4]+1.5*IQR(x=df$lgwage)

#Contamos los valores atipicos con la variable log:

df_p = df %>% 
  mutate(lgwage_atipicos= ifelse(test = lgwage > limite_log, 
                                    yes = 1, 
                                    no = 0))
da2<-table(df_p$lgwage_atipicos)

#comparación datos atipicos
table(da1,da2)

################ Punto No. 3 - Age-wage Profile #####################
df_sin_atipicos<-(df %>%
                    filter(y_ingLab_m_ha <= limite_punto1))

df_anes <- na.omit(df_sin_atipicos[c("y_ingLab_m_ha","age")])
df_anes$age_cuadrado <- df_anes$age^2
df_anes$log_salario <- log(df_anes$y_ingLab_m_ha)
df$age_cuadrado <- df$age^2


##Calculamos la matriz de correlaciones

pairs.panels(df_anes,
             smooth = TRUE,      # Si TRUE, dibuja ajuste suavizados de tipo loess
             scale = FALSE,      # Si TRUE, escala la fuente al grado de correlaci?n
             density = TRUE,     # Si TRUE, a?ade histogramas y curvas de densidad
             ellipses = TRUE,    # Si TRUE, dibuja elipses
             method = "pearson", # M?todo de correlaci?n (tambi?n "spearman" o "kendall")
             pch = 21,           # S?mbolo pch
             lm = FALSE,         # Si TRUE, dibuja un ajuste lineal en lugar de un ajuste LOESS
             cor = TRUE,         # Si TRUE, agrega correlaciones
             jiggle = FALSE,     # Si TRUE, se a?ade ruido a los datos
             factor = 2,         # Nivel de ruido a?adido a los datos
             hist.col = 4,       # Color de los histogramas
             stars = TRUE,       # Si TRUE, agrega el nivel de significaci?n con estrellas
             ci = TRUE)          # Si TRUE, a?ade intervalos de confianza a los ajustes



##Grafica de dispersion

ggplot(df_anes, aes(x = age, y = log_salario))+
  geom_point(color = "red", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  theme_classic() +
  labs(x = "Edad", y = "log(Salario por hora) [COP]",
       title = "Grafico de dispersi?n edad vs log(salarios)",
       caption = "Datos de ejemplo")

#Observamos si hay atipicos en los a?os

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
# Define la funci?n que se usar? para el bootstrap
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


################ Punto No. 4 - Gender earnings gap #####################

## Se crean las variables logaritimas de ingreso "y_ingLab_m_ha" y sexo para las bases de dato utilizadas: 

df <-df%>% mutate(age2 = age*age)
df <- df %>% mutate(logingtot=log(y_ingLab_m_ha))
df <- df %>% mutate(female = ifelse(sex == 0, 1, 0)) # se crea variable 1= mujer 0= hombre

## Se genera un gráfico con las nuevas variables con fundamento en la información de ambas bases: 

g_df<- ggplot(data=df) + 
  geom_histogram(mapping = aes(x=logingtot , group=as.factor(female) , fill=as.factor(female)))
histo_final_df <-g_df + scale_fill_manual(values = c("0"="orange" , "1"="red") , label = c("0"="Hombre" , "1"="Mujer") , name = "Sexo")
histo_final_df

# 2 - Se realiza la regresión inicial en relación con las brechas por el salario y el género: 

reg_df <- lm(logingtot~female, df)

stargazer(reg_df, type="text", digits=7)

# 3- Salaior igualitario para trabajos iguales: 
## Se realizar el control utilizando el proceso Frish-Waugh-Lovell, (en adelante "FLW")

df_anes <- na.omit(df[c("y_ingLab_m_ha","age", "sex", "maxEducLevel")])
df_anes$age_cuadrado <- df_anes$age^2
View(df_anes)

modelonocond = lm(log(y_ingLab_m_ha) ~ sex, 
                  data = df_anes)
summary(modelonocond)
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
  
  #Guardando coeficiente tes
  muestra_intercepto <-
    c(muestra_intercepto, modelo_bootstrap$coefficients[1])
  
  muestra_otros_regresores <-
    c(muestra_otros_regresores, modelo_bootstrap$coefficients[-1])
  
}

##Grafica modelo no condicionado
#gourpby van los predictores 
#mean_y = variable y
## añadir Agregamos una columna con los predictores para el caso yhat_reg

##Agregamos una columna con los predictores
df_anes$salario_sex = predict(df)

summ = df %>%  
  group_by(
   sex 
  ) %>%  
  summarize(
    mean_y = mean(log(y_ingLab_m_ha)),
    yhat_reg = mean(salario_hat_sexx), .groups="drop"
  ) 

ggplot(summ) + 
  geom_point(
    aes(x = sex, y = mean_y),
    color = "blue", size = 2
  ) + 
  geom_line(
    aes(x = sex, y = yhat_reg), 
    color = "green", size = 1.5
  ) + 
  labs(
    title = "Salarios usando como predictor el genero",
    x = "female",
    y = "Salario por hora"
  ) +
  theme_bw()+ 
  scale_y_continuous(limits = c(7, 10))

################ Punto No. 5 - Predicting earnings #####################


#################PUNTO 5###################
####CREAMOS DATAFRAME

df_5 <- df[c("y_ingLab_m_ha", "age", "p6210s1", "totalHoursWorked", "sizeFirm", "oficio","estrato1","sex","maxEducLevel")]
df_5 <- df_5[!is.na(df$maxEducLevel), ]
df_5 = rename(df_5, c(salario="y_ingLab_m_ha", edad="age", gr_escolaridad_ap = "p6210s1", horas_trabajadas_s="totalHoursWorked", tamano_firma="sizeFirm",sexo="sex",max_nivel_edu="maxEducLevel"))
df_5$edad_cuadrado <- df_5$edad^2

######variables categoricas##########

# Definimos las variables categoricas
variables_categoricas <- c("estrato1", "sexo","max_nivel_edu","sizeFirm")

for (v in variables_categoricas) {
  df_5[, v] <- as.factor(df_5[, v, drop = T])
}

str(df_5)

###Verificamos que no hayan na####

sum(is.na(df_5))

##Dividimos los datos

set.seed(1121)
sample <- sample(c(TRUE, FALSE), nrow(df_5), replace=TRUE, prob=c(0.7,0.3))
sum(sample)/nrow(df_5)

train  <- df_5[sample, ] #train sample those that are TRUE in the sample index
test   <- df_5[!sample, ] #test sample those that are FALSE in the sample index


# Convertimos salario y en log
y_train <- log(train[,"salario"])
X_train <- select(train, -salario)
y_test <- log(test[,"salario"])
X_test <- select(test, -salario)

#########Se realiza los histogramas

histograma_train <- ggplot(train, aes(x = salario)) +
  geom_histogram(fill = "#E6F1FF", color = "#4C5FA9") +
  ggtitle("TRAIN") +
  labs(x = "", y = "Ingresos por hora")+
  theme(plot.title = element_text(hjust = 0.5))

histograma_test <- ggplot(test, aes(x = salario)) +
  geom_histogram(fill = "#E0F2E6", color = "#45958C") +
  ggtitle("TEST") +
  labs(x = "", y = "Ingresos por hora")+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(histograma_train, histograma_test, ncol = 2)
#########CAJAS DE TEST Y TRAIN#################

caja_train <- ggplot(train, aes(x = log(salario))) +
  geom_boxplot(fill = "#E6F1FF", color = "#4C5FA9") +
  ggtitle("TRAIN") +
  labs(x = "", y = "Ingresos por hora")+
  theme(plot.title = element_text(hjust = 0.5))

caja_test <- ggplot(test, aes(x = log(salario))) +
  geom_boxplot(fill = "#E0F2E6", color = "#45958C") +
  ggtitle("TEST") +
  labs(x = "", y = "Ingresos por hora")+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(caja_train, caja_test, ncol = 2)

########## Estudiemos la relación entre el puntaje zscore con la desnutrición########
ggplot(train, aes(x = edad, y = salario, 
               color = estrato1)) +
  geom_point() +
  theme_bw() +
  labs(x = "Edad", y = "Salario [COP]") +
  scale_color_discrete(name = "Estrato socieconomico") +
  theme(legend.position="bottom")

###NO TIENE SENTIDO QUE EL VALOR MAXIMO EN SALARIOS SEA UNA PERSONA ESTRATO 4


# Si no se especifica valor de lambda, se selecciona un rango automático.


mod_completo <- lm(log(salario) ~ ., train)
mod_edad <- lm (log(salario)~edad+edad_cuadrado, train)
mod_sexo <- lm (log(salario)~sexo, train)
mod_sexo_completo<- lm (log(salario)~sexo+max_nivel_edu+edad+edad_cuadrado, train)

