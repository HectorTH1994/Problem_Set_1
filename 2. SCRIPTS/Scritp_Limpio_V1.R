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

#########BOOTSTRAP#################
#####funciona por fin##############
###################################

boot_prediccion <- function(df, indices) {
  modelo_boot <- lm(log(y_ingLab_m_ha) ~ age + age_cuadrado, data = df[indices, ])
  return(predict(modelo_boot, newdata = df))
}

set.seed(112)
resultados_boot <- boot(df, boot_prediccion, R = 100)

# Calcula los intervalos de confianza
lower_bound <- apply(exp(resultados_boot$t), 2, quantile, probs = 0.05)
upper_bound <- apply(exp(resultados_boot$t), 2, quantile, probs = 0.95)


# Genera un data frame con los valores predictores y sus predicciones, junto con los límites inferiores y superiores para los intervalos de confianza
predicciones_anes <- data.frame(age= df$age,
                           age_cuadrado=df$age_cuadrado,
                           y_pred = exp(resultados_boot$t[1, ]),
                           lower = lower_bound,
                           upper = upper_bound)

# Grafica las predicciones y los intervalos de confianza
grafica_2 <- ggplot(predicciones_anes, aes(age, y_pred)) +
  geom_point() +
  geom_line(aes(age, y_pred)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)

  

summ = df %>%  
  group_by(
    age, age_cuadrado
  ) %>%  
  summarize(
    mean_y = mean(y_ingLab_m_ha), .groups="drop"
  ) 


ggplot() +
  geom_line(aes(x = age, y = y_pred), data = predicciones_anes)+
  geom_ribbon(aes(x=age, ymin = lower, ymax = upper), alpha = 0.2, data = predicciones_anes)+
  geom_point(aes(x = age, y = mean_y), data = summ, color = "blue", size = 2) + 
  labs(
    title = "Salarios usando como predictor la edad Y BOOTSTRAP",
    x = "Edad",
    y = "Salario por hora"
  )+
  scale_y_continuous(limits = c(0, 15000))


########Buscamos el valor maximo########
X <- seq(from = 1, to = 150, by = 1)
predicciones <- predict(fit_con_atipicos_log, newdata=data.frame(age=X, age_cuadrado=X^2))
valor_maximo <- exp(max(predicciones))
valor_maximo
predicciones_maxima <- predict(fit_con_atipicos_log, newdata=data.frame(age=X, age_cuadrado=X^2))
predicciones_maxima

prediction_max <- exp(predict(fit_con_atipicos_log, newdata = data.frame(age=45, age_cuadrado=45^2), interval = "confidence"))
stargazer(prediction_max, type="text")

##Agregamos una columna con los predictores
df_anes$salario_hat = predict(fit_sin_atipicos_p3)
df$salario_hat_age = predict(fit_con_atipicos_log)



################gRAFICA DE PERFILES SIN BOOSTRAP########


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

####################GRAFICA 2#######################


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



################ Punto No. 4 - Gender earnings gap #####################

## Se crean las variables logaritimas de ingreso "y_ingLab_m_ha" y sexo para las bases de dato utilizadas: 

df <-df%>% mutate(age2 = age*age)
df <- df %>% mutate(logingtot=log(y_ingLab_m_ha))

## Se genera un gráfico con las nuevas variables con fundamento en la información de ambas bases: 

g_df<- ggplot(data=df) + 
  geom_histogram(mapping = aes(x=logingtot , group=as.factor(sex) , fill=as.factor(sex)))
histo_final_df <-g_df + scale_fill_manual(values = c("0"="orange" , "1"="red") , label = c("1"="Hombre" , "0"="Mujer") , name = "Sexo")
histo_final_df

# 2 - Se realiza la regresión inicial en relación con las brechas por el salario y el género: 

modelonocond = lm(log(y_ingLab_m_ha) ~ sex, 
                  data = df)
summary(modelonocond)

stargazer(modelonocond, type="text", digits=7)

# 3- Salaio igualitario para trabajos iguales: 
## Se realiza el control utilizando el proceso Frish-Waugh-Lovell, (en adelante "FLW")

df_anes <- na.omit(df[c("y_ingLab_m_ha","age", "sex", "maxEducLevel", "oficio", "estrato1")])
df_anes$age_cuadrado <- df_anes$age^2
View(df_anes)


####Modelo condicionado

###Obteniendo residuos

modelcondic = lm(log(y_ingLab_m_ha) ~ ., 
                 data = df_anes)
summary(modelcondic)

resid1 = residuals(lm(log(y_ingLab_m_ha) ~ ., 
                      data = df_anes))

####Regresion de residuos 

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
  modelo_bootstrap <- lm(log(y_ingLab_m_ha) ~ ., data = muestra_d)
  
  #Guardando coeficiente test
  muestra_intercepto <-
    c(muestra_intercepto, modelo_bootstrap$coefficients[1])
  
  muestra_otros_regresores <-
    c(muestra_otros_regresores, modelo_bootstrap$coefficients[-1])
  
}

#calculamos la edad pico 
p_load(numDeriv)

# Supongamos que tenemos un data frame llamado "datos" con dos columnas: "edad" y "salario"

# analizamos el comportamiento de las variables 

modelo_salario_edad <- lm(y_ingLab_m_ha ~ age + I(age^2), data = df)

obj_func <- function(age){
  -predict(modelo_salario_edad, newdata = data.frame(age = age))
}

sol <- optimize(obj_func, interval = c(18, 86))

edad_pico <- sol$minimum
edad_pico

# en el caso masculino
modelo_hombres <- lm(data = df[df$sex == "1",], y_ingLab_m_ha ~ age + I(age^2))

pred_func_hombres <- predict(modelo_hombres)
edad_pico_hombres <- with(df[df$sex == "0",], age[which.max(pred_func_hombres)])

edad_pico_hombres

# en el caso femenino 
modelo_mujeres <- lm(data = df[df$sex == "0",], y_ingLab_m_ha ~ age + I(age^2))

pred_func_mujeres <- predict(modelo_mujeres)
edad_pico_mujeres <- with(df[df$sex == "0",], age[which.max(pred_func_mujeres)])
edad_pico_mujeres

## Grafica de predictores edad - salario

##Agregamos una columna con los predictores

df_anes <- df_anes %>% ungroup() %>% mutate(y_hat2 = predict(modelcondic))

ggplot(data = df_anes , 
       mapping = aes(x = age , y = y_hat2 , group=as.factor(sex) , color=as.factor(sex))) +
  geom_point()+scale_color_manual(values = c("blue", "red"))+geom_smooth(method = "nl", se = FALSE) 

stargazer(modelonocond,modelcondic,modelo_bootstrap, type="text", digits=4)

stargazer(modelonocond,modelcondic,modelo_bootstrap, type="Latex", digits=4)

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

# Métricas dentro y fuera completo
r2_in1_com <- R2_Score(y_pred = exp(y_hat_in1_com), y_true = y_train$salario)
rmse_in1_com <- RMSE(y_pred = exp(y_hat_in1_com), y_true = y_train$salario)

r2_out1_com <- R2_Score(y_pred = exp(y_hat_out1_com), y_true = y_test$salario)
rmse_out1_com <- RMSE(y_pred = exp(y_hat_out1_com), y_true = y_test$salario)

# Métricas dentro y fuera edad
r2_in1_edad <- R2_Score(y_pred = exp(y_hat_in1_edad), y_true = y_train$salario)
rmse_in1_edad <- RMSE(y_pred = exp(y_hat_in1_edad), y_true = y_train$salario)

r2_out1_edad <- R2_Score(y_pred = exp(y_hat_out1_edad), y_true = y_test$salario)
rmse_out1_edad <- RMSE(y_pred = exp(y_hat_out1_edad), y_true = y_test$salario)

# Métricas dentro y fuera sexo
r2_in1_sexo <- R2_Score(y_pred = exp(y_hat_in1_sexo), y_true = y_train$salario)
rmse_in1_sexo <- RMSE(y_pred = exp(y_hat_in1_sexo), y_true = y_train$salario)

r2_out1_sexo <- R2_Score(y_pred = exp(y_hat_out1_sexo), y_true = y_test$salario)
rmse_out1_sexo <- RMSE(y_pred = exp(y_hat_out1_sexo), y_true = y_test$salario)


# Métricas dentro y fuera sexo completo
r2_in1_sexo_com <- R2_Score(y_pred = exp(y_hat_in1_sexo_com), y_true = y_train$salario)
rmse_in1_sexo_com <- RMSE(y_pred = exp(y_hat_in1_sexo_com), y_true = y_train$salario)

r2_out1_sexo_com <- R2_Score(y_pred = exp(y_hat_out1_sexo_com), y_true = y_test$salario)
rmse_out1_sexo_com <- RMSE(y_pred = exp(y_hat_out1_sexo_com), y_true = y_test$salario)


modeloStepwise= stepAIC(mod_completo, direction = "both", 
                        trace = TRUE)
summary(modeloStepwise)

modeloStepwise$anova
summary(modeloStepwise)

resultados <- data.frame(Modelo = "Modelo completo", 
                         Muestra = "Train",
                         R2_Score = r2_in1_com, RMSE = rmse_in1_com) %>%
rbind(data.frame(Modelo = "Modelo completo", 
                   Muestra = "TEST",
                   R2_Score = r2_out1_com, RMSE = rmse_out1_com)) %>%
  
  rbind(data.frame(Modelo = "Modelo con edad", 
                   Muestra = "Train",
                   R2_Score = r2_in1_edad, RMSE = rmse_in1_edad)) %>%
  rbind(data.frame(Modelo = "Modelo con edad", 
                   Muestra = "TEST",
                   R2_Score = r2_out1_edad, RMSE = rmse_out1_edad)) %>%
  
  rbind(data.frame(Modelo = "Modelo con sexo", 
                   Muestra = "Train",
                   R2_Score = r2_in1_sexo, RMSE = rmse_in1_sexo)) %>%
  rbind(data.frame(Modelo = "Modelo con sexo", 
                   Muestra = "TEST",
                   R2_Score = r2_out1_sexo, RMSE = rmse_out1_sexo)) %>%
  
  rbind(data.frame(Modelo = "Modelo con sexo, edad y educ", 
                   Muestra = "Train",
                   R2_Score = r2_in1_sexo_com, RMSE = rmse_in1_sexo_com)) %>%
  rbind(data.frame(Modelo = "Modelo con sexo, edad y educ", 
                   Muestra = "TEST",
                   R2_Score = r2_out1_sexo_com, RMSE = rmse_out1_sexo_com))
resultados

stargazer(mod_completo, mod_edad, mod_sexo, type="text")


residuos <- rstandard(mod_completo)
qqnorm(residuos,col="blue")
qqline(residuos,col="red")

residuos <- rstandard(mod_sexo)
qqnorm(residuos,col="blue")
qqline(residuos,col="red")

residuos <- rstandard(mod_edad)
qqnorm(residuos,col="blue")
qqline(residuos,col="red")

# Se genera el modelo lineal, dado que se va a emplear LOOCV no es necesario

# Se emplea la función cv.glm() para la validación LOOCV

mod_edad <- glm(log(salario)~edad+edad_cuadrado, data = df_5)
cv_error_edad <- cv.glm(data = df_5, glmfit =  mod_edad)
cv_error$delta[1]

mod_sexo <- glm(log(salario)~sexo, data = df_5)
cv_error_sexo <- cv.glm(data = df_5, glmfit =  mod_sexo)
cv_error_sexo$delta[1]

mod_sexo_com <- glm(log(salario)~sexo+max_nivel_edu+edad+edad_cuadrado, data = df_5)
cv_error_sexo_com <- cv.glm(data = df_5, glmfit =  mod_sexo_com)
cv_error_sexo_com$delta[1]

mod_completo <- lm(log(salario) ~ ., df_5)
cv_error_completo <- cv.glm(data = df_5, glmfit =  mod_completo)
cv_error_completo$delta[1]

