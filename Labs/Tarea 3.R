setwd('/Users/axelcanales/Documents/GitHub/r_cortetransversal_2023/Labs')
library(ggplot2)
datos <-  read.table("LONDON.dat", header = TRUE) 
attach(datos)
ggplot(data = datos, mapping = aes(x = totexp, y = walc)) +
  geom_point()

#La función lm significa "linear model" del paquete stats y está integrada en el R base.
#Generamos el objeto modelo_1 que contendrá la estimación

modelo_1 <- lm(formula =  walc ~ log(totexp) + age + nk, data = datos)

summary(modelo_1)

#Accedemos a parte de los residuos de la regresión con la indexación de la lista

#primera forma
head(modelo_1$residuals)

#segunda forma
head(modelo_1[[2]])

#Predicción del modelo e intervalos de confianza de la predicción y de los coeficientes
prediccion_modelo_1<-predict.lm(modelo_1, se.fit = TRUE, level = 0.95, interval = c("confidence"))

head(prediccion_modelo_1$fit)


#Grafico de residuos MCO

#Primero se cree un objeto con los datos a graficar
#La función cbind pega columnas, luego la función names asigna nombres a las columnas, se debe transformar de matriz a data.frame

datos_regresion <- cbind(datos$walc, fitted.values(modelo_1), resid(modelo_1))
datos_regresion<-as.data.frame(datos_regresion)
names(datos_regresion)<-c("walc", "fitted", "residuos")

head(datos_regresion)

#Primero el gráfico de los residuos
ggplot(data = datos_regresion,
       mapping = aes(x = seq(1:length(residuos)),
                     y = residuos)) +
  geom_line()

#Segundo el gráfico de los valores observados y ajustados
#Se transforman los datos para su uso

library(tidyr)
datos_regresion1<-datos_regresion %>% 
  pivot_longer(  cols = c(1,2), 
                 names_to = "walc_names", 
                 values_to = "walc_regression" 
  )

ggplot(data = datos_regresion1,
       mapping = aes(x = seq(1:length(residuos)),
                     y = walc_regression, color = as.factor(walc_names))) +
  geom_line()+
  scale_color_manual( values=c('#009E73', '#D55E00'), name='walc', labels=c('Observado', 'Predicho') )

#TAREA 3
#Pronóstico con la regresión estimada

#Tarea 3: Escriba un programa que:

#Efectúe un pronóstico dentro de muestra para la serie wfood.

#Construya un intervalo de confianza del 95% para la serie pronosticada.

#Grafique la serie de pronóstico con los intervalos de confianza.

#Analisis exploratorio

ggplot(data = datos, mapping = aes(x = totexp, y = wfood)) +
  geom_point()  
       
#modelo
modelo_2 <- lm(formula =  wfood ~ log(totexp)  + age + nk, data = datos)
summary(modelo_2)

install.packages('tidyverse')
library(tidyverse)
install.packages('stargazer')
install.packages('patchwork')
library(stargazer)
library(patchwork)
library(ggplot2)

ci90 <- predict(modelo_2, datos, interval = "confidence", level = 0.90)
ci95 <- predict(modelo_2, datos, interval = "confidence", level = 0.95)
ci99 <- predict(modelo_2, datos, interval = "confidence", level = 0.99)

ci_pred <- as.data.frame(ci90) %>% 
  mutate(ID = "90") %>% 
  bind_rows(as.data.frame(ci95) %>% 
              mutate(ID = "95"),
            as.data.frame(ci99) %>% 
              mutate(ID = "99")
  )
head(ci_pred)


final_data <- datos %>% 
  bind_cols(
    ci_pred %>%
      remove_rownames() %>% 
      pivot_wider(names_from = ID, values_from = c(lwr, upr)) %>% 
      unnest()
  ) %>% 
  pivot_longer(names_to = "key", values_to = "value", cols = 12:17) %>% 
  separate(key, into = c("Type", "Level"), sep = "_") %>% 
  pivot_wider(names_from = Type, values_from = value) %>%
  unnest()

head(final_data)


final_data %>% 
  ggplot(aes(x = totexp, y = wfood, color = Level)) +
 # geom_point(colour = "midnightblue", alpha = 0.5) +
  geom_line(aes(y = fit), lty = 1) +
  geom_line(aes(y = lwr), lty = 2) +
  geom_line(aes(y = upr), lty = 2) +
 # labs(x = "Log(totexp)", y = "wfood", color = NULL) +
  theme_test() +
  theme(legend.position = c(0.08, 0.85))

ggplot(data = final_data,
       mapping = aes(x = seq(1:length(fit)),
                     y = fit, color = as.factor(Level))) +
  geom_line()+
  scale_color_manual( values=c('#009E73', '#D55E00',  '#D52E99'), name='walc', labels=c('Observado', 'Predicho') )


#Tarea 4
