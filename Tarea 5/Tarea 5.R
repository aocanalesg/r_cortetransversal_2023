## Tarea 5 ###
## Axel Omar Canales Garcia

#Working Directory

setwd("/Users/axelcanales/Documents/GitHub/r_cortetransversal_2023/Tarea 5")
library("readxl")
install.packages("stargazer")
library(stargazer)



#####TAREA


## Ejercicio 1: Interprete los coeficientes asociados a la TBP (preste atención a las unidades) y al ITCER. 
##¿Qué se necesitaría para conocer la elasticidad del IMAE a X? Tome en cuenta que cuando una variable regresora
##o dependiente se encuentra en logaritmos, la modificación de las unidades de medida 
#solamente afecta el intercepto de la regresión (¿Por qué?).

datos1<-read_xlsx("Lab6.xlsx", col_names = TRUE, sheet = 1)

modelo_int<-lm(ln_imae ~ ln_ipc + ln_itcer + X + TBP, datos1)

stargazer(modelo_int, type = "text")

print("El coeficiente asociado a la TBP se interpreta como la semielasticidad de la TBP con respecto al IMAE ya que la TBP
      no es afectado por una transformacion logaritmica mientras que el IMAE sí, en particular, manteniendo todo lo demas 
      constantes un aumento de un punto porcentual en la tasa basica pasiva esta asociado a una disminucion de 0.4 por ciento del IMAE.
      
      Por otra parte, ceteris paribus, un aumento de 1 por ciento en el indice de tipo de cambio real, provoca un aumento de 8.7% en el IMAE. 
      Para saber la elasticidad del IMAE a X se necesitaria transformar las exportaciones logaritmicamente y correr la regresion nuevamente.")

## Ejercicio 2: Corra una regresión donde ln(imae)
#sea explicado por una constante, ipc, itcer, x y tbp. 
#Guarde los resultados. Utilizando las mismas variables explicativas, corra una segunda regresión donde la variable dependiente sea el logaritmo natural de imae∗10(log(imae)×10). Compare los resultados de ambas regresiones. ¿Qué le ocurre al error estándar y al estadístico t asociado a cada una de las regresoras? ¿Cuál es el valor exacto de la diferencia de ambas constantes?

modelo_tarea<-lm(ln_imae ~ IPC + ITCER + X + TBP, datos1)

modelo_tarea2<-lm(ln_imae*10*(ln_imae*10) ~ IPC + ITCER + X + TBP, datos1)

stargazer(modelo_tarea, type = "text")

stargazer(modelo_tarea2, type = "text")

require(broom)    
df_modelo_tarea <- tidy(modelo_tarea)
df_modelo_tarea2 <- tidy(modelo_tarea2)
dif <- df_modelo_tarea[1,2]-df_modelo_tarea2[1,2]
print(dif)

print("Puede observarse que los coeficientes y errores estandar han sido reescalados en un factor de 1000. Por otra parte la diferencia de las constantes es: 2118.248, siendo mayor el coeficiente del modelo 2, luego del reescalamiento del IMAE")

save.image("Tarea5_Axel_Canales")
