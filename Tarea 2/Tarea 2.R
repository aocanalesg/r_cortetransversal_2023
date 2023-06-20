###############################################################################
#
# Tarea 2
# Axel Canales
#
###############################################################################


#Working directory
setwd("C:/Users/Axel Canales/Documents/GitHub/r_cortetransversal_2023/Tarea 2")

#install.packages("ggplot2")
library(ggplot2)
library(dplyr)
library(writexl)

#import and examine data

tarea2 <- read.delim("C:/Users/Axel Canales/Documents/GitHub/r_cortetransversal_2023/Tarea 2/LONDON.DAT", sep="")


#Histogram

wfood_hist <- ggplot(data = tarea2, mapping = aes(x = wfood))  +
  geom_histogram(binwidth = 1) +
  labs(title = "Porci�n (%) del presupuesto dedicada a gastos de alimentaci�n")
wfood_hist

#Otra opcion: wfood_hist <- hist(tarea2$wfood)


#Descriptive statistics
desc_stat <- summary(tarea2)
desc_stat

#PDF doc with histogram
pdf(file="graph1.pdf")
wfood_hist
dev.off()

#Cree una nueva variable llamada “gastos_b” que sea igual a la suma de wfood, wfuel, wcloth, walc, y wtrans.
tarea2 <- transform(tarea2,
                    gastos_b=wfood+wfuel+wcloth+walc+wtrans
  
)

#Cree un vector de coeficientes que contenga el promedio, la variancia, el dato mayor y el menor de la serie “gastos_b” y llámele “resumen” y guardelo en un excel.

resumen <- as.data.frame(c(mean(tarea2$gastos_b), var(tarea2$gastos_b), max(tarea2$gastos_b), min(tarea2$gastos_b)),
                         row.names=c("mean", "variance", "max", "min")
                         )
write_xlsx(resumen
           ,"C:/Users/Axel Canales/Documents/GitHub/r_cortetransversal_2023/resumen.xlsx")

#Global environment

save.image(file="Tarea_2_resuelta.RData")