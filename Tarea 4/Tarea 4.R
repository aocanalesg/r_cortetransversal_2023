#Tarea 4
##Axel Omar Canales Garcia

setwd('/Users/axelcanales/Documents/GitHub/r_cortetransversal_2023/Tarea 4')


#Paso 1. Regresar, guardar coeficientes, matriz con las variables explicativas
datos <-  read.table("LONDON.dat", header = TRUE) 
attach(datos)

modelo_1 <- lm(wcloth ~ wfood + nk + log(income) + wtrans, datos)

coeficientes_modelo_1 <- coefficients(modelo_1)

mat_x <- cbind(rep(1, nrow(datos)), wfood, nk, log(income), wtrans)

colnames(mat_x) <- c("const", "wfood", "nk", "lingreso", "wtrans")

mat_y <- datos$wcloth

names(mat_y) <- c("wcloth")

#Paso 2. 
#Obtenga una matriz que sea el resultado de la multiplicación de mat_x y su transpuesta.

#Obtenga una matriz que sea el resultado de la multiplicación de la matriz anterior y su inversa.

#Obtenga una matriz que sea el resultado de la inversa de la primera matriz computada dividida entre el promedio de la serie wfuel.

mat_ej2_1 <- t(mat_x) %*% mat_x

mat_ej2_2 <- mat_ej2_1 %*% solve(mat_ej2_1)

mat_ej2_3 <- solve(mat_ej2_1)/mean(wfuel)


#Paso 3
beta_coef <- solve(t(mat_x)%*%mat_x)%*%t(mat_x)%*%mat_y

t(beta_coef)


coeficientes_modelo_1

#Paso 4
mat_cov_modelo_1 <- vcov(modelo_1)

sigma_cuad_hat <- (t(resid(modelo_1)) %*% resid(modelo_1))/(nrow(mat_x)-ncol(mat_x))

#Note que sigma_cuad_hat R lo reconoce como un matriz 1X1!! Hay que volverlo un escalar!!
mat_cov_matrices <- as.numeric(sigma_cuad_hat)*(solve(t(mat_x)%*%mat_x))

mat_cov_modelo_1

mat_cov_matrices