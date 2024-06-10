# Cargar las librerías necesarias

library(e1071)
library(MASS)
library(fitdistrplus)
library(DescTools)
library(PropCIs)
library(rcompanion)
library(EstimationTools)
library(boot)
library(survival)
library(TeachingDemos)
library(boot)
library(stats4)

# muestra de datos
Sample1 <- c(5.243546, 3.624798, 4.597245, 6.814677, 4.32791, 4.259092, 5.952399, 5.114901, 4.316348, 3.831358, 6.958219, 4.113977, 6.531121, 4.84677, 7.125954, 6.052483, 5.057894, 5.444031, 3.220108, 4.597651)



Sample5 <- c(5.243546, 3.624798, 4.597245, 6.814677, 4.32791, 4.259092, 5.952399, 5.114901, 4.316348, 3.831358, 
             6.958219, 4.113977, 6.531121, 4.84677, 7.125954, 6.052483, 5.057894, 5.444031, 3.220108, 4.597651)

Sample6 <- c(4.144823, 6.016923, 4.582143, 4.365151, 4.403069, 4.126845, 4.761123, 6.210473, 5.3924, 4.326786, 
             7.038215, 6.206891, 5.409031, 5.185587, 5.468273, 5.248758, 5.12194, 5.378915, 4.730653, 5.358759)

Sources.Total <- c(19973, 604, 6507, 615355, 2897, 2396, 85009, 21776.6, 3526.8, 827, 920914, 1209.9, 319259, 9736.8, 1373998, 102758, 15709, 30742, 298, 5448)
Uses.Total <- c(7472, 302, 2489, 354978, 1542, 684, 30897, 15140, 5182, 498, 470380, 931, 102146, 4855, 503198, 20220, 9904, 8754, 92, 2231)
Revenue.Total <- c(175204, 4214, 39558, 6526450, 21276, 18158, 896186, 130286, 20717, 6781, 9082792, 13000, 3397201, 70269, 13364533, 1128452, 114259, 277990, 1659, 39595)

Uses.Retail <- c(5.243546, 3.624798, 4.597245, 6.814677, 4.32791, 4.259092, 5.952399, 5.114901, 4.316348, 3.831358, 
                 6.958219, 4.113977, 6.531121, 4.84677, 7.125954, 6.052483, 5.057894, 5.444031, 3.220108, 4.597651)
Uses.Losses <- c(3.937016, 0, 0, 5.298058, 3.027757, 2.78746, 4.387194, 3.648555, 0, 0, 
                 5.630245, 0, 5.370224, 3.713407, 5.402688, 4.52952, 3.969229, 3.88167, 0, 3.615634)


#PREGUNTA 1
# Calcular Q1
Q1_sample1 <- quantile(Sample1, probs = 0.25)
# resultado
Q1_sample1

#PREGUNTA 2
# Calcular Q3
Q3_sample1 <- quantile(Sample1, probs = 0.75)
# resultado
Q3_sample1

#PREGUNTA 3
# Calcular Q1 (primer cuartil)
Q1_sample1 <- quantile(Sample1, probs = 0.25)
# Realizar la prueba t para H0: mu > Q1, H1: mu <= Q1
t_test_result <- t.test(Sample1, mu = Q1_sample1, alternative = "less"
# Calcular el estadístico t manualmente
n <- length(Sample1)
mean_sample1 <- mean(Sample1)
sd_sample1 <- sd(Sample1)
t_statistic <- (mean_sample1 - Q1_sample1) / (sd_sample1 / sqrt(n))
# Mostrar los resultados
t_statistic

#PREGUNTA 4
# Calcular el primer cuartil (Q1)
Q1 <- quantile(sample1, 0.25)
# Prueba t unilateral para H0: mu > Q1, H1: mu <= Q1
t_test_result <- t.test(sample1, mu = Q1, alternative = "less")
# Calcular los grados de libertad
n <- length(sample1) - 1
# Imprimir grados de libertad
print(n)
                        
#PREGUNTA 5
# Calcular Q1 (primer cuartil)
Q1_sample1 <- quantile(Sample1, probs = 0.25)
# Realizar la prueba t para H0: mu > Q1, H1: mu <= Q1
t_test_result <- t.test(Sample1, mu = Q1_sample1, alternative = "less")
# Extraer el p-valor
p_value <- t_test_result$p.value
# Mostrar los resultados
print(paste("El primer cuartil (Q1) de Sample1 es:", Q1_sample1))
print(paste("El p-valor de la prueba t es:", p_value))

#PREGUNTA 6
# Calcular Q1 (primer cuartil)
Q1_sample1 <- quantile(Sample1, probs = 0.25)
# Calcular la media y desviación estándar de la muestra
mean_sample1 <- mean(Sample1)
sd_sample1 <- sd(Sample1)
# Calcular el tamaño de la muestra
n <- length(Sample1)
# Calcular el estadístico t manualmente
t_statistic <- (mean_sample1 - Q1_sample1) / (sd_sample1 / sqrt(n))
# Realizar la prueba t para H0: mu < Q1, H1: mu >= Q1
t_test_result <- t.test(Sample1, mu = Q1_sample1, alternative = "greater")
# Mostrar los resultados
print(paste("El primer cuartil (Q1) de Sample1 es:", Q1_sample1))
print(paste("El estadístico t calculado es:", t_statistic))
                        
#PREGUNTA 7
# Calcular Q1 (primer cuartil)
Q1_sample1 <- quantile(Sample1, probs = 0.25)
# Calcular el tamaño de la muestra
n <- length(Sample1)
#Calcular los grados de libertad
df <- n - 1
# Realizar la prueba t para H0: mu < Q1, H1: mu >= Q1
t_test_result <- t.test(Sample1, mu = Q1_sample1, alternative = "greater")
# Mostrar los resultados
print(paste("El primer cuartil (Q1) de Sample1 es:", Q1_sample1))
print(paste("El tamaño de la muestra (n) es:", n))
print(paste("Los grados de libertad (n-1) son:", df))
                        
#PREGUNTA 8
# Calcular Q1 (primer cuartil)
Q1_sample1 <- quantile(Sample1, probs = 0.25)
# Realizar la prueba t para H0: mu < Q1, H1: mu >= Q1
t_test_result <- t.test(Sample1, mu = Q1_sample1, alternative = "greater")
# Extraer el p-valor
p_value <- t_test_result$p.value
# Mostrar los resultados
print(paste("El primer cuartil (Q1) de Sample1 es:", Q1_sample1))
print(paste("El p-valor de la prueba t es:", p_value))

                        
#PREGUNTA 9
# Calcular Q1 (primer cuartil)
Q1_sample1 <- quantile(Sample1, probs = 0.25)
# Calcular la media y desviación estándar de la muestra
mean_sample1 <- mean(Sample1)
sd_sample1 <- sd(Sample1)
# Calcular el tamaño de la muestra
n <- length(Sample1)
# Calcular el estadístico t manualmente
t_statistic <- (mean_sample1 - Q1_sample1) / (sd_sample1 / sqrt(n))
# Realizar la prueba t para H0: mu == Q1, H1: mu != Q1
t_test_result <- t.test(Sample1, mu = Q1_sample1, alternative = "two.sided")
# Mostrar los resultados
print(paste("El primer cuartil (Q1) de Sample1 es:", Q1_sample1))
print(paste("El estadístico t calculado es:", t_statistic))
                        
#PREGUNTA 10
# Calcular Q1 (primer cuartil)
Q1_sample1 <- quantile(Sample1, probs = 0.25)
# Calcular el tamaño de la muestra
n <- length(Sample1)
# Calcular los grados de libertad
df <- n - 1
# Realizar la prueba t para H0: mu == Q1, H1: mu != Q1
t_test_result <- t.test(Sample1, mu = Q1_sample1, alternative = "two.sided")
# Mostrar los resultados
print(paste("El primer cuartil (Q1) de Sample1 es:", Q1_sample1))
print(paste("El tamaño de la muestra (n) es:", n))
print(paste("Los grados de libertad (n-1) son:", df))
                        
#PREGUNTA 11
# Calcular Q1 (primer cuartil)
Q1_sample1 <- quantile(Sample1, probs = 0.25)
# Realizar la prueba t para H0: mu == Q1, H1: mu != Q1
t_test_result <- t.test(Sample1, mu = Q1_sample1, alternative = "two.sided")
# Extraer el p-valor
p_value <- t_test_result$p.value
# Mostrar los resultados
print(paste("El primer cuartil (Q1) de Sample1 es:", Q1_sample1))
print(paste("El p-valor de la prueba t es:", p_value))
                        
#PREGUNTA 12
# Calcular Q3 (tercer cuartil)
Q3_sample1 <- quantile(Sample1, probs = 0.75)
# Calcular la media y desviación estándar de la muestra
mean_sample1 <- mean(Sample1)
sd_sample1 <- sd(Sample1)
# Calcular el tamaño de la muestra
n <- length(Sample1)
# Calcular el estadístico t manualmente
t_statistic <- (mean_sample1 - Q3_sample1) / (sd_sample1 / sqrt(n))
# Realizar la prueba t para H0: mu > Q3, H1: mu <= Q3
t_test_result <- t.test(Sample1, mu = Q3_sample1, alternative = "less")
# Mostrar los resultados
print(paste("El tercer cuartil (Q3) de Sample1 es:", Q3_sample1))
print(paste("El estadístico t calculado es:", t_statistic))
print(t_test_result)
                        
#PREGUNTA 13
# Calcular Q3 (tercer cuartil)
Q3_sample1 <- quantile(Sample1, probs = 0.75)
# Calcular el tamaño de la muestra
n <- length(Sample1)
# Calcular los grados de libertad
df <- n - 1
# Realizar la prueba t para H0: mu > Q3, H1: mu <= Q3
t_test_result <- t.test(Sample1, mu = Q3_sample1, alternative = "less")
# Mostrar los resultados
print(paste("El tercer cuartil (Q3) de Sample1 es:", Q3_sample1))
print(paste("El tamaño de la muestra (n) es:", n))
print(paste("Los grados de libertad (n-1) son:", df))
                        
#PREGUNTA 14
# Calcular Q3 (tercer cuartil)
Q3_sample1 <- quantile(Sample1, probs = 0.75)
# Realizar la prueba t para H0: mu > Q3, H1: mu <= Q3
t_test_result <- t.test(Sample1, mu = Q3_sample1, alternative = "less")
# Extraer el p-valor
p_value <- t_test_result$p.value
# Mostrar los resultados
print(paste("El tercer cuartil (Q3) de Sample1 es:", Q3_sample1))
print(paste("El p-valor de la prueba t es:", p_value))
                        
#PREGUNTA 15
# Calcular Q3 (tercer cuartil)
Q3_sample1 <- quantile(Sample1, probs = 0.75)
# Calcular la media y desviación estándar de la muestra
mean_sample1 <- mean(Sample1)
sd_sample1 <- sd(Sample1)
# Calcular el tamaño de la muestra
n <- length(Sample1)
# Calcular el estadístico t manualmente
t_statistic <- (mean_sample1 - Q3_sample1) / (sd_sample1 / sqrt(n))
# Realizar la prueba t para H0: mu < Q3, H1: mu >= Q3
t_test_result <- t.test(Sample1, mu = Q3_sample1, alternative = "greater")
# Mostrar los resultados
print(paste("El tercer cuartil (Q3) de Sample1 es:", Q3_sample1))
print(paste("El estadístico t calculado es:", t_statistic))
                        
#PREGUNTA 16
# Calcular Q3 (tercer cuartil)
Q3_sample1 <- quantile(Sample1, probs = 0.75)
# Calcular el tamaño de la muestra
n <- length(Sample1)
# Calcular los grados de libertad
df <- n - 1
# Realizar la prueba t para H0: mu < Q3, H1: mu >= Q3
t_test_result <- t.test(Sample1, mu = Q3_sample1, alternative = "greater")
# Mostrar los resultados
print(paste("El tercer cuartil (Q3) de Sample1 es:", Q3_sample1))
print(paste("El tamaño de la muestra (n) es:", n))
print(paste("Los grados de libertad (n-1) son:", df))
                        
#PREGUNTA 17
# Calcular Q3 (tercer cuartil)
Q3_sample1 <- quantile(Sample1, probs = 0.75)
# Realizar la prueba t para H0: mu < Q3, H1: mu >= Q3
t_test_result <- t.test(Sample1, mu = Q3_sample1, alternative = "greater")
# Extraer el p-valor
p_value <- t_test_result$p.value
# Mostrar los resultados
print(paste("El tercer cuartil (Q3) de Sample1 es:", Q3_sample1))
print(paste("El p-valor de la prueba t es:", p_value))
                        
#PREGUNTA 18
# Calcular Q3 (tercer cuartil)
Q3_sample1 <- quantile(Sample1, probs = 0.75)
# Calcular la media y desviación estándar de la muestra
mean_sample1 <- mean(Sample1)
sd_sample1 <- sd(Sample1)
# Calcular el tamaño de la muestra
n <- length(Sample1)
# Calcular el estadístico t manualmente
t_statistic <- (mean_sample1 - Q3_sample1) / (sd_sample1 / sqrt(n))
# Realizar la prueba t para H0: mu == Q3, H1: mu != Q3
t_test_result <- t.test(Sample1, mu = Q3_sample1, alternative = "two.sided")
# Mostrar los resultados
print(paste("El estadístico t calculado es:", t_statistic))
                        
#PREGUNTA 19
# Calcular Q3 (tercer cuartil)
Q3_sample1 <- quantile(Sample1, probs = 0.75)
# Calcular el tamaño de la muestra
n <- length(Sample1)
# Calcular los grados de libertad
df <- n - 1
# Realizar la prueba t para H0: mu == Q3, H1: mu != Q3
t_test_result <- t.test(Sample1, mu = Q3_sample1, alternative = "two.sided")
# Mostrar los resultados
print(paste("El tercer cuartil (Q3) de Sample1 es:", Q3_sample1))
print(paste("El tamaño de la muestra (n) es:", n))
print(paste("Los grados de libertad (n-1) son:", df))
                        
#PREGUNTA 20
# Calcular Q3 (tercer cuartil)
Q3_sample1 <- quantile(Sample1, probs = 0.75)
# Realizar la prueba t para H0: mu == Q3, H1: mu != Q3
t_test_result <- t.test(Sample1, mu = Q3_sample1, alternative = "two.sided")
# Extraer el p-valor
p_value <- t_test_result$p.value
# Mostrar los resultados
print(paste("El tercer cuartil (Q3) de Sample1 es:", Q3_sample1))
print(paste("El p-valor de la prueba t es:", p_value))
                        
#PREGUNTA 21
# Calcular la varianza muestral
var_sample1 <- var(Sample1)
# Calcular el tamaño de la muestra
n <- length(Sample1)
# Varianza bajo la hipótesis nula
var_h0 <- 1
# Calcular el estadístico d
d_statistic <- (n - 1) * var_sample1 / var_h0
# Mostrar los resultados
print(paste("La varianza muestral (s^2) es:", var_sample1))
print(paste("El tamaño de la muestra (n) es:", n))
print(paste("El estadístico d calculado es:", d_statistic))
                      
#PREGUNTA 22
# Calcular la varianza muestral
var_sample1 <- var(Sample1)
# Calcular el tamaño de la muestra
n <- length(Sample1)
# Calcular los grados de libertad
df <- n - 1
# Varianza bajo la hipótesis nula
var_h0 <- 1
# Calcular el estadístico d
d_statistic <- (n - 1) * var_sample1 / var_h0
# Mostrar los resultados
print(paste("Los grados de libertad (n-1) son:", df))
                       
#PREGUNTA 23

                  
#PREGUNTA 24
# Calcular las medias y desviaciones estándar de las muestras
mean_sample1 <- mean(Sample1)
mean_sample2 <- mean(Sample2)
sd_sample1 <- sd(Sample1)
sd_sample2 <- sd(Sample2)
# Calcular el tamaño de las muestras
n1 <- length(Sample1)
n2 <- length(Sample2)
# Calcular el estadístico t para dos muestras independientes
pooled_sd <- sqrt(((n1 - 1) * sd_sample1^2 + (n2 - 1) * sd_sample2^2) / (n1 + n2 - 2))
t_statistic <- (mean_sample1 - mean_sample2) / (pooled_sd * sqrt(1/n1 + 1/n2))
# Realizar la prueba t para dos muestras independientes
t_test_result <- t.test(Sample1, Sample2, alternative = "two.sided", var.equal = TRUE)
# Mostrar los resultados
print(paste("El estadístico t calculado es:", t_statistic))
                        
#PREGUNTA 25

                        
#PREGUNTA 26
# Realizar la prueba t para dos muestras independientes
t_test_result <- t.test(Sample1, Sample2, alternative = "two.sided", var.equal = TRUE)
# Mostrar el p-valor de la prueba t
print(t_test_result$p.value)
                        
#PREGUNTA 27
# Calcular las varianzas muestrales
var_sample1 <- var(Sample1)
var_sample2 <- var(Sample2)
# Calcular el estadístico F
F_statistic <- var_sample1 / var_sample2
# Mostrar el estadístico F calculado
print(F_statistic)

                        
#PREGUNTA 28
# Calcular el tamaño de las muestras
n1 <- length(Sample1)
n2 <- length(Sample2)
# Calcular los grados de libertad
df1 <- n1 - 1
df2 <- n2 - 1
# Mostrar los grados de libertad
print(paste("Grados de libertad para Sample1 (df1):", df1))
print(paste("Grados de libertad para Sample2 (df2):", df2))
                        
#PREGUNTA 29
# Prueba de varianzas
var_test_result <- var.test(sample1, sample2)
# Imprimir p-value
print(var_test_result$p.value)
                        
#PREGUNTA 30
# Prueba de Pearson para normalidad
library(nortest)
pearson_test_result <- pearson.test(sample3)
# Imprimir estadístico Pearson chi-square
print(pearson_test_result$statistic)
                        
#PREGUNTA 31
# Prueba de Pearson para normalidad
library(nortest)
pearson_test_result <- pearson.test(sample3)
# Imprimir número de clases usadas en el test
print(pearson_test_result$n.classes)
                        
#PREGUNTA 32
# Prueba de Pearson para normalidad
library(nortest)
pearson_test_result <- pearson.test(sample3)
# Imprimir p-value
print(pearson_test_result$p.value)
                        
#PREGUNTA 33
# Prueba Kolmogorov-Smirnov para normalidad
ks_test_result <- ks.test(sample3, "pnorm", mean(sample3), sd(sample3))
# Imprimir estadístico de la prueba Kolmogorov-Smirnov
print(ks_test_result$statistic)
                       
#PREGUNTA 34
# Prueba Kolmogorov-Smirnov para normalidad
ks_test_result <- ks.test(sample3, "pnorm", mean(sample3), sd(sample3))
# Imprimir p-value de la prueba Kolmogorov-Smirnov
print(ks_test_result$p.value)
                       
#PREGUNTA 35
# Crear tabla de frecuencias observadas
observed <- table(sample4)
# Número de categorías
n <- length(observed)
# Crear distribución uniforme discreta esperada
expected <- rep(sum(observed) / n, n)
# Prueba chi-cuadrado de homogeneidad
chi_sq_test_result <- chisq.test(observed, p = expected / sum(expected))
# Imprimir estadístico chi-squared
print(chi_sq_test_result$statistic)
                        
#PREGUNTA 36
# Crear tabla de frecuencias observadas
observed <- table(sample4)
# Prueba chi-cuadrado de homogeneidad
chi_sq_test_result <- chisq.test(observed, p = rep(1/length(observed), length(observed)))
# Imprimir grados de libertad
print(chi_sq_test_result$parameter)
                        
#PREGUNTA 37
# Crear tabla de frecuencias observadas
observed <- table(sample4)
# Prueba chi-cuadrado de homogeneidad
chi_sq_test_result <- chisq.test(observed, p = rep(1/length(observed), length(observed)))
# Imprimir p-value
print(chi_sq_test_result$p.value)
                        
#PREGUNTA 38
# Prueba de Wilcoxon para muestras no pareadas (Mann-Whitney)
wilcox_test_result <- wilcox.test(sample5, sample6)
# Imprimir estadístico de la prueba de Wilcoxon
print(wilcox_test_result$statistic)
                        
#PREGUNTA 39

                        
#PREGUNTA 40
# Prueba de Wilcoxon para muestras no apareadas (Mann-Whitney)
wilcox_test_result <- wilcox.test(sample5, sample6)
# Imprimir p-value de la prueba de Wilcoxon
print(wilcox_test_result$p.value)
                        
#PREGUNTA 41
library(lmtest)
# Modelo de regresión lineal
model <- lm(Revenue.Total ~ Sources.Total + Uses.Total)
# Prueba de Durbin-Watson
dw_test_result <- dwtest(model)
# stadístico dwtest
q4241_dw_statistic <- dw_test_result$statistic
q4241_dw_statistic
                        
#PREGUNTA 42

                        
#PREGUNTA 43
# Crear el modelo de regresión lineal
model <- lm(Uses.Losses ~ Uses.Retail)
summary(model)
# Calcular Beta0 (intercepto)
Beta0 <- coef(model)[1]
Beta0

                        
#PREGUNTA 44

                        
#PREGUNTA 45
# Calcular R2
R2 <- summary(model)$r.squared
R2

                        
#PREGUNTA 46
# Calcular el p-valor del contraste de hipótesis de Beta0 == 0
p_value_Beta0 <- summary(model)$coefficients[1, 4]
p_value_Beta0

                        
#PREGUNTA 47
# Calcular el p-valor del contraste de hipótesis de Beta1 == 0
p_value_Beta1 <- summary(model)$coefficients[2, 4]
p_value_Beta1

#PREVIO A LA 48 EN ADELANTE-----------------------------
# Definir los vectores de datos
Uses.Retail <- c(5.243546, 3.624798, 4.597245, 6.814677, 4.32791, 4.259092, 5.952399, 5.114901, 4.316348, 3.831358, 
                 6.958219, 4.113977, 6.531121, 4.84677, 7.125954, 6.052483, 5.057894, 5.444031, 3.220108, 4.597651)
Uses.Losses <- c(3.937016, 0, 0, 5.298058, 3.027757, 2.78746, 4.387194, 3.648555, 0, 0, 
                 5.630245, 0, 5.370224, 3.713407, 5.402688, 4.52952, 3.969229, 3.88167, 0, 3.615634)

# Crear el modelo de regresión lineal
model <- lm(Uses.Losses ~ Uses.Retail)
summary(model)
                       
#PREGUNTA 48

                        
#PREGUNTA 49

                        
#PREGUNTA 50
# Calcular la estimación puntual de la predicción del mínimo de Uses.Retail
min_Uses_Retail <- min(Uses.Retail)
prediction_min <- predict(model, newdata = data.frame(Uses.Retail = min_Uses_Retail))
prediction_min

#PREGUNTA 51

#PREGUNTA 52


#PREGUNTA 53
# Definir los vectores de datos
Uses.Retail <- c(5.243546, 3.624798, 4.597245, 6.814677, 4.32791, 4.259092, 5.952399, 5.114901, 4.316348, 3.831358, 
                 6.958219, 4.113977, 6.531121, 4.84677, 7.125954, 6.052483, 5.057894, 5.444031, 3.220108, 4.597651)
Uses.Losses <- c(3.937016, 0, 0, 5.298058, 3.027757, 2.78746, 4.387194, 3.648555, 0, 0, 
                 5.630245, 0, 5.370224, 3.713407, 5.402688, 4.52952, 3.969229, 3.88167, 0, 3.615634)

# Crear el modelo de regresión lineal
model <- lm(Uses.Losses ~ Uses.Retail)
summary(model)

# Calcular la estimación puntual de la predicción de la media de Uses.Retail
mean_Uses_Retail <- mean(Uses.Retail)
prediction_mean <- predict(model, newdata = data.frame(Uses.Retail = mean_Uses_Retail))
prediction_mean

#PREGUNTA 54

#PREGUNTA 55

#PREGUNTA 56
# Calcular la estimación puntual de la predicción del máximo de Uses.Retail
max_Uses_Retail <- max(Uses.Retail)
prediction_max <- predict(model, newdata = data.frame(Uses.Retail = max_Uses_Retail))
prediction_max


#PREGUNTA 57

#PREGUNTA 58




..........................................................................................................................................................................................................................................................................................................................




                        
install.packages("nortest", dependencies=TRUE)
library(nortest)

#PREGUNTA 1
Sample1 <- c(1.149219, 0.7403627, 1.705864, 0.30103, 1.305351, 1.399674, 1.245513, 1.313867,
             2.531479, 1.457882, 3.562673, 0, 1.586587, 1.176091, 0, 1.900367, 0, 0.8750613,
             0, 0.2787536)
# Calcular el primer cuartil (Q1)
Q1 <- quantile(Sample1, 0.25)

# Pregunta 1: Contrastar H0: mu < Q1 contra H1: mu >= Q1, alpha = 0.95
# Utilizar una prueba t de una muestra
t_test_below_Q1 <- t.test(Sample1, alternative = "greater", mu = Q1)
cat("Pregunta 1: P-value = ", t_test_below_Q1$p.value, "\n")

#PREGUNTA 2
# Pregunta 2: Contrastar H0: mu == Q1 contra H1: mu != Q1, alpha = 0.95
# Calcular el estadístico t de una muestra
t_test_equal_Q1 <- t.test(Sample1, mu = Q1)
cat("Pregunta 2: Estadístico t = ", t_test_equal_Q1$statistic, "\n")

#PREGUNTA 3
# Pregunta 3: Calcular los grados de libertad para la pregunta 2
# Los grados de libertad en una prueba t de una muestra son n-1
degrees_of_freedom <- length(Sample1) - 1
cat("Pregunta 3: Grados de libertad = ", degrees_of_freedom, "\n")

#PREGUNTA 4 
# Calcular el tercer cuartil (Q3)
Q3 <- quantile(Sample1, 0.75)

# Pregunta 4: Contrastar H0: mu == Q1 contra H1: mu != Q1, alpha=0.95
# Utilizar una prueba t de una muestra para el cálculo del p-value
t_test_equal_Q1_Q4 <- t.test(Sample1, mu = quantile(Sample1, 0.25))
cat("Pregunta 4: P-value = ", t_test_equal_Q1_Q4$p.value, "\n")

#PREGUNTA 5 
# Pregunta 5 y Pregunta 7: Contrastar H0: mu > Q3 contra H1: mu <= Q3, alpha=0.95
# Calculamos el estadístico t y el p-value para esta hipótesis
t_test_above_Q3 <- t.test(Sample1, alternative = "less", mu = Q3)
cat("Pregunta 5: Estadístico t = ", t_test_above_Q3$statistic, "\n")

#PREGUNTA 6
# Grados de libertad para las pruebas que involucran Q3 (igual para todas)
degrees_of_freedom_Q3 <- length(Sample1) - 1
cat("Pregunta 6: Grados de libertad = ", degrees_of_freedom_Q3, "\n")

#PREGUNTA 7 
cat("Pregunta 7: P-value = ", t_test_above_Q3$p.value, "\n")

#PREGUNTA 8 
# Pregunta 8: Contrastar H0: mu < Q3 contra H1: mu >= Q3, alpha=0.95
# Calculamos el estadístico t para esta hipótesis alternativa
t_test_below_Q3 <- t.test(Sample1, alternative = "greater", mu = Q3)
cat("Pregunta 8: Estadístico t = ", t_test_below_Q3$statistic, "\n")

#PREGUNTA 9 
degrees_of_freedom_Q3 <- length(Sample1) - 1
cat("Pregunta 9: Grados de libertad = ", degrees_of_freedom_Q3, "\n")

#PREGUNTA 10 
# Calcular el p-value para H0: mu < Q3 contra H1: mu >= Q3
Q3 <- quantile(Sample1, 0.75)
t_test_below_Q3 <- t.test(Sample1, alternative = "greater", mu = Q3)
cat("Pregunta 10: P-value = ", t_test_below_Q3$p.value, "\n")

#PREGUNTA 11 
# Calcular el estadístico t para H0: mu == Q3 contra H1: mu != Q3
t_test_equal_Q3 <- t.test(Sample1, mu = Q3)
cat("Pregunta 11: Estadístico t = ", t_test_equal_Q3$statistic, "\n")

#PREGUNTA 12 
# Los grados de libertad son los mismos para todas las pruebas t con esta muestra
cat("Pregunta 12: Grados de libertad = ", degrees_of_freedom_Q3, "\n")

#PREGUNTA 13 
# Calcular el p-value para H0: mu == Q3 contra H1: mu != Q3
cat("Pregunta 13: P-value = ", t_test_equal_Q3$p.value, "\n")

#PREGUNTA 14 
#PREGUNTA 15 
#PREGUNTA 16 
n <- length(Sample1)
degrees_of_freedom_var <- n - 1
cat("Pregunta 16: Grados de libertad = ", degrees_of_freedom_var, "\n")

#PREGUNTA 17 
#PREGUNTA 18 
#PREGUNTA 19 
#PREGUNTA 20 
#PREGUNTA 21  
degrees_of_freedom_var <- n - 1
cat("Pregunta 21: Grados de libertad = ", degrees_of_freedom_var, "\n")

#PREGUNTA 22
Q1 <- quantile(Sample1, 0.25)
cat("Pregunta 22: Q1 = ", Q1, "\n")

#PREGUNTA 23 
#PREGUNTA 24 
# Realiza la prueba Pearson chi-square para normalidad
pearson_test <- pearson.test(sample1)

# Estadístico chi-cuadrado
pearson_test$statistic

#PREGUNTA 25 
#PREGUNTA 26 
# P-value de la prueba de Pearson
cat("Pregunta 26: P-value = ", pearson_result$p.value, "\n")

#PREGUNTA 27 
#PREGUNTA 28 
#PREGUNTA 29 
#PREGUNTA 30 
#PREGUNTA 31 
#PREGUNAT 32
# Datos para Sample1
Sample1 <- c(1.149219, 0.7403627, 1.705864, 0.30103, 1.305351, 1.399674, 1.245513, 1.313867, 2.531479, 1.457882, 3.562673, 0, 1.586587, 1.176091, 0, 1.900367, 0, 0.8750613, 0, 0.2787536)

# Calcular Q3
Q3 <- quantile(Sample1, 0.75)
cat("Pregunta 32: Q3 = ", Q3, "\n")

#PREGUNTA 33
#PREGUNTA 34
#PREGUNTA 35
#PREGUNTA 36 
# Calcular Q1 de Sample1
Q1_Sample1 <- quantile(Sample1, 0.25)

# Prueba t
t_test_below_Q1 <- t.test(Sample1, mu = Q1_Sample1, alternative = "less")
cat("Pregunta 36: Estadístico t = ", t_test_below_Q1$statistic, "\n")

#PREGUNAT 37
# Grados de libertad para la prueba t
df_t_test <- length(Sample1) - 1
cat("Pregunta 37: Grados de libertad = ", df_t_test, "\n")

#PREGUNTA 38
# Prueba t p-value
p_value_t_test <- t_test_below_Q1$p.value
cat("Pregunta 38: p-value = ", p_value_t_test, "\n")

#PREGUNTA 39
# Suponiendo que Sample1 ya está definido
Sample1 <- c(1.149219, 0.7403627, 1.705864, 0.30103, 1.305351, 1.399674, 1.245513, 1.313867, 2.531479, 1.457882, 3.562673, 0, 1.586587, 1.176091, 0, 1.900367, 0, 0.8750613, 0, 0.2787536)
Q1_Sample1 <- quantile(Sample1, 0.25)

# Realizar la prueba t
t_test_greater_Q1 <- t.test(Sample1, mu = Q1_Sample1, alternative = "greater")

# Mostrar el estadístico t
cat("Pregunta 39: Estadístico t = ", t_test_greater_Q1$statistic, "\n")

#PREGUNTA 40
# Grados de libertad para la prueba t
df_t_test <- length(Sample1) - 1
cat("Pregunta 40: Grados de libertad = ", df_t_test, "\n")

