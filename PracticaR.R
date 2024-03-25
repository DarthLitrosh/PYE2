# Cargar las librerías necesarias
library(e1071)
library(MASS)

# Definir la muestra de datos
Sample1 <- c(5.243546, 3.624798, 4.597245, 6.814677, 4.32791, 4.259092, 5.952399, 5.114901, 4.316348, 3.831358, 6.958219, 4.113977, 6.531121, 4.84677, 7.125954, 6.052483, 5.057894, 5.444031, 3.220108, 4.597651)
#PREGUNTA 1
# Calcular el valor mínimo
min(Sample1)
#PREGUNTA 2
# Calcular el primer cuartil (Q1)
quantile(Sample1, 0.25)
#PREGUNTA 3
# Calcular la mediana (Q2)
median(Sample1)
#PREGUNTA 4
#(q1114) Media de Sample1
# Calcular la media 
mean(Sample1)
#PREGUNTA 5
# Calcular el tercer cuartil (Q3)
quantile(Sample1, 0.75)
#PREGUNTA 6
# Calcular el valor máximo
max(Sample1)
#PREGUNTA 7
# Calcular el coeficiente de asimetría (skewness)
skewness(Sample1)
#PREGUNTA 8
# Calcular el coeficiente de apuntamiento (curtosis)
kurtosis(Sample1)
#PREGUNTA 9
# Ajustar los datos a una distribución normal
fit <- fitdistr(Sample1, "normal")
# Obtener el estimador de la media
fit$estimate["mean"]
#PREGUNTA 10
# Obtener el estimador de la desviación típica
fit$estimate["sd"]
#PREGUNTA 11
# Obtener los parámetros estimados de la distribución normal
mu <- fit$estimate["mean"]
sigma <- fit$estimate["sd"]
# Realizar el test de Kolmogorov-Smirnov
ks_result <- ks.test(Sample1, "pnorm", mean = mu, sd = sigma)
# Obtener el p-valor
ks_result$p.value
#PREGUNTA 12
# Ajustar los datos a una distribución exponencial
fit <- fitdistr(Sample1, "exponential")

# Obtener el estimador de la tasa lambda
fit$estimate["rate"]


# PREGUNTA 13
# Estimar el parámetro lambda
lambda_estimado <- 1/mean(Sample1)
# Realizar la prueba KS para una distribución exponencial
ks_resultado <- ks.test(Sample1, "pexp", lambda_estimado)
# Mostrar el valor 
ks_resultado$p.value

