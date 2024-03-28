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

# Definir la muestra de datos
Sample1 <- c(5.243546, 3.624798, 4.597245, 6.814677, 4.32791, 4.259092, 5.952399, 5.114901, 4.316348, 3.831358, 6.958219, 4.113977, 6.531121, 4.84677, 7.125954, 6.052483, 5.057894, 5.444031, 3.220108, 4.597651)
Sample2 <- c("Municipal", "Municipal", "Municipal", "Political Subdivision", "Municipal", "Municipal", "Municipal", "Municipal", "Behind the Meter", "Municipal", "Municipal", "Municipal", "Retail Power Marketer", "Municipal", "Investor Owned", "Cooperative", "Cooperative", "Municipal", "Municipal", "Municipal")
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
curtosis_result <- kurtosis(Sample1)
print(curtosis_result)
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




#otra parte
# PREGUNTA 14
# Definir el vector de medias muestrales
Medias_Muestrales_30 <- c(186508, 17069.65, 36312.45, 25875.35, 163188.8, 115084.2, 146225.1, 12687.45, 62314.05, 90988, 14934.75, 17007.7, 97357.1, 33063, 18807.4, 12269.6, 22688.15, 16010.55, 74900.3, 20907.25, 22294.65, 221893.1, 90823.1, 21103.7, 153812.6, 101743.1, 11259.85, 43944.05, 32856.15, 20296.3)

# Calcular la media de las medias muestrales
estimador_media <- mean(Medias_Muestrales_30)

# Mostrar el resultado
print(estimador_media)
# PREGUNTA 15
# Calcular la desviación estándar de las medias muestrales
estimador_desviacion <- sd(Medias_Muestrales_30)

# Mostrar el resultado
print(estimador_desviacion)
# PREGUNTA 16
# Realizar el test de Kolmogorov-Smirnov para evaluar el ajuste a una distribución normal
ks_result <- ks.test(Medias_Muestrales_30, "pnorm", mean = mean(Medias_Muestrales_30), sd = sd(Medias_Muestrales_30))

# Obtener el p-valor
p_valor <- ks_result$p.value

# Mostrar el resultado
print(p_valor)

#PREGUNTA 17
# Definir el vector de proporciones muestrales
Proporciones_Muestrales_30 <- c(0.05, 0, 0.25, 0.05, 0, 0.1, 0, 0.1, 0.2, 0.1,
                                0.15, 0.05, 0.05, 0.1, 0.05, 0.1, 0, 0.05, 0.2, 0,
                                0.05, 0.05, 0.05, 0.1, 0.1, 0.2, 0.1, 0, 0.05, 0)

# Calcular la media de las proporciones muestrales
mean(Proporciones_Muestrales_30)

#PREGUNTA 18
sd(Proporciones_Muestrales_30)

#Pregunta 19
ks_result <- ks.test(Proporciones_Muestrales_30, "pnorm", mean = mean(Proporciones_Muestrales_30), sd = sd(Proporciones_Muestrales_30))

# Obtener el p-valor
p2_valor <- ks_result$p.value

# Mostrar el resultado
print(p2_valor)
#PREGUNTA 20
# Calcular la media de Sample1 por MLE suponiendo una distribución normal
mle_mean <- mean(Sample1)

# Mostrar el resultado
print(mle_mean)

#PREGUNTA 21
# Calcular la desviación estándar de Sample1 por MLE suponiendo una distribución normal
# Calcular la desviación estándar de Sample1
sample_sd <- sd(Sample1)

# Mostrar el resultado
print(sample_sd)

#PREGUNTA 22
# Filtrar Sample1 para incluir solo los valores correspondientes a Utility.Type == Municipal
Sample1_municipal <- Sample1[Utility.Type == "Municipal"]

# Calcular la media de Sample1 condicionado a Utility.Type == Municipal
mle_mean_municipal <- mean(Sample1_municipal)

# Mostrar el resultado
print(mle_mean_municipal)

# PREGUNTA 23
# Filtrar Sample1 para incluir solo los valores correspondientes a Utility.Type == Municipal
Sample1_municipal <- Sample1[Sample2 == "Municipal"]

# Calcular la desviación típica de Sample1 condicionado a Utility.Type == Municipal
sd_mle_municipal <- sd(Sample1_municipal)

# Mostrar el resultado
print(sd_mle_municipal)

#PREGUNTA 24
# Filtrar Sample1 para incluir solo los valores correspondientes a Utility.Type != Municipal
Sample1_no_municipal <- Sample1[Sample2 != "Municipal"]

# Calcular la media de Sample1 condicionado a Utility.Type != Municipal
mean_mle_no_municipal <- mean(Sample1_no_municipal)

# Mostrar el resultado
print(mean_mle_no_municipal)

#PREGUNTA 25
# Filtrar Sample1 para incluir solo los valores correspondientes a Utility.Type != Municipal
Sample1_no_municipal <- Sample1[Sample2 != "Municipal"]

# Calcular la desviación típica de Sample1 condicionado a Utility.Type != Municipal
sd_mle_no_municipal <- sd(Sample1_no_municipal)

# Mostrar el resultado
print(sd_mle_no_municipal)

#PREGUNTA 26
# Definir la desviación estándar conocida
desviacion_conocida <- 1.149346 # sd(Sample1)=1.149346 

# Calcular el tamaño de la muestra
n <- length(Sample1)

# Calcular el valor crítico z para un nivel de confianza del 95%
z <- qnorm(0.975)

# Calcular el límite inferior del IC.95
limite_inferior_IC95 <- mean(Sample1) - z * (desviacion_conocida / sqrt(n))

# Mostrar el resultado
print(limite_inferior_IC95)

#PREGUNTA 27

# Definir la desviación estándar conocida
desviacion_conocida <- 1.149346 # Aquí ingresa el valor de la desviación estándar conocida

# Calcular el tamaño de la muestra
n <- length(Sample1)

# Calcular el valor crítico z para un nivel de confianza del 95%
z <- qnorm(0.975)

# Calcular el límite superior del IC.95
limite_superior_IC95 <- mean(Sample1) + z * (desviacion_conocida / sqrt(n))

# Mostrar el resultado
print(limite_superior_IC95)

#PREGUNTA 28
# Definir la media y la desviación estándar conocida
media <- mean(Sample1)
desviacion_estandar_conocida <- 1.149346  # Aquí ingresa el valor de la desviación estándar conocida

# Definir el tamaño de la muestra
n <- length(Sample1)

# Definir el valor z
valor_z <- (media - media_poblacional) / (desviacion_estandar_conocida / sqrt(n))

# Calcular el valor p
valor_p <- 2 * (1 - pnorm(abs(valor_z)))

# Mostrar el resultado
print(valor_p)

#PREGUNTA 29
# Definir la media estimada
media_estimada <- mean(Sample1)

# Mostrar el resultado
print(media_estimada)


#PREGUNTA 30
# Realizar un t-test para obtener el intervalo de confianza de la media
resultado_t_test <- t.test(Sample1)

# Extraer el límite inferior del intervalo de confianza del 95%
limite_inferior_ic <- resultado_t_test$conf.int[1]

# Imprimir el resultado
print(limite_inferior_ic)


#PREGUNTA 31
# Realizar un t-test para obtener el intervalo de confianza de la media
resultado_t_test <- t.test(Sample1)

# Extraer el límite superior del intervalo de confianza del 95%
limite_superior_ic <- resultado_t_test$conf.int[2]

# Imprimir el resultado
print(limite_superior_ic)


#PREGUNTA 32
# Realizar un z-test asumiendo que la desviación estándar de la muestra es una estimación de la población
z_test_result <- z.test(Sample1, stdev = sd(Sample1))

# Obtener el p-valor del z-test
p_valor <- z_test_result$p.value

# Imprimir el p-valor
print(p_valor)

#PREGUNTA 33
# Calcular la media muestral
media_estimada <- mean(Sample1)

# Imprimir la media estimada
print(media_estimada)


#PREGUNTA 34
# Tamaño de la muestra
n <- length(Sample1)

# Varianza muestral
s2 <- var(Sample1)

# Nivel de significancia
alpha <- 0.05

# Cuantiles de la distribución chi-cuadrada para los límites del IC
chi2_lower <- qchisq(alpha / 2, df = n - 1, lower.tail = TRUE)
chi2_upper <- qchisq(1 - alpha / 2, df = n - 1)

# Límite inferior del intervalo de confianza para la varianza
variance_lower_bound <- ((n - 1) * s2) / chi2_upper

# Imprimir el límite inferior del intervalo de confianza para la varianza
print(variance_lower_bound)


#PREGUNTA 35
# Tamaño de la muestra
n <- length(Sample1)

# Varianza muestral
s2 <- var(Sample1)

# Nivel de significancia
alpha <- 0.05

# Cuantiles de la distribución chi-cuadrada para los límites del IC
chi2_lower <- qchisq(alpha / 2, df = n - 1)
chi2_upper <- qchisq(1 - alpha / 2, df = n - 1, lower.tail = TRUE)

# Límite superior del intervalo de confianza para la varianza
variance_upper_bound <- ((n - 1) * s2) / chi2_lower

# Imprimir el límite superior del intervalo de confianza para la varianza
print(variance_upper_bound)


#PREGUNTA 36
# Calcular la varianza muestral
varianza_estimada <- var(Sample1)

# Imprimir la varianza estimada
print(varianza_estimada)


#PREGUNTA 37
