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

#PREGUNTA 38

#PREGUNTA 39

#PREGUNTA 40

#PREGUNTA 41
# Función para calcular la media
meanFunc <- function(data, indices) {
  return(mean(data[indices]))
}

# Configurar el seed para reproducibilidad
set.seed(2023)

# Aplicar Bootstrap para calcular el IC de la media
bootResult <- boot(sample1, statistic = meanFunc, R = 1000)

# Calcular el intervalo de confianza del 95% usando el método normal
bootCI <- boot.ci(bootResult, conf = 0.95, type = "norm")

# Mostrar el límite inferior del IC
bootCI$normal[2]

#PREGUNTA 42

# Número de re-muestreos Bootstrap
bootR <- 1000

# Establecer semilla para reproducibilidad
set.seed(2023)

# Función para calcular la media
boot.mean <- function(data, indices) {
  mean(data[indices])
}

# Realizar el Bootstrap
boot.results <- boot(data=Sample1, statistic=boot.mean, R=bootR)

# Calcular el intervalo de confianza del 95% para la media con el método percentil
boot.ci(boot.results, conf=0.95, type="perc")

#PREGUNTA 43
meanFunc <- function(data, indices) {
  return(mean(data[indices]))
}
# Configurar el seed para reproducibilidad
set.seed(2023)

# Aplicar el método Bootstrap para estimar la media de sample1
bootResult <- boot(sample1, statistic = meanFunc, R = 1000)

# La media estimada se puede obtener directamente de la estadística original del resultado bootstrap
meanEstimate <- bootResult$t0

# Mostrar la media estimada
meanEstimate


#PREGUNTA 44
# Definir la función para calcular la varianza
varFunc <- function(data, indices) {
  return(var(data[indices]))
}

# Configurar el seed para reproducibilidad
set.seed(2023)

# Asegúrate de tener el conjunto de datos 'sample1' definido antes de continuar

# Aplicar el método Bootstrap para calcular la distribución de la varianza muestral
bootResultVar <- boot(sample1, statistic = varFunc, R = 1000)

# Calcular el intervalo de confianza del 95% usando el método normal
bootCIVar <- boot.ci(bootResultVar, conf = 0.95, type = "norm")

# Mostrar el límite inferior del intervalo de confianza del 95% para la varianza
bootCIVar$normal[2]


#PREGUNTA 45
# Cargar la librería necesaria
library(boot)

# Definir la función para calcular la varianza de los datos seleccionados
varFunc <- function(data, indices) {
  return(var(data[indices]))
}

# Configurar el seed para reproducibilidad
set.seed(2023)

# Aplicar el método Bootstrap para calcular la distribución de la varianza muestral
bootResultVar <- boot(sample1, statistic = varFunc, R = 1000)

# Calcular el intervalo de confianza del 95% usando el método BCA
bootCIVar <- boot.ci(bootResultVar, conf = 0.95, type = "bca")

# Mostrar el límite superior del intervalo de confianza del 95% para la varianza utilizando 'bca'
bootCIVar$bca[5]


#PREGUNTA 46
# Número de réplicas Bootstrap
bootR <- 1000

# Establece una semilla para reproducibilidad
set.seed(2023)

# Inicializa un vector para almacenar las varianzas de las muestras bootstrap
bootstrap_variances <- numeric(bootR)

# Bucle para generar muestras bootstrap y calcular su varianza
for (i in 1:bootR) {
  # Muestra con reemplazo
  sample_boot <- sample(Sample1, size = length(Sample1), replace = TRUE)
  
  # Calcula la varianza de la muestra bootstrap y la almacena
  bootstrap_variances[i] <- var(sample_boot)
}

# Calcula la varianza estimada de las varianzas bootstrap
estimated_variance <- mean(bootstrap_variances)

# Imprime la varianza estimada
print(estimated_variance)


#PREGUNTA 47
# Configurar el seed para reproducibilidad
set.seed(2023)

# Asegúrate de que 'Sample2' está definido y es apropiado para tu análisis

# Aplicar el método Bootstrap para estimar la proporción de "Municipal"
bootResultProp <- boot(Sample2, statistic = propMunicipalFunc, R = 1000)

# Calcular el intervalo de confianza del 95% usando el método normal (norm)
bootCIPropNorm <- boot.ci(bootResultProp, conf = 0.95, type = "norm")

# Mostrar el límite inferior del intervalo de confianza del 95% con 'norm'
limite_inferior_norm <- bootCIPropNorm$normal[2]

limite_inferior_norm


#PREGUNTA 48
# Configurar el seed para reproducibilidad
set.seed(2023)

# Asegúrate de que 'Sample2' está definido y es apropiado para tu análisis

# Aplicar el método Bootstrap para estimar la proporción de "Municipal"
bootResultProp <- boot(Sample2, statistic = propMunicipalFunc, R = 1000)

# Calcular el intervalo de confianza del 95% usando el método percentil (perc)
bootCIPropPerc <- boot.ci(bootResultProp, conf = 0.95, type = "perc")

# Mostrar el valor superior del intervalo de confianza del 95% con 'perc'
valor_superior_perc <- bootCIPropPerc$percent[5]

valor_superior_perc


#PREGUNTA 49
# Convertir a un vector binario donde 1 representa "Municipal" y 0 cualquier otra categoría
Sample2_binary <- as.numeric(Sample2 == "Municipal")

# Número de re-muestreos Bootstrap
bootR <- 1000

# Establecer semilla para reproducibilidad
set.seed(2023)

# Función para calcular la proporción de "Municipal"
boot.prop <- function(data, indices) {
  sum(data[indices]) / length(indices)
}

# Realizar el Bootstrap
boot.results <- boot(data=Sample2_binary, statistic=boot.prop, R=bootR)

# Estimar la proporción
proporcion_estimada <- mean(boot.results$t)

# Mostrar la proporción estimada
proporcion_estimada

#PREGUNTA 50
# Convertimos Sample2 en un vector lógico indicando si cada elemento es "Municipal"
is_municipal <- Sample2 == "Municipal"

# Separamos Sample1 en dos grupos basados en la clasificación de Municipal vs. No Municipal
sample1_municipal <- Sample1[is_municipal]
sample1_no_municipal <- Sample1[!is_municipal]

# Realizamos la prueba t de Welch para la diferencia de medias (var.equal = FALSE por defecto)
t_test_result <- t.test(sample1_municipal, sample1_no_municipal)

# Extraemos el valor inferior del intervalo de confianza del 95%
lower_bound <- t_test_result$conf.int[1]

# Imprimimos el resultado
print(lower_bound)

#PREGUNTA 51
# Convertir Sample2 en un factor para facilitar el análisis
Sample2 <- factor(Sample2)

# Crear un data.frame para facilitar el análisis
data <- data.frame(Sample1, Sample2)

# Dividir Sample1 en dos grupos: Municipal vs No Municipal
municipal_values <- data$Sample1[data$Sample2 == "Municipal"]
no_municipal_values <- data$Sample1[data$Sample2 != "Municipal"]

# Realizar el Test t para muestras independientes
t_test_result <- t.test(municipal_values, no_municipal_values, var.equal = FALSE)

# Extraer el valor superior del intervalo de confianza del 95%
upper_ci_value <- t_test_result$conf.int[2]

# Imprimir el resultado
print(upper_ci_value)


#PREGUNTA 52
# Convertimos Sample2 en un vector lógico indicando si cada elemento es "Municipal"
is_municipal <- Sample2 == "Municipal"

# Separamos Sample1 en dos grupos basados en la clasificación de Municipal vs. No Municipal
sample1_municipal <- Sample1[is_municipal]
sample1_no_municipal <- Sample1[!is_municipal]

# Realizamos la prueba t de Welch para la diferencia de medias
t_test_result <- t.test(sample1_municipal, sample1_no_municipal)

# Extraemos el p-valor de la prueba t
p_value <- t_test_result$p.value

# Imprimimos el p-valor
print(p_value)


#PREGUNTA 53
# Convertir Sample2 en un factor para facilitar el análisis
Sample2 <- factor(Sample2)

# Crear un data.frame para facilitar el análisis
data <- data.frame(Sample1, Sample2)

# Dividir Sample1 en dos grupos: Municipal vs No Municipal
municipal_values <- data$Sample1[data$Sample2 == "Municipal"]
no_municipal_values <- data$Sample1[data$Sample2 != "Municipal"]

# Realizar el Test t para muestras independientes
t_test_result <- t.test(municipal_values, no_municipal_values, var.equal = FALSE)

# Extraer la diferencia de medias estimada
mean_difference <- t_test_result$estimate[1] - t_test_result$estimate[2]

# Imprimir la diferencia de medias estimada
print(mean_difference)
