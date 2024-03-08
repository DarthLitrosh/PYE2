# Cargar las librerías necesarias
library(e1071)
library(MASS)

# Definir la muestra de datos
Sample1 <- c(5.243546, 3.624798, 4.597245, 6.814677, 4.32791, 4.259092, 5.952399, 5.114901, 4.316348, 3.831358, 6.958219, 4.113977, 6.531121, 4.84677, 7.125954, 6.052483, 5.057894, 5.444031, 3.220108, 4.597651)
#PREGUNTA 1
# Calcular estadísticas descriptivas
mean(Sample1)  # Media
sd(Sample1)    # Desviación estándar
skewness(Sample1) # Asimetría
kurtosis(Sample1) # Curtosis
# Ajustar un modelo de regresión lineal
lm_result <- lm(Sample2 ~ Sample1)

# Ver los resultados del modelo
summary(lm_result)
# Calcular el valor mínimo
min(Sample1)


#PREGUNTA 2

