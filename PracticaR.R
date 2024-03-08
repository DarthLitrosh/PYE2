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

