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
#PREGUNTA 25 
#PREGUNTA 26 
# P-value de la prueba de Pearson
cat("Pregunta 26: P-value = ", pearson_result$p.value, "\n")

#PREGUNTA 27 
#PREGUNTA 28 
#PREGUNTA 29 
#PREGUNTA 30 
#PREGUNTA 31 
