



library(e1071)
library(MASS)
Sample1 <- c(5.243546, 3.624798, 4.597245, 6.814677, 4.32791, 4.259092, 5.952399, 5.114901, 4.316348, 3.831358, 6.958219, 4.113977, 6.531121, 4.84677, 7.125954, 6.052483, 5.057894, 5.444031, 3.220108, 4.597651)
#sd(Sample1)
#PREGUNTA 21
# Calcular la desviación estándar de Sample1 por MLE suponiendo una distribución normal
# Calcular la desviación estándar de Sample1
sample_sd <- sd(Sample1)

# Mostrar el resultado
print(sample_sd)


Medias_Muestrales_30 <- c(186508, 17069.65, 36312.45, 25875.35, 163188.8, 115084.2, 146225.1, 12687.45, 62314.05, 90988, 14934.75, 17007.7, 97357.1, 33063, 18807.4, 12269.6, 22688.15, 16010.55, 74900.3, 20907.25, 22294.65, 221893.1, 90823.1, 21103.7, 153812.6, 101743.1, 11259.85, 43944.05, 32856.15, 20296.3)


 sd(Medias_Muestrales_30)
