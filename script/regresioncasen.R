# Instalar librerías si no las tienes
install.packages("car")
install.packages("broom")
install.packages("ggfortify")
install.packages("haven")
library(haven)
library(car)
library(broom)

# 1. Entrada
ruta_rds = "data/casen_rm.rds"
casen_rm = readRDS(ruta_rds)

# Histograma y Boxplot de ypc
hist(casen_rm$ytotcor)
boxplot(casen_rm$ytotcor)

# Calcular umbral del percentil 90
umbral <- quantile(casen_rm$ytotcor, 0.90, na.rm = TRUE)

# Filtrar los datos que están por debajo de ese umbral
casen_filtrada <- casen_rm[casen_rm$ytotcor <= umbral, ]

# Verificar cómo queda el histograma y boxplot
hist(casen_filtrada$ytotcor, main = "Ingreso total (<= P90)", col = "skyblue")
boxplot(casen_filtrada$ytotcor, main = "Boxplot sin extremos", col = "lightgreen")

# Convertir las variables categóricas a factores
casen_filtrada$sexo <- as_factor(casen_filtrada$sexo)
casen_filtrada$educ <- factor(casen_filtrada$educ)

# Resumen de las variables
summary(casen_filtrada$sexo)
summary(casen_filtrada$educ)
summary(casen_filtrada$edad)

# Modelo de regresión lineal
modelo <- lm(ytotcor ~ sexo + educ + edad, data = casen_filtrada)

# Mostrar el resumen del modelo
summary(modelo)

# Gráficos de diagnóstico del modelo
plot(modelo)

# Calcular el VIF
library(car)
vif(modelo)
anova(modelo)

# Obtener R² del modelo
r2 <- summary(modelo)$r.squared
print(paste("R² del modelo CASEN:", round(r2, 4)))
