# Instalar librerías si no las tienes
install.packages("car")
install.packages("broom")
install.packages("haven")
install.packages("here")
library(here)
library(haven)
library(car)
library(broom)

# -----------------------------------------
# 1. Cargar datos y limpiar ingreso
# -----------------------------------------

# Leer base CASEN
ruta_rds <- "data/casen_rm.rds"
casen_rm <- readRDS(ruta_rds)

# Histograma y Boxplot de ypc
hist(casen_rm$ypc,
     main = "Histograma ingreso per cápita (original)",
     xlab = "Ingreso per cápita", col = "skyblue", breaks = 50)

# Boxplot original
boxplot(casen_rm$ypc,
        main = "Boxplot ingreso per cápita (original)", col = "lightgreen")

# Eliminar NA y ceros en ypc
casen_limpio <- casen_rm[!is.na(casen_rm$ypc) & casen_rm$ypc > 0, ]

# -----------------------------------------
# 2. Análisis exploratorio inicial (antes de filtrar)
# -----------------------------------------

# Histograma y Boxplot de ypc original
hist(casen_limpio$ypc, main = "Histograma ingreso original", col = "skyblue", breaks = 50)
boxplot(casen_limpio$ypc, main = "Boxplot ingreso original", col = "lightgreen")

# -----------------------------------------
# 3. Filtrado por percentil 90
# -----------------------------------------

umbral <- quantile(casen_limpio$ypc, 0.90)
casen_filtrada <- casen_limpio[casen_limpio$ypc <= umbral, ]

# Histograma y Boxplot tras filtrado
hist(casen_filtrada$ypc, main = "Histograma ingreso filtrado (<= P90)", col = "tomato", breaks = 50)
boxplot(casen_filtrada$ypc, main = "Boxplot ingreso filtrado", col = "orange")

# -----------------------------------------
# 4. Conversión de variables categóricas
# -----------------------------------------

library(haven)
casen_filtrada$sexo <- haven::as_factor(casen_filtrada$sexo)
casen_filtrada$educ <- haven::as_factor(casen_filtrada$educ)
casen_filtrada$rama1 <- haven::as_factor(casen_filtrada$rama1)
# Revisar resumen de variables
summary(casen_filtrada$sexo)
summary(casen_filtrada$educ)
summary(casen_filtrada$edad)
summary(casen_filtrada$rama1)
# -----------------------------------------
# 5. Modelo de regresión lineal
# -----------------------------------------

modelo <- lm(ypc ~ sexo + educ + edad + rama1, data = casen_filtrada)
summary(modelo)
# -----------------------------------------
# 6. Diagnóstico del modelo
# -----------------------------------------

par(mfrow = c(2, 2))
plot(modelo)

# Se realiza el test VIF y ANOVA para determinar si existe multicolinealidad, y para conocer el P valor de cada variable, conociendo que tan explicativa es
library(car)
vif(modelo)
anova(modelo)

# Se obtiene el R² del modelo
r2 <- summary(modelo)$r.squared
print(paste("R² del modelo CASEN:", round(r2, 4)))

#se cargan las librerias correspondientes para realizar los ultimos graficos del estudio
library(broom)
library(ggplot2)

## GRAFICOS POR VARIABLE EXPLICATIVA
#Sexo y educacion
#Por ultimo, se realizaron graficos de las variables anteriormente mencionadas, para obtener una visualizacion clara de que tan importante es cada una por separado, en la prediccion del ingreso
#
# Ingreso por sexo
ggplot(casen_filtrada, aes(x = sexo, y = ypc)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribución del ingreso por sexo", x = "Sexo", y = "Ingreso per cápita")

#ingreso por nivel educativo
ggplot(casen_filtrada, aes(x = educ, y = ypc)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Distribución del ingreso por nivel educativo",
       x = "Nivel educativo", y = "Ingreso per cápita") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

