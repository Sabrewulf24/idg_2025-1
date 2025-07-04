# --- CARGA DE LIBRERÍAS ---
library(haven)
library(ggplot2)
library(pROC)
library(mgcv)

# --- CARGA DE DATOS ---
personas <- read_dta("data/datos_epf/base-personas-ix-epf-stata.dta")
gastos   <- read_dta("data/datos_epf/base-gastos-ix-epf-stata.dta")
cantidades <- read_dta("data/datos_epf/base-cantidades-ix-epf-stata.dta")
ccif     <- read_dta("data/datos_epf/ccif-ix-epf-stata.dta")

# --- FILTRO GRAN SANTIAGO ---
personas_gs = subset(personas, macrozona == 2 & sprincipal == 1)
valores_invalidos <- c(-99, -88, -77)
personas_gs = subset(personas_gs, !(edad %in% valores_invalidos) &
                       !(edue %in% valores_invalidos) &
                       ing_disp_hog_hd_ai >= 0)
personas_gs$ing_pc = personas_gs$ing_disp_hog_hd_ai / personas_gs$npersonas

# --- FILTRAR GASTOS DE GIMNASIO ---
gastos_servicio = subset(gastos, ccif == "09.4.6.02.04" & macrozona == 2)
gasto_hogar_servicio = merge(gastos_servicio, personas_gs, by = "folio")
tabla_gastos = gasto_hogar_servicio[, c("sexo", "edad", "edue", "fe.x", "cse", "ing_pc", "gasto")]

# --- GRAFICOS EXPLORATORIOS ---
hist(tabla_gastos$ing_pc, breaks = 30, col = "lightblue", main = "Distribución del Ingreso", xlab = "Ingreso per cápita")
hist(tabla_gastos$gasto, breaks = 30, col = "lightblue", main = "Distribución del Gasto en Gimnasio", xlab = "Gasto en gimnasio")

boxplot(gasto ~ factor(sexo), data = tabla_gastos, main = "Gasto en Gimnasio según Sexo", xlab = "Sexo", col = c("tomato", "lightgreen"))

plot(tabla_gastos$edad, tabla_gastos$gasto, main = "Edad vs Gasto", xlab = "Edad", ylab = "Gasto", pch = 20, col = rgb(0,0,0,0.3))
lines(lowess(tabla_gastos$edad, tabla_gastos$gasto), col = "red", lwd = 2)

plot(tabla_gastos$ing_pc, tabla_gastos$gasto, main = "Ingreso vs Gasto", xlab = "Ingreso per cápita", ylab = "Gasto", pch = 20, col = rgb(0,0,0,0.3))
lines(lowess(tabla_gastos$ing_pc, tabla_gastos$gasto), col = "blue", lwd = 2)

# Escolaridad agrupada
tabla_gastos$grupo_escolaridad <- cut(tabla_gastos$edue, breaks = c(-Inf, 8, 12, 16, Inf), labels = c("Básica o menos", "Media-baja", "Media-alta", "Alta"), right = TRUE)
boxplot(gasto ~ grupo_escolaridad, data = tabla_gastos, main = "Gasto según Escolaridad", xlab = "Escolaridad", col = "skyblue")

# Agrupación edad
tabla_gastos$grupo_edad <- cut(tabla_gastos$edad, breaks = c(0, 29, 39, 49, 59, 69, 120), labels = c("0–29", "30–39", "40–49", "50–59", "60–69", "70+"), right = TRUE, include.lowest = TRUE)