# --- LIBRERÍAS ---
library(haven)
library(ggplot2)
library(pROC)

# --- CARGA EPF ---
personas <- read_dta("data/datos_epf/base-personas-ix-epf-stata.dta")
gastos   <- read_dta("data/datos_epf/base-gastos-ix-epf-stata.dta")

# --- FILTRO GRAN SANTIAGO Y VARIABLES VÁLIDAS ---
personas_gs <- subset(personas, macrozona == 2 & sprincipal == 1)
personas_gs <- subset(personas_gs, !(edad %in% c(-99, -88, -77)) & !(edue %in% c(-99, -88, -77)))
personas_gs$ing_pc <- personas_gs$ing_disp_hog_hd_ai / personas_gs$npersonas

# --- FILTRAR GASTOS DE GIMNASIO ---
gastos_gym <- subset(gastos, ccif == "09.4.6.02.04" & macrozona == 2)

# 1. Agregar gasto total por persona (folio)
gastos_gym_total <- aggregate(gasto ~ folio, data = gastos_gym, sum)

# 2. Asegurar que 'folio' sea caracter para merge
personas_gs$folio <- as.character(personas_gs$folio)
gastos_gym_total$folio <- as.character(gastos_gym_total$folio)

# 3. Merge para obtener gasto gym por persona con info personas
gym_data <- merge(personas_gs, gastos_gym_total, by = "folio", all.x = TRUE)

# 4. Reemplazar NA en gasto por 0 (personas que no reportan gasto en gym)
gym_data$gasto[is.na(gym_data$gasto)] <- 0

# 5. Crear variable binaria de gasto (0 = no gasta, 1 = gasta)
gym_data$gasta_bin <- ifelse(gym_data$gasto > 0, 1, 0)
gym_data$gasta_bin_f <- factor(gym_data$gasta_bin, levels = c(0,1))

# 6. Convertir variables a tipos correctos para modelar
gym_data$sexo <- factor(gym_data$sexo)  # Ahora como factor
gym_data$edue <- as.numeric(gym_data$edue)
gym_data$edad <- as.numeric(gym_data$edad)
gym_data$ing_pc <- as.numeric(gym_data$ing_pc)

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




# --- MODELO LOGIT (¿Gasta o no?) ---
modelo_logit <- glm(gasta_bin ~ edad + edue + ing_pc + sexo, 
                    family = "binomial", data = gym_data)
summary(modelo_logit)

# --- MODELO LINEAL (¿Cuánto gasta?) ---
gym_data_gasta <- subset(gym_data, gasto > 0)
modelo_lineal <- lm(gasto ~ edad + edue + ing_pc + sexo, data = gym_data_gasta)
summary(modelo_lineal)


# --- CURVA ROC ---
roc_curve <- roc(gym_data$gasta_bin_f, predict(modelo_logit, type = "response"))
plot(roc_curve, main = "Curva ROC - Logit")
auc(roc_curve)

# --- CARGA DE CASEN ---
casen <- readRDS("data/casen_rm.rds")  # Asegúrate que el path es correcto

# --- LIMPIEZA Y CREACIÓN VARIABLES EN CASEN ---
casen <- subset(casen, !(edad %in% c(-88, -99, -77)) & !(esc %in% c(-88, -99, -77)))

casen$ing_pc <- casen$ytotcorh / casen$numper
casen$edad <- as.numeric(casen$edad)
casen$edue <- as.numeric(casen$esc)
casen$sexo <- factor(casen$sexo, levels = levels(gym_data$sexo))  # Mismo factor que en gym_data

# --- PREDICCIÓN PROBABILIDAD DE GASTO ---
casen$prob_gym <- predict(modelo_logit, newdata = casen, type = "response")

# --- PREDICCIÓN MONTO ESTIMADO DE GASTO ---
casen$gasto_estimado <- predict(modelo_lineal, newdata = casen)

# --- IMPUTACIÓN FINAL DE GASTO ---
umbral <- 0.65
casen$gasto_imputado <- ifelse(casen$prob_gym >= umbral, casen$gasto_estimado, 0)

# --- RESULTADOS ---
hist(casen$gasto_imputado, breaks = 30, col = "skyblue",
     main = "Gasto imputado en gimnasio", xlab = "Monto estimado")
summary(casen$gasto_imputado)
