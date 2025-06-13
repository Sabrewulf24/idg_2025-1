install.packages("rakeR")
install.packages("RPostgres")
install.packages("DBI")
install.packages("ggplot2")
install.packages("sf")
install.packages("biscale")
install.packages("here")


# LIBRERÍAS
library(rakeR)
library(RPostgres)
library(DBI)
library(ggplot2)
library(sf)
library(here)



# ENTRADAS
cons_censo_df <- readRDS("data/cons_censo_df.rds")
casen_raw <- readRDS("data/casen_rm.rds")

# VARIABLES BASE
col_cons <- sort(setdiff(names(cons_censo_df), c("GEOCODIGO", "COMUNA")))
age_levels  <- grep("^edad", col_cons, value = TRUE)
esc_levels  <- grep("^esco", col_cons, value = TRUE)
sexo_levels <- grep("^sexo_", col_cons, value = TRUE)

vars_base <- c("estrato", "esc", "edad", "sexo", "e6a", "s22a_preg")

casen <- casen_raw[, vars_base, drop = FALSE]
rm(casen_raw)

# PREPROCESAMIENTO
casen$Comuna <- substr(as.character(casen$estrato), 1, 5)
casen$estrato <- NULL

casen$esc <- as.integer(unclass(casen$esc))
casen$edad <- as.integer(unclass(casen$edad))
casen$e6a <- as.numeric(unclass(casen$e6a))
casen$sexo <- as.integer(unclass(casen$sexo))
casen$s22a_preg <- as.integer(unclass(casen$s22a_preg))

# Recode s22a_preg: 1 = sí recibió atención mental, 0 = no
casen$salud_mental <- ifelse(casen$s22a_preg == 1, 1,
                             ifelse(casen$s22a_preg == 2, 0, NA))

# IMPUTACIÓN DE ESCOLARIDAD
idx_na <- which(is.na(casen$esc))
fit <- lm(esc ~ e6a, data = casen[-idx_na, ])
pred <- predict(fit, newdata = casen[idx_na, , drop = FALSE])
casen$esc[idx_na] <- as.integer(round(pmax(0, pmin(29, pred))))

casen$ID <- as.character(seq_len(nrow(casen)))

# RECODIFICACIÓN PARA RAKE
casen$edad_cat <- cut(
  casen$edad,
  breaks = c(0, 30, 40, 50, 60, 70, 80, Inf),
  labels = age_levels,
  right = FALSE, include.lowest = TRUE
)

casen$esc_cat <- factor(
  with(casen,
       ifelse(esc == 0, esc_levels[1],
              ifelse(esc <= 8, esc_levels[2],
                     ifelse(esc <= 12, esc_levels[3], esc_levels[4])))),
  levels = esc_levels
)

casen$sexo_cat <- factor(
  ifelse(casen$sexo == 2, sexo_levels[1],
         ifelse(casen$sexo == 1, sexo_levels[2], NA)),
  levels = sexo_levels
)

# MICROSIMULACIÓN
cons_censo_comunas <- split(cons_censo_df, cons_censo_df$COMUNA)
inds_list <- split(casen, casen$Comuna)

sim_list <- lapply(names(cons_censo_comunas), function(zona) {
  cons_i <- cons_censo_comunas[[zona]]
  col_order <- sort(setdiff(names(cons_i), c("COMUNA", "GEOCODIGO")))
  cons_i <- cons_i[, c("GEOCODIGO", col_order), drop = FALSE]
  
  tmp <- inds_list[[zona]]
  inds_i <- tmp[, c("ID", "edad_cat", "esc_cat", "sexo_cat"), drop = FALSE]
  names(inds_i) <- c("ID", "Edad", "Escolaridad", "Sexo")
  
  w_frac <- weight(cons = cons_i, inds = inds_i,
                   vars = c("Edad", "Escolaridad", "Sexo"))
  sim_i <- integerise(weights = w_frac, inds = inds_i, seed = 123)
  merge(sim_i, tmp[, c("ID", "salud_mental")], by = "ID", all.x = TRUE)
})

sim_df <- data.table::rbindlist(sim_list, idcol = "COMUNA")

# CÁLCULO DE % QUE RECIBIÓ ATENCIÓN DE SALUD MENTAL
zonas_saludmental <- aggregate(
  salud_mental ~ zone,
  data = sim_df,
  FUN = function(x) {
    total <- sum(!is.na(x))
    con_atencion <- sum(x == 1, na.rm = TRUE)
    round(100 * con_atencion / total, 2)
  }
)

names(zonas_saludmental) <- c("geocodigo", "atencion_salud_mental")

# EXPORTACIÓN A POSTGRESQL
con <- dbConnect(
  Postgres(),
  dbname = "censo_rm_2017",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "postgres"
)

dbWriteTable(
  conn = con,
  name = Id(schema = "dpa", table = "tmp_saludmental_rm"),
  value = zonas_saludmental,
  overwrite = TRUE,
  row.names = FALSE
)

dbExecute(con, "CREATE INDEX ON dpa.tmp_saludmental_rm(geocodigo)")
dbExecute(con, "ANALYZE dpa.tmp_saludmental_rm")
dbExecute(con, "DROP TABLE IF EXISTS dpa.zonas_saludmental")

dbExecute(con, "
  CREATE TABLE dpa.zonas_saludmental AS
  SELECT
    z.*,
    t.atencion_salud_mental
  FROM dpa.zonas_censales_rm AS z
  LEFT JOIN dpa.tmp_saludmental_rm AS t
    ON z.geocodigo::text = t.geocodigo
  WHERE urbano = 1 AND (nom_provin = 'SANTIAGO' OR nom_comuna = 'SAN BERNARDO' OR nom_comuna = 'PUENTE ALTO')
")

zonas_saludmental_sf <- st_read(con, query = "
  SELECT * FROM dpa.zonas_saludmental
")

# VISUALIZACIÓN
ggplot(zonas_saludmental_sf) +
  geom_sf(aes(fill = atencion_salud_mental), color = "black", size = 0.2) +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", na.value = "grey90",
                      name = "Atención salud mental (%)") +
  theme_minimal() +
  labs(title = "Acceso a atención de salud mental en los últimos 3 meses",
       subtitle = "Porcentaje de personas con atención reciente por zona censal") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())





# LIBRERÍAS
library(rakeR)
library(RPostgres)
library(DBI)
library(ggplot2)
library(sf)

# ENTRADAS
cons_censo_df <- readRDS("data/cons_censo_df.rds")
casen_raw <- readRDS("data/casen_rm.rds")

# VARIABLES BASE
col_cons <- sort(setdiff(names(cons_censo_df), c("GEOCODIGO", "COMUNA")))
age_levels  <- grep("^edad", col_cons, value = TRUE)
esc_levels  <- grep("^esco", col_cons, value = TRUE)
sexo_levels <- grep("^sexo_", col_cons, value = TRUE)

vars_base <- c("estrato", "esc", "edad", "sexo", "e6a", "s22a_preg", "ind_hacina")

casen <- casen_raw[, vars_base, drop = FALSE]
rm(casen_raw)

# PREPROCESAMIENTO
casen$Comuna <- substr(as.character(casen$estrato), 1, 5)
casen$estrato <- NULL

casen$esc <- as.integer(unclass(casen$esc))
casen$edad <- as.integer(unclass(casen$edad))
casen$e6a <- as.numeric(unclass(casen$e6a))
casen$sexo <- as.integer(unclass(casen$sexo))
casen$s22a_preg <- as.integer(unclass(casen$s22a_preg))
casen$ind_hacina <- as.integer(unclass(casen$ind_hacina))

# Recode salud mental (para otros análisis)
casen$salud_mental <- ifelse(casen$s22a_preg == 1, 1,
                             ifelse(casen$s22a_preg == 2, 0, NA))

# Recode hacinamiento: 1 si ind_hacina >= 2 (medio o alto), 0 en caso contrario
casen$hacinamiento <- ifelse(casen$ind_hacina >= 2, 1, 0)

# IMPUTACIÓN DE ESCOLARIDAD
idx_na <- which(is.na(casen$esc))
fit <- lm(esc ~ e6a, data = casen[-idx_na, ])
pred <- predict(fit, newdata = casen[idx_na, , drop = FALSE])
casen$esc[idx_na] <- as.integer(round(pmax(0, pmin(29, pred))))

casen$ID <- as.character(seq_len(nrow(casen)))

# RECODIFICACIÓN PARA RAKE
casen$edad_cat <- cut(
  casen$edad,
  breaks = c(0, 30, 40, 50, 60, 70, 80, Inf),
  labels = age_levels,
  right = FALSE, include.lowest = TRUE
)

casen$esc_cat <- factor(
  with(casen,
       ifelse(esc == 0, esc_levels[1],
              ifelse(esc <= 8, esc_levels[2],
                     ifelse(esc <= 12, esc_levels[3], esc_levels[4])))),
  levels = esc_levels
)

casen$sexo_cat <- factor(
  ifelse(casen$sexo == 2, sexo_levels[1],
         ifelse(casen$sexo == 1, sexo_levels[2], NA)),
  levels = sexo_levels
)

# MICROSIMULACIÓN
cons_censo_comunas <- split(cons_censo_df, cons_censo_df$COMUNA)
inds_list <- split(casen, casen$Comuna)

sim_list <- lapply(names(cons_censo_comunas), function(zona) {
  cons_i <- cons_censo_comunas[[zona]]
  col_order <- sort(setdiff(names(cons_i), c("COMUNA", "GEOCODIGO")))
  cons_i <- cons_i[, c("GEOCODIGO", col_order), drop = FALSE]
  
  tmp <- inds_list[[zona]]
  inds_i <- tmp[, c("ID", "edad_cat", "esc_cat", "sexo_cat"), drop = FALSE]
  names(inds_i) <- c("ID", "Edad", "Escolaridad", "Sexo")
  
  w_frac <- weight(cons = cons_i, inds = inds_i,
                   vars = c("Edad", "Escolaridad", "Sexo"))
  sim_i <- integerise(weights = w_frac, inds = inds_i, seed = 123)
  
  merge(sim_i, tmp[, c("ID", "hacinamiento")], by = "ID", all.x = TRUE)
})

sim_df <- data.table::rbindlist(sim_list, idcol = "COMUNA")

# CÁLCULO DE % EN HACINAMIENTO POR ZONA
zonas_hacinamiento <- aggregate(
  hacinamiento ~ zone,
  data = sim_df,
  FUN = function(x) {
    total <- sum(!is.na(x))
    con_hacinamiento <- sum(x == 1, na.rm = TRUE)
    round(100 * con_hacinamiento / total, 2)
  }
)

names(zonas_hacinamiento) <- c("geocodigo", "hacinamiento_pct")

# EXPORTACIÓN A POSTGRESQL
con <- dbConnect(
  Postgres(),
  dbname = "censo_rm_2017",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "postgres"
)

dbWriteTable(
  conn = con,
  name = Id(schema = "dpa", table = "tmp_hacinamiento_rm"),
  value = zonas_hacinamiento,
  overwrite = TRUE,
  row.names = FALSE
)

dbExecute(con, "CREATE INDEX ON dpa.tmp_hacinamiento_rm(geocodigo)")
dbExecute(con, "ANALYZE dpa.tmp_hacinamiento_rm")
dbExecute(con, "DROP TABLE IF EXISTS dpa.zonas_hacinamiento")

dbExecute(con, "
  CREATE TABLE dpa.zonas_hacinamiento AS
  SELECT
    z.*,
    t.hacinamiento_pct
  FROM dpa.zonas_censales_rm AS z
  LEFT JOIN dpa.tmp_hacinamiento_rm AS t
    ON z.geocodigo::text = t.geocodigo
  WHERE urbano = 1 AND (nom_provin = 'SANTIAGO' OR nom_comuna = 'SAN BERNARDO' OR nom_comuna = 'PUENTE ALTO')
")

zonas_hacinamiento_sf <- st_read(con, query = "
  SELECT * FROM dpa.zonas_hacinamiento
")

# VISUALIZACIÓN
ggplot(zonas_hacinamiento_sf) +
  geom_sf(aes(fill = hacinamiento_pct), color = "black", size = 0.2) +
  scale_fill_gradient(low = "lightcoral", high = "darkred", na.value = "grey90",
                      name = "Hacinamiento (%)") +
  theme_minimal() +
  labs(title = "Porcentaje de personas en situación de hacinamiento",
       subtitle = "Zonas censales del Gran Santiago") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())






# LIBRERÍAS
library(rakeR)
library(RPostgres)
library(DBI)
library(ggplot2)
library(sf)

# ENTRADAS
cons_censo_df <- readRDS("data/cons_censo_df.rds")
casen_raw <- readRDS("data/casen_rm.rds")

# VARIABLES BASE
col_cons <- sort(setdiff(names(cons_censo_df), c("GEOCODIGO", "COMUNA")))
age_levels  <- grep("^edad", col_cons, value = TRUE)
esc_levels  <- grep("^esco", col_cons, value = TRUE)
sexo_levels <- grep("^sexo_", col_cons, value = TRUE)

vars_base <- c("estrato", "esc", "edad", "sexo", "e6a", "s19a")

casen <- casen_raw[, vars_base, drop = FALSE]
rm(casen_raw)

# PREPROCESAMIENTO
casen$Comuna <- substr(as.character(casen$estrato), 1, 5)
casen$estrato <- NULL

casen$esc <- as.integer(unclass(casen$esc))
casen$edad <- as.integer(unclass(casen$edad))
casen$e6a <- as.numeric(unclass(casen$e6a))
casen$sexo <- as.integer(unclass(casen$sexo))
casen$s19a <- as.integer(unclass(casen$s19a))

# Recode s19a: 1 = tuvo problemas para llegar, 0 = no
casen$problemas_acceso <- ifelse(casen$s19a == 1, 1,
                                 ifelse(casen$s19a == 2, 0, NA))

# IMPUTACIÓN DE ESCOLARIDAD
idx_na <- which(is.na(casen$esc))
fit <- lm(esc ~ e6a, data = casen[-idx_na, ])
pred <- predict(fit, newdata = casen[idx_na, , drop = FALSE])
casen$esc[idx_na] <- as.integer(round(pmax(0, pmin(29, pred))))

casen$ID <- as.character(seq_len(nrow(casen)))

# RECODIFICACIÓN PARA RAKE
casen$edad_cat <- cut(
  casen$edad,
  breaks = c(0, 30, 40, 50, 60, 70, 80, Inf),
  labels = age_levels,
  right = FALSE, include.lowest = TRUE
)

casen$esc_cat <- factor(
  with(casen,
       ifelse(esc == 0, esc_levels[1],
              ifelse(esc <= 8, esc_levels[2],
                     ifelse(esc <= 12, esc_levels[3], esc_levels[4])))),
  levels = esc_levels
)

casen$sexo_cat <- factor(
  ifelse(casen$sexo == 2, sexo_levels[1],
         ifelse(casen$sexo == 1, sexo_levels[2], NA)),
  levels = sexo_levels
)

# MICROSIMULACIÓN
cons_censo_comunas <- split(cons_censo_df, cons_censo_df$COMUNA)
inds_list <- split(casen, casen$Comuna)

sim_list <- lapply(names(cons_censo_comunas), function(zona) {
  cons_i <- cons_censo_comunas[[zona]]
  col_order <- sort(setdiff(names(cons_i), c("COMUNA", "GEOCODIGO")))
  cons_i <- cons_i[, c("GEOCODIGO", col_order), drop = FALSE]
  
  tmp <- inds_list[[zona]]
  inds_i <- tmp[, c("ID", "edad_cat", "esc_cat", "sexo_cat"), drop = FALSE]
  names(inds_i) <- c("ID", "Edad", "Escolaridad", "Sexo")
  
  w_frac <- weight(cons = cons_i, inds = inds_i,
                   vars = c("Edad", "Escolaridad", "Sexo"))
  sim_i <- integerise(weights = w_frac, inds = inds_i, seed = 123)
  merge(sim_i, tmp[, c("ID", "problemas_acceso")], by = "ID", all.x = TRUE)
})

sim_df <- data.table::rbindlist(sim_list, idcol = "COMUNA")

# CÁLCULO DE % QUE TUVO PROBLEMAS PARA LLEGAR A CONSULTA
zonas_acceso <- aggregate(
  problemas_acceso ~ zone,
  data = sim_df,
  FUN = function(x) {
    total <- sum(!is.na(x))
    con_problemas <- sum(x == 1, na.rm = TRUE)
    round(100 * con_problemas / total, 2)
  }
)

names(zonas_acceso) <- c("geocodigo", "problemas_para_llegar")

# EXPORTACIÓN A POSTGRESQL
con <- dbConnect(
  Postgres(),
  dbname = "censo_rm_2017",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "postgres"
)

dbWriteTable(
  conn = con,
  name = Id(schema = "dpa", table = "tmp_problemas_acceso_rm"),
  value = zonas_acceso,
  overwrite = TRUE,
  row.names = FALSE
)

dbExecute(con, "CREATE INDEX ON dpa.tmp_problemas_acceso_rm(geocodigo)")
dbExecute(con, "ANALYZE dpa.tmp_problemas_acceso_rm")
dbExecute(con, "DROP TABLE IF EXISTS dpa.zonas_problemas_acceso")

dbExecute(con, "
  CREATE TABLE dpa.zonas_problemas_acceso AS
  SELECT
    z.*,
    t.problemas_para_llegar
  FROM dpa.zonas_censales_rm AS z
  LEFT JOIN dpa.tmp_problemas_acceso_rm AS t
    ON z.geocodigo::text = t.geocodigo
  WHERE urbano = 1 AND (nom_provin = 'SANTIAGO' OR nom_comuna = 'SAN BERNARDO' OR nom_comuna = 'PUENTE ALTO')
")

zonas_problemas_acceso_sf <- st_read(con, query = "
  SELECT * FROM dpa.zonas_problemas_acceso
")

# VISUALIZACIÓN
ggplot(zonas_problemas_acceso_sf) +
  geom_sf(aes(fill = problemas_para_llegar), color = "black", size = 0.2) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey90",
                      name = "Problemas para llegar (%)") +
  theme_minimal() +
  labs(title = "Problemas para acceder a atención médica",
       subtitle = "Porcentaje de personas con dificultades de acceso por zona censal") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())




library(dplyr)
library(sf)
library(biscale)
library(ggplot2)
library(cowplot)

# Preparar datos para bivariado
df_problemas_sin_geom <- zonas_problemas_acceso_sf %>%
  st_set_geometry(NULL) %>%
  select(geocodigo, problemas_para_llegar)

bivariado_sf <- zonas_saludmental_sf %>%
  left_join(df_problemas_sin_geom, by = "geocodigo") %>%
  filter(!is.na(atencion_salud_mental), !is.na(problemas_para_llegar))

# Crear variable bivariada con paleta aceptada (oscura)
bi_data <- bi_class(bivariado_sf,
                    x = atencion_salud_mental,
                    y = problemas_para_llegar,
                    style = "quantile", dim = 3)

# Crear mapa bivariado
map_bi <- ggplot() +
  geom_sf(data = bi_data, aes(fill = bi_class), color = "white", size = 0.1) +
  bi_scale_fill(pal = "DkBlue", dim = 3, guide = FALSE) +  # <- Paleta válida y oscura
  labs(title = "Mapa bivariado: Atención en salud mental vs Problemas de acceso",
       subtitle = "Región Metropolitana",
       caption = "Fuente: Microsimulación con CASEN 2022 y CENSO 2017") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# Crear leyenda
legend_bi <- bi_legend(pal = "DkBlue", dim = 3,
                       xlab = "Mayor acceso a salud mental →",
                       ylab = "Mayor dificultad para llegar →",
                       size = 8)

# Combinar mapa y leyenda
final_plot <- ggdraw() +
  draw_plot(map_bi, 0, 0, 1, 1) +
  draw_plot(legend_bi, 0.7, 0.1, 0.25, 0.25)

# Mostrar resultado
print(final_plot)
