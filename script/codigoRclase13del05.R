install.packages("rakeR")
install.packages("RPostgres")
install.packages("ggplot2")
# LIBRERÍAS
library(rakeR)
library(RPostgres)
library(DBI)
library(ggplot2)
library(sf)
library(data.table)

# ENTRADAS
cons_censo_df <- readRDS("data/cons_censo_df.rds")
casen_raw <- readRDS("data/casen_rm.rds")

# VARIABLES BASE
col_cons <- sort(setdiff(names(cons_censo_df), c("GEOCODIGO", "COMUNA")))
age_levels  <- grep("^edad", col_cons, value = TRUE)
esc_levels  <- grep("^esco", col_cons, value = TRUE)
sexo_levels <- grep("^sexo_", col_cons, value = TRUE)

# Usamos escolaridad, edad, sexo, y la variable a microsimular: ingreso per cápita
vars_base <- c("estrato", "esc", "edad", "sexo", "e6a", "ypc")

casen <- casen_raw[, vars_base, drop = FALSE]
rm(casen_raw)

# PREPROCESAMIENTO
casen$Comuna <- substr(as.character(casen$estrato), 1, 5)
casen$estrato <- NULL

# Conversión de variables
casen$esc   <- as.integer(unclass(casen$esc))
casen$edad  <- as.integer(unclass(casen$edad))
casen$e6a   <- as.numeric(unclass(casen$e6a))
casen$sexo  <- as.integer(unclass(casen$sexo))
casen$ypc   <- as.numeric(unclass(casen$ypc))  # Ingreso

# IMPUTACIÓN DE ESCOLARIDAD
idx_na <- which(is.na(casen$esc))
fit <- lm(esc ~ e6a, data = casen[-idx_na, ])
pred <- predict(fit, newdata = casen[idx_na, , drop = FALSE])
casen$esc[idx_na] <- as.integer(round(pmax(0, pmin(29, pred))))

# Crear ID
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
  merge(sim_i, tmp[, c("ID", "ypc")], by = "ID", all.x = TRUE)
})

sim_df <- data.table::rbindlist(sim_list, idcol = "COMUNA")

# CÁLCULO DE INGRESO MEDIANO POR ZONA
zonas_ingreso <- aggregate(
  ypc ~ zone,
  data = sim_df,
  FUN = function(x) round(median(x, na.rm = TRUE), 0)
)

names(zonas_ingreso) <- c("geocodigo", "mediana_ingreso")

# EXPORTACIÓN A POSTGRESQL
con <- dbConnect(
  Postgres(),
  dbname = "censo_rm_2017",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "postgres"
)

# Subir tabla temporal
dbWriteTable(
  conn = con,
  name = Id(schema = "dpa", table = "tmp_ingreso_rm"),
  value = zonas_ingreso,
  overwrite = TRUE,
  row.names = FALSE
)

dbExecute(con, "CREATE INDEX ON dpa.tmp_ingreso_rm(geocodigo)")
dbExecute(con, "ANALYZE dpa.tmp_ingreso_rm")
dbExecute(con, "DROP TABLE IF EXISTS dpa.zonas_ingreso")

# Crear tabla con geometría para mapas
dbExecute(con, "
  CREATE TABLE dpa.zonas_ingreso AS
  SELECT
    z.*,
    t.mediana_ingreso
  FROM dpa.zonas_censales_rm AS z
  LEFT JOIN dpa.tmp_ingreso_rm AS t
    ON z.geocodigo::text = t.geocodigo
  WHERE urbano = 1 AND (nom_provin = 'SANTIAGO' OR nom_comuna = 'SAN BERNARDO' OR nom_comuna = 'PUENTE ALTO')
")

# VISUALIZACIÓN OPCIONAL
zonas_ingreso_sf <- st_read(con, query = "SELECT * FROM dpa.zonas_ingreso")

ggplot(zonas_ingreso_sf) +
  geom_sf(aes(fill = mediana_ingreso), color = "black", size = 0.2) +
  scale_fill_viridis_c(option = "D", name = "Ingreso mediano") +
  theme_minimal() +
  labs(title = "Mediana del ingreso per cápita",
       subtitle = "Zonas censales del Gran Santiago") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())
