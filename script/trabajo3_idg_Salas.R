install.packages("GGally")
install.packages("plotly")
install.packages("RPostgres")
install.packages("sf")
install.packages("factoextra")
install.packages("vegan")
# =============================================================================
# 2) CARGAR LIBRERÍAS NECESARIAS
# =============================================================================
library(vegan)
library(GGally)
library(plotly)
library(factoextra)
library(ggfortify)
library(DBI)
library(RPostgres)
library(sf)
library(ggplot2)
library(cowplot)
library(tibble)


# =============================================================================
# 2) CONEXIÓN
# =============================================================================
con <- dbConnect(
  Postgres(),
  dbname = "censo_rm_2017",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "postgres"
)


# =============================================================================
# 1) CARGAR DATOS MICROSIMULADOS DESDE POSTGRES
# =============================================================================

# Atención salud mental
df_salud <- dbGetQuery(con, "
  SELECT geocodigo::double precision, atencion_salud_mental
  FROM dpa.tmp_saludmental_rm
")

# Problemas para llegar a consulta
df_acceso <- dbGetQuery(con, "
  SELECT geocodigo::double precision, problemas_para_llegar
  FROM dpa.tmp_problemas_acceso_rm
")

# Ingreso mediano
df_ingreso <- dbGetQuery(con, "
  SELECT geocodigo::double precision, mediana_ingreso
  FROM dpa.tmp_ingreso_rm
")

# Unir las tres variables
df_indicadores <- Reduce(function(x, y) merge(x, y, by = "geocodigo", all = TRUE),
                         list(df_salud, df_acceso, df_ingreso))

# Eliminar filas con NA
df_indicadores <- na.omit(df_indicadores)

# =============================================================================
# 2) ESCALAR VARIABLES Y HACER K-MEANS
# =============================================================================

# Selección y escalado
vars_clusters <- df_indicadores[, c("atencion_salud_mental", "problemas_para_llegar", "mediana_ingreso")]
vars_scaled <- scale(vars_clusters)

# Determinar número óptimo de clusters
fviz_nbclust(vars_scaled, kmeans, method = "wss") +
  labs(title = "Método del Codo", x = "Número de clusters", y = "WSS")

# Aplicar K-means
set.seed(123)
km <- kmeans(vars_scaled, centers = 3, nstart = 25)

# Asignar cluster
df_indicadores$cluster <- as.factor(km$cluster)

# Gráfico de relaciones entre variables
df_plot <- df_indicadores[, c("atencion_salud_mental", "problemas_para_llegar", "mediana_ingreso", "cluster")]

ggpairs(
  df_plot,
  columns = 1:3,
  mapping = aes(color = cluster),
  upper = list(continuous = "points"),
  lower = list(continuous = "points"),
  diag = list(continuous = "densityDiag")
)

# =============================================================================
# 3) MAPA DE CLUSTERS
# =============================================================================

# Leer geometría de zonas censales
sql_geom = "
SELECT geocodigo::double precision, geom
FROM dpa.zonas_censales_rm
WHERE urbano = 1 AND (nom_provin = 'SANTIAGO' OR nom_comuna IN ('PUENTE ALTO', 'SAN BERNARDO'))
"
sf_zonas <- st_read(con, query = sql_geom)

# Unir geometría con datos de clustering
sf_mapa <- merge(sf_zonas, df_indicadores, by = "geocodigo")

# Leer geometría de comunas para bordes y etiquetas
sql_comunas = "
SELECT cut, nom_comuna, geom
FROM dpa.comunas_rm_shp
WHERE nom_provin = 'SANTIAGO'
"
sf_comunas_santiago <- st_read(con, query = sql_comunas)

# Bounding box
bbox <- st_bbox(sf_mapa)

# Crear mapa de clusters
ggplot() +
  geom_sf(data = sf_mapa, aes(fill = cluster), color = NA) +
  geom_sf(data = sf_comunas_santiago, fill = NA, color = "black", size = 0.3) +
  geom_sf_text(data = st_centroid(sf_comunas_santiago), aes(label = nom_comuna), size = 2) +
  scale_fill_brewer(palette = "Set2", name = "Cluster") +
  labs(
    title = "Clusters de Zonas Censales según variables microsimuladas",
    subtitle = "Gran Santiago, 2025"
  ) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]),
           expand = FALSE) +
  theme_void()





# Mapa de atención en salud mental
ggplot(sf_mapa) +
  geom_sf(aes(fill = atencion_salud_mental), color = NA) +
  scale_fill_viridis_c(name = "% Atención Salud Mental", option = "C") +
  theme_minimal() +
  labs(title = "Porcentaje de personas con atención en salud mental")

# Mapa de problemas de acceso
ggplot(sf_mapa) +
  geom_sf(aes(fill = problemas_para_llegar), color = NA) +
  scale_fill_viridis_c(name = "% Problemas de acceso", option = "D") +
  theme_minimal() +
  labs(title = "Porcentaje que tuvo problemas para llegar a la atención")

# Mapa de ingreso mediano
ggplot(sf_mapa) +
  geom_sf(aes(fill = mediana_ingreso), color = NA) +
  scale_fill_viridis_c(name = "Ingreso mediano", option = "B") +
  theme_minimal() +
  labs(title = "Ingreso per cápita mediano por zona censal")



# Leer comunas si no lo habías hecho
sql_comunas = "
SELECT cut, nom_comuna, geom
FROM dpa.comunas_rm_shp
WHERE nom_provin = 'SANTIAGO'
"
sf_comunas <- st_read(con, query = sql_comunas)

# Unir zona censal a comuna
sf_mapa_comunas <- st_join(sf_mapa, sf_comunas[, c("cut", "nom_comuna")])



library(tibble)

tabla_shannon <- sf_mapa_comunas |>
  st_drop_geometry() |>
  count(nom_comuna, cluster) |>
  filter(!is.na(nom_comuna)) |>
  tidyr::pivot_wider(names_from = cluster, values_from = n, values_fill = 0) |>
  column_to_rownames("nom_comuna")


library(vegan)

shannon <- diversity(tabla_shannon, index = "shannon")

df_shannon <- data.frame(
  nom_comuna = names(shannon),
  shannon_index = shannon
)


sf_comunas_shannon <- merge(sf_comunas, df_shannon, by = "nom_comuna")

ggplot(sf_comunas_shannon) +
  geom_sf(aes(fill = shannon_index), color = "white") +
  scale_fill_viridis_c(name = "Índice de Shannon") +
  labs(
    title = "Variabilidad interna de clusters por comuna",
    subtitle = "Índice de diversidad (mayor = más heterogénea)"
  ) +
  theme_minimal()

