# =============================================================================
# 2) CARGAR LIBRERÍAS NECESARIAS
# =============================================================================
install.packages("DBI")
install.packages("RPostgres")
install.packages("sf")
install.packages("ggplot2")
install.packages("cowplot")
install.packages("factoextra")
install.packages("ggfortify")
# Cargar librerías necesarias
library(factoextra)
library(ggfortify)
library(DBI)
library(RPostgres)
library(sf)
library(ggplot2)
library(cowplot)

# =============================================================================
# 3) CONFIGURAR CONEXIÓN A BASE DE DATOS
# =============================================================================
# Definir parámetros de conexión
db_host     = "localhost"       # servidor de BD
db_port     = 5432               # puerto de escucha
db_name     = "censo_rm_2017"   # nombre de la base
db_user     = "postgres"        # usuario de conexión
db_password = "postgres"        # clave de usuario

# Establecer conexión usando RPostgres
con = dbConnect(
  Postgres(),
  dbname   = db_name,
  host     = db_host,
  port     = db_port,
  user     = db_user,
  password = db_password
)

# =============================================================================
# 4) EXTRAER INDICADORES DESDE CENSO
# =============================================================================
# SQL para calcular:
# - % de personas con nivel educativo profesional (p15 entre 12 y 14)
# - % de viviendas con indicadores de hacinamiento (v.ind_hacin_rec en {2,4})

sql_indicadores = "
SELECT
  z.geocodigo::double precision AS geocodigo,
  c.nom_comuna,

  -- Porcentaje de migrantes
  ROUND(
    COUNT(*) FILTER (WHERE p.p12 NOT IN (1, 2, 98, 99)) * 100.0
    / NULLIF(COUNT(*), 0),
  2) AS ptje_migrantes,

  -- Porcentaje de personas con escolaridad mayor a 12 años
  ROUND(
    COUNT(*) FILTER (WHERE p.escolaridad >= 12) * 100.0
    / NULLIF(COUNT(*) FILTER (WHERE p.escolaridad IS NOT NULL), 0),
  2) AS ptje_esc_mayor_12,

  -- Porcentaje de adultos mayores
  ROUND(
    COUNT(*) FILTER (WHERE p.p09 >= 65) * 100.0
    / NULLIF(COUNT(*) FILTER (WHERE p.p09 IS NOT NULL), 0),
  2) AS ptje_adulto_mayor

FROM public.personas   AS p
JOIN public.hogares    AS h ON p.hogar_ref_id    = h.hogar_ref_id
JOIN public.viviendas  AS v ON h.vivienda_ref_id = v.vivienda_ref_id
JOIN public.zonas      AS z ON v.zonaloc_ref_id  = z.zonaloc_ref_id
JOIN public.comunas    AS c ON z.codigo_comuna   = c.codigo_comuna

GROUP BY z.geocodigo, c.nom_comuna
ORDER BY ptje_esc_mayor_12 DESC;
"
# Ejecutar consulta y importar resultados a data.frame en R
df_indicadores = dbGetQuery(con, sql_indicadores)

#------------------------------------------------------------------------------------------
#5) seleccionar variables y escalarlas
#------------------------------------------------------------------------------------------

vars_clusters = df_indicadores[,c("ptje_migrantes",
                                  "ptje_esc_mayor_12",
                                  "ptje_adulto_mayor")]
#se escalan las variables
vars_scaled = scale(vars_clusters)

# Visualiza la suma de cuadrados dentro del cluster (WSS) para varios K
fviz_nbclust(vars_scaled, kmeans, method = "wss") +
  labs(title = "Método del codo", x = "Número de clusters (K)", y = "WSS")

#5 k_means
set.seed(123)  # para reproducibilidad
km = kmeans(vars_scaled, centers = 4, nstart = 25)
km$cluster

df_indicadores$cluster = as.factor(km$cluster)
# escolaridad vs migracion
ggplot(df_indicadores, aes(x = ptje_esc_mayor_12, y = ptje_migrantes, color = cluster)) +
  geom_point(size = 2) +
  labs(title = "escolaridad v/s migrantes",
       x = "%poblacion con>=12 años de escolaridad",
       y = "% poblacion migrante") +
  theme_minimal()