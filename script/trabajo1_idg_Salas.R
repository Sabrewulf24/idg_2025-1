# =============================================================================
# 1) INSTALAR PAQUETES (solo una vez)
# =============================================================================
# Estos paquetes permiten conexión a BD, manejo de geometrías y visualización
# install.packages(c("DBI", "RPostgres", "sf", "ggplot2", "cowplot", "biscale"))

# =============================================================================
# 2) CARGAR LIBRERÍAS NECESARIAS
# =============================================================================

library(DBI)
library(RPostgres)
library(sf)
library(ggplot2)
library(cowplot)
library(biscale)

# =============================================================================
# 3) CONFIGURAR CONEXIÓN A BASE DE DATOS
# =============================================================================
# Definir parámetros de conexión
db_host     = "localhost"       # servidor de BD
db_port     = 5432                # puerto de escucha
db_name     = "Censo2017"   # nombre de la base
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
# 4) EXTRAER INDICADORES DESDE CENSO - MODIFICADO CON NUEVAS CONSULTAS
# =============================================================================

# Consulta: % adultos mayores y % en viviendas precarias por comuna
sql_indicadores = "
SELECT c.nom_comuna,
     c.codigo_comuna::double precision,
	 COUNT(*) AS total_personas,
     COUNT(*) filter (where p.p09 >65) AS total_adultos_mayores,
	 ROUND(COUNT(*) filter (where p.p09 >65) * 100.0 / COUNT(*),2) AS ptje_adultos_mayores
	 
FROM personas AS p
JOIN hogares AS h ON p.hogar_ref_id = h.hogar_ref_id  
JOIN viviendas AS v ON h.vivienda_ref_id = v.vivienda_ref_id
JOIN zonas AS z ON v.zonaloc_ref_id = z.zonaloc_ref_id
JOIN comunas AS c ON z.codigo_comuna = c.codigo_comuna
GROUP BY c.nom_comuna, c.codigo_comuna
ORDER BY ptje_adultos_mayores DESC;
"

df_adultos_mayores = dbGetQuery(con, sql_indicadores)

sql_viviendas_precarias = "
SELECT 
    c.nom_comuna,
    c.codigo_comuna::double precision,
    COUNT(*) AS total_personas,
    COUNT(*) FILTER (
        WHERE v.p01 = 5 OR v.p03b = 6
    ) AS total_personas_viv_precaria,
    ROUND(
        COUNT(*) FILTER (WHERE v.p01 = 5 OR v.p03b = 6) * 100.0 / COUNT(*),
        2
    ) AS ptje_viv_precarias
FROM personas AS p
JOIN hogares AS h ON p.hogar_ref_id = h.hogar_ref_id
JOIN viviendas AS v ON h.vivienda_ref_id = v.vivienda_ref_id
JOIN zonas AS z ON v.zonaloc_ref_id = z.zonaloc_ref_id
JOIN comunas AS c ON z.codigo_comuna = c.codigo_comuna
GROUP BY c.nom_comuna, c.codigo_comuna
ORDER BY ptje_viv_precarias DESC;

"

df_viviendas_precarias = dbGetQuery(con, sql_viviendas_precarias)

# Unir ambos indicadores en un solo data.frame
df_indicadores = merge(df_adultos_mayores, df_viviendas_precarias, by = c("codigo_comuna", "nom_comuna"))

# =============================================================================
# 5) CARGAR GEOMETRÍA DE COMUNAS
# =============================================================================
sql_geometria = "
SELECT
  cut::double precision AS codigo_comuna,
  nom_comuna,
  geom
FROM dpa.comunas_rm_shp
WHERE nom_provin = 'SANTIAGO';
"
sf_comunas = st_read(con, query = sql_geometria)

# =============================================================================
# 6) COMBINAR DATOS TABULARES Y ESPACIALES
# =============================================================================
sf_mapa = merge(
  x     = sf_comunas,
  y     = df_indicadores,
  by    = "codigo_comuna",
  all.x = FALSE
)

# =============================================================================
# 7) MAPAS TEMÁTICOS SIMPLES
# =============================================================================
map_adultos_mayores = ggplot(sf_mapa) +
  geom_sf(aes(fill = ptje_adultos_mayores), color = "#AAAAAA30", size = 0.1) +  
  labs(
    title = "Porcentaje de Adultos Mayores",
    fill  = "% Adultos Mayores"
  ) +
  theme_minimal()

map_viv_precarias = ggplot(sf_mapa) +
  geom_sf(aes(fill = ptje_viv_precarias), color = "#AAAAAA30", size = 0.1) +
  labs(
    title = "Porcentaje de Viviendas Precarias",
    fill  = "% Viviendas Precarias"
  ) +
  theme_minimal()

print(map_adultos_mayores)
print(map_viv_precarias)

# =============================================================================
# 8) GRÁFICO DE DISPERSIÓN BIVARIADO
# =============================================================================
# 8.1 Calcular medianas para dividir cuadrantes
mediana_adultos_mayores = median(sf_mapa$ptje_adultos_mayores, na.rm = TRUE)
mediana_viv_precarias    = median(sf_mapa$ptje_viv_precarias,   na.rm = TRUE)

# 8.2 Crear la variable que indica el cuadrante según comparaciones con medianas
sf_mapa$cuadrante = with(
  sf_mapa,
  ifelse(
    ptje_adultos_mayores >= mediana_adultos_mayores & ptje_viv_precarias >= mediana_viv_precarias, 'Q1: Alta/Alta',
    ifelse(
      ptje_adultos_mayores <  mediana_adultos_mayores & ptje_viv_precarias >= mediana_viv_precarias, 'Q2: Baja/Alta',
      ifelse(
        ptje_adultos_mayores <  mediana_adultos_mayores & ptje_viv_precarias <  mediana_viv_precarias, 'Q3: Baja/Baja',
        'Q4: Alta/Baja'
      )
    )
  )
)

# 8.3 Definir paleta de colores manual para cada cuadrante
colores_cuadrantes = c(
  'Q1: Alta/Alta' = '#08519c',  # alto/alto
  'Q2: Baja/Alta' = '#6baed6',  # bajo/alto
  'Q3: Baja/Baja' = '#eff3ff',  # bajo/bajo
  'Q4: Alta/Baja' = '#bdd7e7'   # alto/bajo
)

# 8.4 Construir scatterplot con líneas de mediana
grafico_cuadrantes = ggplot(
  sf_mapa,
  aes(
    x     = ptje_adultos_mayores,
    y     = ptje_viv_precarias,
    color = cuadrante
  )
) +
  geom_point(size = 2) +
  geom_vline(xintercept = mediana_adultos_mayores, linetype = 'dashed', color = 'gray50') +
  geom_hline(yintercept = mediana_viv_precarias,    linetype = 'dashed', color = 'gray50') +
  scale_color_manual(name = 'Cuadrante', values = colores_cuadrantes) +
  labs(x = '% Adultos Mayores', y = '% Viviendas Precarias', title = 'Dispersión por Cuadrantes') +
  theme_minimal()

print(grafico_cuadrantes)


# =============================================================================
# 9) MAPA BIVARIADO CON BISCALE
# =============================================================================
# 9.1 Obtener geometría comunal para Santiago
sql_comunas = "
SELECT cut, nom_comuna, geom
FROM dpa.comunas_rm_shp
WHERE nom_provin = 'SANTIAGO';
"
sf_comunas_santiago = st_read(con, query = sql_comunas)

# 9.2 Clasificar datos en 3 x 3 bivariado
sf_mapa_bi = bi_class(sf_mapa, x = ptje_adultos_mayores, y = ptje_viv_precarias, dim = 3, style = 'jenks')


# 9.3 Calcular bbox y centroides para etiquetas comunales
caja = sf::st_bbox(sf_mapa_bi)
sf_comunas_centroides = st_centroid(sf_comunas_santiago)

# 9.4 Crear mapa bivariado sin bordes internos y con etiquetas
mapa_bivariado_etiquetas = ggplot() +
  geom_sf(data = sf_mapa_bi, aes(fill = bi_class), color = NA, show.legend = FALSE) +
  geom_sf(data = sf_comunas_santiago, fill = NA, color = 'black', size = 0.4) +
  geom_sf_text(data = sf_comunas_centroides, aes(label = nom_comuna), size = 1.75, fontface = 'bold') +
  bi_scale_fill(pal = 'DkBlue', dim = 3) +
  labs(title = 'Mapa bivariado para Adultos Mayores vs. Viviendas Precarias', subtitle = 'Provincia de Santiago, RM') +
  coord_sf(xlim = c(caja['xmin'], caja['xmax']), ylim = c(caja['ymin'], caja['ymax']), expand = FALSE) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5))

# 9.5 Generar y posicionar leyenda bivariada
leyenda_bivariada = bi_legend(pal = 'DkBlue', dim = 3, xlab = '% adultos mayores', ylab = '% Viviendas Precarias', size = 8)
mapa_final = ggdraw() +
  draw_plot(mapa_bivariado_etiquetas, x = 0,    y = 0,    width = 1,    height = 1) +
  draw_plot(leyenda_bivariada,          x = 0.75, y = 0.05, width = 0.30, height = 0.30)

print(mapa_final)
