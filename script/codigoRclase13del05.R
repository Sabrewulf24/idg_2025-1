install.packages("rakeR")
install.packages("RPostgres")
install.packages("ggplot2")
library(rakeR)
library(DBI)
library(RPostgres)
library(ggplot2)

cons_censo_df <- readRDS("data/cons_censo_df.rds")
casen_raw = readRDS("data/casen_rm.rds")
View(cons_censo_df)
View(casen_raw)
# Cada registro de la case representa 1 perosna. ID, escolaridad, edad, sexo

# Ordenar y extraer una sola vez los nombres de las columnas de constraints
col_cons   = sort(setdiff(names(cons_censo_df), c("GEOCODIGO","COMUNA")))

# De ahí generar dinámicamente los niveles que luego deben coincidir con los factor levels
age_levels  <- grep("^edad", col_cons, value = TRUE)    # p.ej. "edad_menor_30", "edad_30_40", …
esc_levels  <- grep("^esco", col_cons, value = TRUE)    # p.ej. "esco_0","esco_1_8",…
sexo_levels <- grep("^sexo_",col_cons, value = TRUE)    # p.ej. "sexo_f","sexo_m"


# Sleccionar variables.. Se deben elimnar N.A

vars_base = c("estrato", # para extraer ID de comuna
              "esc", # Escolaridad
              "edad",
              "sexo",
              "e6a", # Imputar escolaridad
              "ypc") # Var a micro simular

# Flitrar CASEN
casen = casen_raw[ , vars_base, drop = FALSE]
rm(casen_raw) # Eliminar data sin utilizar

# Extraer comuna
casen$Comuna = substr(as.character(casen$estrato), 1, 5)
casen$estrato = NULL

# Se quitan etiquetas (transformar de heyven a dtaframe normal)
casen$e6a = as.integer(unclass(casen$e6a))
casen$ypc = as.integer(unclass(casen$ypc))
casen$Comuna = as.integer(unclass(casen$Comuna))
casen$sexo = as.integer(unclass(casen$sexo))
casen$edad = as.integer(unclass(casen$edad))
casen$esc = as.integer(unclass(casen$esc))
View(casen)

#debemos imputar los datos donde hay NA´S
cor(casen$esc, casen$e6a, use = "complete.obs")
idx_na= which(is.na(casen$esc))

#CREACION MODELO DE REGRESION
fit = lm(esc ~ e6a, data = casen[-idx_na,])
summary(fit)

#PREDICCION
pred = predict(fit, newdata = casen[idx_na, ,drop = FALSE])

#IMPUTAR DATOS
casen$esc[idx_na] = as.integer(round(pmax(0, pmin(29,pred))))

casen$ID = nrow(casen)

## Recodificamos 

casen$edad_cat <- cut(
  casen$edad,
  breaks = c(0,30,40,50,60,70,80,Inf),
  labels = age_levels,
  right = FALSE, include.lowest = TRUE
)

casen$esc_cat <- factor(
  with(casen,
       ifelse(esc == 0,           esc_levels[1],
              ifelse(esc <= 8,    esc_levels[2],
                     ifelse(esc <= 12, esc_levels[3],
                            esc_levels[4])))),
  levels = esc_levels
)

casen$sexo_cat <- factor(
  ifelse(casen$sexo == 2, sexo_levels[1],  
         ifelse(casen$sexo == 1, sexo_levels[2], NA)), 
  levels = sexo_levels
)



## Microsimulación
# crear la lista de constraints POR COMUNA
cons_censo_comunas = split(cons_censo_df, cons_censo_df$COMUNA)

# Lista de INDS 
inds_list = split(casen, casen$Comuna)


sim_list = lapply(names(cons_censo_comunas), function(zona) {
  cons_i    = cons_censo_comunas[[zona]]
  col_order = sort(setdiff(names(cons_i), c("COMUNA","GEOCODIGO")))
  cons_i    = cons_i[, c("GEOCODIGO", col_order), drop = FALSE]
  
  tmp    = inds_list[[zona]]
  inds_i = tmp[, c("ID","edad_cat","esc_cat","sexo_cat"), drop = FALSE]
  names(inds_i) = c("ID","Edad","Escolaridad","Sexo")
  
  
  
  w_frac  = weight(cons = cons_i, inds = inds_i,
                   vars = c("Edad","Escolaridad","Sexo"))
  sim_i   = integerise(weights = w_frac, inds = inds_i, seed = 123)
  merge(sim_i,
        tmp[, c("ID","ypc")],
        by = "ID", all.x = TRUE)
})

# Data Frame de toda la población
sim_df = data.table::rbindlist(sim_list, idcol = "COMUNA")

zonas_ypc = aggregate(
  ypc ~ zone,
  data = sim_df,
  FUN  = function(x) median(x, na.rm = TRUE)
)
names(zonas_ypc) <- c("geocodigo", "mediana_ingreso")

source("trabajos/t2_microsim/R/conexion_db.R")

con = conectar_db("censo_rm_2017")

dbWriteTable(
  conn      = con,
  name      = Id(schema = "dpa", table = "tmp_ingreso_rm"),
  value     = zonas_ypc,
  overwrite = TRUE,
  row.names = FALSE
)

dbExecute(con, "CREATE INDEX ON dpa.tmp_ingreso_rm(geocodigo)")
dbExecute(con, "ANALYZE dpa.tmp_ingreso_rm")

# 1) Crea la nueva capa directamente con un SELECT … LEFT JOIN
dbExecute(con, "
  CREATE TABLE dpa.zonas_censales_gs_income AS
  SELECT
    z.*,
    t.mediana_ingreso
  FROM dpa.zonas_censales_rm AS z
  LEFT JOIN dpa.tmp_ingreso_rm AS t
    ON z.geocodigo::text = t.geocodigo
WHERE urbano = 1 AND (nom_provin = 'SANTIAGO' OR nom_comuna = 'SAN BERNARDO' OR nom_comuna = 'PUENTE ALTO')
")