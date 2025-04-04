#1. entradas

ruta_rds = "data/casen_rm.rds"
casen_rm = readRDS(ruta_rds)
hist(casen_rm$ypc)
umbral = quantile(casen_rm$ypc, 0.9, na.rm = TRUE)

casen_clean = casen_rm((casen_rm$ypc <= umbral))
                       