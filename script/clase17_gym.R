library(haven)
# Leer archivos Stata
personas <- read_dta("data/datos_epf/base-personas-ix-epf-stata.dta")
gastos   <- read_dta("data/datos_epf/base-gastos-ix-epf-stata.dta")
cantidades <- read_dta("data/datos_epf/base-cantidades-ix-epf-stata.dta")
ccif     <- read_dta("data/datos_epf/ccif-ix-epf-stata.dta")

#ahora se filtra la zona para solo trabajar con el gran santiago
# Filtro para valores inválidos.
valores_invalidos <- c(-99, -88, -77)

personas_gs = personas[personas$macrozona ==2,]
# Edad y escolaridad
personas_gs = personas_gs[!(personas_gs$edad %in% valores_invalidos), ]
personas_gs = personas_gs[!(personas_gs$edue %in% valores_invalidos), ]
personas_gs = personas_gs[!(personas_gs$ing_disp_hog_hd_ai < 0), ]
View(personas_gs)
# Se calcula el ingreso per cápita
personas_gs$ing_pc = personas_gs$ing_disp_hog_hd_ai / personas_gs$npersonas

#filtrar la base de entidades en funcion
gastos_servicio =
  gastos[
    gastos$ccif == "09.4.6.02.04",
  ]

#filtar gasto servicio por hogar
gasto_hogar_servicio = merge(gastos_servicio, personas_gs, by = "folio")