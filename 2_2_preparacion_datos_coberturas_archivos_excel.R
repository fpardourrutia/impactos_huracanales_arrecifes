# En este script se homologan los datos de remuestreos de sitio correspondientes
# a porcentajes de cobertura coralina, provenientes de los archivos de Excel.
# Estos archivos ya tienen la forma deseada, a saber:
# nombre_sitio,
# latitud,
# longitud,
# fecha_muestra_inicial,
# fecha_muestra_final,
# porcentaje_cobertura_coral_muestra_inicial,
# porcentaje_cobertura_coral_muestra_final
# fuente,
# datos_especificos_analisis_huracanes,
# otras_variables...
# Sin embargo, se requiere un poco de trabajo para que queden exactamente en el
# mismo formato y se puedan fusionar.

library("plyr")
library("dplyr")
library("tidyr")
library("lubridate")
library("readxl")
source("0_config.R")

################################################################################
# 1. Leyendo los archivos de Excel
################################################################################

remuestreos_porcentajes_cobertura_datos_especiales_huracanes <-
  read_excel(rutas_archivos_insumos["remuestreos_porcentajes_cobertura_datos_especiales_huracanes"])
glimpse(remuestreos_porcentajes_cobertura_datos_especiales_huracanes)

remuestreos_porcentajes_cobertura_florida_islas_virgenes_no_se_sabe_si_hay_huracanes <- 
  read_excel(rutas_archivos_insumos[
    "remuestreos_porcentajes_cobertura_florida_islas_virgenes_no_se_sabe_si_hay_huracanes"])
glimpse(remuestreos_porcentajes_cobertura_florida_islas_virgenes_no_se_sabe_si_hay_huracanes)

remuestreos_porcentajes_cobertura_no_se_sabe_si_hay_huracanes <-
  read_excel(rutas_archivos_insumos["remuestreos_porcentajes_cobertura_no_se_sabe_si_hay_huracanes"])
glimpse(remuestreos_porcentajes_cobertura_no_se_sabe_si_hay_huracanes)

nrow(remuestreos_porcentajes_cobertura_datos_especiales_huracanes)
nrow(remuestreos_porcentajes_cobertura_florida_islas_virgenes_no_se_sabe_si_hay_huracanes)
nrow(remuestreos_porcentajes_cobertura_no_se_sabe_si_hay_huracanes)

################################################################################
# 2. Homologando datos y creando variables adicionales
################################################################################

remuestreos_porcentajes_cobertura_exceles <- rbind(
  remuestreos_porcentajes_cobertura_datos_especiales_huracanes %>%
    mutate(
      datos_especificos_analisis_huracanes = TRUE,
      fuente = "archivos Excel"
    ),
  remuestreos_porcentajes_cobertura_florida_islas_virgenes_no_se_sabe_si_hay_huracanes %>%
    mutate(
      datos_especificos_analisis_huracanes = FALSE,
      fuente = "archivos Excel James"
    ),
  remuestreos_porcentajes_cobertura_no_se_sabe_si_hay_huracanes %>%
    mutate(
      datos_especificos_analisis_huracanes = FALSE,
      fuente = "archivos Excel"
    )
  ) %>%
  mutate(
    
    # Variables informativas

    fecha_muestra_inicial = paste0(anio_antes_del_huracan, "-", mes_antes_del_huracan,
      "-", dia_antes_del_huracan) %>%
      ymd(),
    fecha_muestra_final = paste0(anio_despues_del_huracan, "-", mes_despues_del_huracan,
      "-", dia_despues_del_huracan) %>%
      ymd()
    
  ) %>%
  rename(
    nombre_sitio = nombre_del_sitio
  ) %>%
  select(
    -ends_with("huracan"),
    -Serie
  )
glimpse(remuestreos_porcentajes_cobertura_exceles)
summary(remuestreos_porcentajes_cobertura_exceles)

saveRDS(remuestreos_porcentajes_cobertura_exceles,
  rutas_archivos_productos["datos_cobertura_archivos_excel"])

