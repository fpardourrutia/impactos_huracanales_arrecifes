# En este script se incluyen las siguientes variables adicionales, que
# únicamente dependen de la geografía de los sitios muestreados:
# 1. Exposición: ¿Qué tan expuesto está un sitio de muestreo ante los vientos
# huracanales?
# 2. Para los sitios de los archivos de Excel se adicionarán las variables
# de "tipo_arrecife", "zona_arrecifal", "profundidad_media_m".

# Cabe destacar que cuando se realice el análisis por complejidad arrecifal,
# este script se deberá modificar para incorporarlo (en lugar de
# "datos_cobertura_integrados" se tendrán "datos_integrados").

library("plyr")
library("dplyr")
library("tidyr")
library("readxl")
library("readr")
source("0_config.R")
source("funciones_auxiliares.R")

################################################################################
# 1. Obteniendo las bases de datos de interés para la adición de las variables
# nuevas
################################################################################

datos_cobertura_integrados <- readRDS(rutas_archivos_productos["datos_cobertura_integrados"])
glimpse(datos_cobertura_integrados)

variables_adicionales_todos_sitios <- read_excel(
  rutas_archivos_insumos["variables_adicionales_todos_sitios"]) %>%
  mutate(
    nombre_sitio = estandariza_strings(nombre_sitio)
  ) %>%
  # Seleccionando columnas apropiadas, posiblemente con cambios de nombre para
  # evitar conflictos
  select(
    nombre_sitio,
    exposicion,
    orden_exposicion,
    tipo_arrecife_adicional = tipo_arrecife,
    zona_arrecifal_adicional = zona_arrecifal,
    profundidad_media_m_adicional = profundidad_media_m
  )
glimpse(variables_adicionales_todos_sitios)
summary(variables_adicionales_todos_sitios)

################################################################################
# 2. Revisiones varias de las tablas anteriores.
################################################################################

# Revisando la base de datos con los datos de cobertura integrados antes de
# proseguir:
datos_cobertura_integrados %>%
  select(
    fuente,
    datos_especificos_analisis_huracanes
  ) %>%
  table(useNA = "ifany")
# Perfecto, es la base de datos deseada.

# Revisando que "nombre_sitio" sea una llave primaria de la tabla con las
# variables adicionales:
nrow(variables_adicionales_todos_sitios)
variables_adicionales_todos_sitios %>%
  distinct(nombre_sitio) %>%
  nrow()
# Perfecto!

# Verificando consistencia de las exposiciones con sus órdenes
table(variables_adicionales_todos_sitios$exposicion,
  variables_adicionales_todos_sitios$orden_exposicion)

################################################################################
# 3. Creando el data frame parcial que contendrá la información de todas las
# variables dependientes del sitio (a excepción del fetch)
################################################################################

datos_cobertura_integrados_variables_adicionales_no_fetch <- datos_cobertura_integrados %>%
  left_join(variables_adicionales_todos_sitios, by = "nombre_sitio") %>%
  # Completando variables existentes en "datos_cobertura_integrados" con la
  # información contenida en el nuevo archivo de Excel
  mutate(
    tipo_arrecife = ifelse(
      !is.na(tipo_arrecife), tipo_arrecife, tipo_arrecife_adicional),
    zona_arrecifal = ifelse(
      !is.na(zona_arrecifal), zona_arrecifal, zona_arrecifal_adicional),
    profundidad_media_m = ifelse(
      !is.na(profundidad_media_m), profundidad_media_m,
      profundidad_media_m_adicional)
  ) %>%
  select(
    -tipo_arrecife_adicional,
    -zona_arrecifal_adicional,
    -profundidad_media_m_adicional
  )
  
summary(datos_cobertura_integrados_variables_adicionales_no_fetch)
glimpse(datos_cobertura_integrados_variables_adicionales_no_fetch)

################################################################################
# 4. Revisiones adicionales
################################################################################

# Revisando si no se crearon artefactos del join
nrow(datos_cobertura_integrados_variables_adicionales_no_fetch) ==
  nrow(datos_cobertura_integrados)
# Perfecto! todo va bien hasta ahora

# Revisando si hay sitios a los que no se les asoció exposición.
datos_cobertura_integrados %>%
  anti_join(variables_adicionales_todos_sitios, by = "nombre_sitio") %>%
  distinct(nombre_sitio, latitud, longitud) %>%
  arrange(nombre_sitio) %>%
  write_csv(rutas_archivos_productos["sitios_sin_exposicion"])
# No hay, perfecto!

# Revisando si hay registros de exposición donde no hay sitio asociado
variables_adicionales_todos_sitios %>%
  anti_join(datos_cobertura_integrados, by = "nombre_sitio") %>%
  distinct(nombre_sitio) %>%
  arrange(nombre_sitio) %>%
  write_csv(rutas_archivos_productos["exposiciones_sin_sitio"])
# Sí hay, pero Esme dice que son los sitios donde sólo se tomó complejidad.

# Revisiones de campos individuales
tablas_revision <- revisa_valores(datos_cobertura_integrados_variables_adicionales_no_fetch)
names(tablas_revision)

saveRDS(datos_cobertura_integrados_variables_adicionales_no_fetch,
  rutas_archivos_productos["datos_cobertura_integrados_variables_adicionales_no_fetch"])