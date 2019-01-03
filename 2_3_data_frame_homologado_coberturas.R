# En este script se homologan los datos de coberturas obtenidos de la base de
# datos y Exceles. También se realizan las revisiones apropiadas.

library("plyr")
library("dplyr")
library("tidyr")
library("readr")
library("readxl")
library("stringi")
library("lubridate")
library("geosphere") # Obtener distancias geográficas con la función "distGeo()"
source("funciones_auxiliares.R")
source("0_config.R")

################################################################################
# 1. Leyendo los datos
################################################################################
datos_cobertura_bd <- readRDS(rutas_archivos_productos["datos_cobertura_bd"])
glimpse(datos_cobertura_bd)

datos_cobertura_archivos_excel <- readRDS(
  rutas_archivos_productos["datos_cobertura_archivos_excel"])
glimpse(datos_cobertura_archivos_excel)

################################################################################
# 2. Creando la versión borrador del data frame integrado
################################################################################

# Esta versión será la que revisaré antes de crear la versión final
datos_cobertura_integrados_borrador <- rbind.fill(
  datos_cobertura_bd %>%
    select(
      nombre_sitio,
      #pais,
      tipo_arrecife,
      zona_arrecifal,
      latitud,
      longitud,
      fecha_muestra_inicial,
      fecha_muestra_final,
      #id_muestra_inicial,
      #id_muestra_final,
      muestreo_muestra_inicial,
      muestreo_muestra_final,
      tabla_muestra_inicial,
      tabla_muestra_final,
      profundidad_m_muestra_inicial,
      profundidad_m_muestra_final,
      porcentaje_cobertura_coral_muestra_inicial,
      porcentaje_cobertura_coral_muestra_final,
      fuente,
      datos_especificos_analisis_huracanes
    ) %>%
    mutate(
      # Obteniendo la profundidad
      profundidad_media_m = case_when(
        is.na(profundidad_m_muestra_inicial) ~ profundidad_m_muestra_final,
        is.na(profundidad_m_muestra_final) ~ profundidad_m_muestra_inicial,
        TRUE ~ (profundidad_m_muestra_final + profundidad_m_muestra_inicial) / 2
      ),
      # Generando una columna auxiliar para estimar si los muestreos fueron
      # exactamente en el mismo lugar
      # diferencia_profundidades_m = abs(profundidad_m_muestra_final -
      #   profundidad_m_muestra_inicial)
      
      # Arreglando valores de "tipo_arrecife" y "zona_arrecifal"
      tipo_arrecife = ifelse(tipo_arrecife == "" | tipo_arrecife == "Otra",
        NA, tipo_arrecife),
      zona_arrecifal = ifelse(zona_arrecifal == "" | zona_arrecifal == "Otra",
        NA, zona_arrecifal)
    ) %>%
    select(
      -profundidad_m_muestra_inicial,
      -profundidad_m_muestra_final
    ),
    datos_cobertura_archivos_excel
  ) %>%
  mutate(
    # id_remuestreo = 1:nrow(.), # Aún no, todavía...
    nombre_sitio = estandariza_strings(nombre_sitio)
  ) %>%
  filter(fecha_muestra_inicial < fecha_muestra_final)

summary(datos_cobertura_integrados_borrador)
glimpse(datos_cobertura_integrados_borrador)
View(datos_cobertura_integrados_borrador)

################################################################################
# 3. Revisiones varias
################################################################################

# Revisando que la base "datos_cobertura_integrados" en verdad contenga los
# datos procedentes de todas nuestras fuentes de interés
datos_cobertura_integrados_borrador %>%
  select(datos_especificos_analisis_huracanes, fuente) %>%
  table(useNA = "always")
# Perfecto, veo datos de las 4 fuentes de datos distintas que tengo

# Revisando los valores del data frame:
tablas_revision_borrador <- revisa_valores(datos_cobertura_integrados_borrador)
names(tablas_revision_borrador)

################################################################################

# Revisando que no haya registros de remuestreo duplicados usando la llave
# ("nombre_sitio", "fecha_muestra_inicial"):
nrow(datos_cobertura_integrados_borrador)
datos_cobertura_integrados_borrador %>%
  distinct(nombre_sitio, fecha_muestra_inicial) %>%
  nrow()
# Sí hay registros duplicados utilizando la llave anterior.

# Revisando cuáles muestreos están duplicados
datos_cobertura_integrados_borrador %>%
  group_by(nombre_sitio, fecha_muestra_inicial) %>%
  tally() %>%
  ungroup() %>%
  filter(n >= 2) %>%
  inner_join(datos_cobertura_integrados_borrador, by = c("nombre_sitio", "fecha_muestra_inicial")) %>%
  View()
# Claramente se trata de registros duplicados, por lo que podemos combinar y
# extraer su información de la manera que mejor nos convenga.

################################################################################

# Revisando que no haya registros de remuestreo duplicados usando la llave
# ("nombre_sitio", "fecha_muestra_final"):
nrow(datos_cobertura_integrados_borrador)
datos_cobertura_integrados_borrador %>%
  distinct(nombre_sitio, fecha_muestra_final) %>%
  nrow()
# Sí los hay, voy a revisar qué está pasando también

# Revisando cuáles muestreos están duplicados
datos_cobertura_integrados_borrador %>%
  group_by(nombre_sitio, fecha_muestra_final) %>%
  tally() %>%
  ungroup() %>%
  filter(n >= 2) %>%
  inner_join(datos_cobertura_integrados_borrador, by = c("nombre_sitio", "fecha_muestra_final")) %>%
  View()
# Es sólo un registro duplicado con fecha inicial incorrecta.

################################################################################
# 3. Creando el data frame final con los datos de cobertura integrados
################################################################################

# Componiendo manualmente los registros duplicados, tomando datos de ambos
# registros según convenga.

datos_cobertura_integrados <- rbind(
  # Agregando datos del Monitoreo PNAC 2005
  datos_cobertura_integrados_borrador %>%
    group_by(nombre_sitio, fecha_muestra_inicial) %>%
    tally() %>%
    ungroup() %>%
    filter(n >= 2) %>%
    inner_join(datos_cobertura_integrados_borrador, by = c("nombre_sitio", "fecha_muestra_inicial")) %>%
    # Arreglando a mano los campos principales
    group_by(nombre_sitio) %>%
    mutate(
      zona_arrecifal = ifelse(nombre_sitio == "yucab", NA_character_, "Frontal")
    ) %>%
    summarise(
      fecha_muestra_inicial = as_date("2005-05-01"),
      fecha_muestra_final = as_date("2005-07-20"),
      tipo_arrecife = "Barrera",
      zona_arrecifal = first(zona_arrecifal),
      latitud = mean(latitud),
      longitud = mean(longitud),
      muestreo_muestra_inicial = "Monitoreo PNAC 2005",
      muestreo_muestra_final = "Monitoreo PNAC 2005",
      tabla_muestra_inicial = "Muestra_transecto_bentos_punto",
      tabla_muestra_final = "Muestra_transecto_bentos_punto",
      porcentaje_cobertura_coral_muestra_inicial = mean(
        porcentaje_cobertura_coral_muestra_inicial),
      porcentaje_cobertura_coral_muestra_final = mean(
        porcentaje_cobertura_coral_muestra_final),
      fuente = "base de datos",
      datos_especificos_analisis_huracanes = TRUE,
      profundidad_media_m = mean(profundidad_media_m, na.rm = TRUE)
    ),
  datos_cobertura_integrados_borrador %>%
    # El complemento a los anteriores
    anti_join(datos_cobertura_integrados_borrador %>%
      group_by(nombre_sitio, fecha_muestra_inicial) %>%
      tally() %>%
      ungroup() %>%
      filter(n >= 2), by = c("nombre_sitio", "fecha_muestra_inicial"))
  ) %>%
  # Quitando el último registro que falta
  filter(!(nombre_sitio == "chankanaab" & fecha_muestra_inicial == as_date("2005-07-01"))) %>%
  mutate(
    id_remuestreo = 1:nrow(.)
  )
# Deben ser 1723 registros porque había 7 duplicados en total
nrow(datos_cobertura_integrados_borrador) - nrow(datos_cobertura_integrados)

################################################################################
# 3. Revisiones varias del data frame anterior
################################################################################

# Se realizarán las revisiones anteriores a ver si ahora sí las pasa
# Revisando que no haya registros de remuestreo duplicados usando la llave
# ("nombre_sitio", "fecha_muestra_inicial"):
nrow(datos_cobertura_integrados)
datos_cobertura_integrados %>%
  distinct(nombre_sitio, fecha_muestra_inicial) %>%
  nrow()
# Perfecto!

# Revisando que no haya registros de remuestreo duplicados usando la llave
# ("nombre_sitio", "fecha_muestra_final"):
nrow(datos_cobertura_integrados)
datos_cobertura_integrados %>%
  distinct(nombre_sitio, fecha_muestra_final) %>%
  nrow()
# Ya pasó las revisiones principales que sugieren que la base de datos está
# formada por puros muestreos adyacentes.

# Revisando campos individuales:
tablas_revision <- revisa_valores(datos_cobertura_integrados)
names(tablas_revision)

# Revisando las fuentes de datos de esta tabla
datos_cobertura_integrados %>%
  select(
    fuente,
    datos_especificos_analisis_huracanes
  ) %>%
  table(useNA = "always")

saveRDS(datos_cobertura_integrados,
  rutas_archivos_productos["datos_cobertura_integrados"])


