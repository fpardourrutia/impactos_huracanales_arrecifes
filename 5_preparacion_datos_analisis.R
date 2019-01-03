# En este script se calcularán los data frames para el análisis, que contendrán
# la siguiente información:

# Información a nivel de sitio:
# - "nombre_sitio"
# - "latitud_sitio"
# - "longitud_sitio"
# - "tipo_arrecife"
# - "zona_arrecifal"
# - "profundidad_media_m"
# - "exposicion"
# - "fetch_m" (si resolvemos como calcularlo en general) *
# - "tamanio_m" * p

# Información a nivel de remuestreos adyacentes de un sitio:
# - "id_remuestreo"
# - "fecha_muestra_inicial"
# - "fecha_muestra_final"
# - "porcentaje_cobertura_coral_muestra_inicial"
# - "porcentaje_cobertura_coral_muestra_final"
# - "complejidad_muestra_inicial" *
# - "complejidad_muestra_final" *
# - "fuente"
# - "datos_especificos_analisis_huracanes"
# - "muestreo_muestra_inicial"
# - "muestreo_muestra_final"
# - "tabla_muestra_inicial"
# - "tabla_muestra_final"
# - "sst_c" (y adicionales) * p

# Información a nivel de huracanes:
# - "llave_huracan"
# - "nombre_huracan"
# - "numero_snapshots_huracan"
# - "marejada_media_m" (posiblemente obtenerla de bases adicionales) *
# - "oleaje_m" (posiblemente obtenerla de bases adicionales) * p
# - "precipitacion" * p

# Información a nivel de snapshots de huracanes:
# - "id_snapshot"
# - "latitud_snapshot"
# - "longitud_snapshot"
# - "datum_snapshot"
# - "fecha_hora_snapshot"
# - "tipo_registro_snapshot"
# - "estado_sistema_snapshot"
# - "viento_sostenido_maximo_kt_snapshot"
# - "presion_central_mb_snapshot"
# - "radio_maximo_vientos_..."

# Información que depende de combinaciones entre los niveles anteriores:
# - "distancia_snapshot_sitio_m" (depende del sitio y snapshot del huracán)
# - "fetch_huracan_m" * (depende del sitio y del huracán) *

# *: falta.
# p: Importancia pendiente a definir en el futuro, por lo que está en standby si
# es necesario calcularla o no.

# En realidad, para el entregable final se considerarán las siguientes covariables:
# - Tipo de arrecife si sale algo al revisarla con Lorenzo
# - Zona arrecifal si sale algo al revidarla con Lorenzo
# - Exposición si sale algo al agregarla en 2 categorías.
# - Fetch (m) porque la piden los donantes
# - Marejada (m) poque la piden los donantes
# - Profundidad media (m) si sale algo
# - Viento sostenido máximo (kt) si sale algo
# - Presión central (mb) si sale algo
# - Tamaño del arrecife (m) si sale algo
# - Cobertura inicial (%)
# - Tiempo de exposición a vientos huracanados (h)
# - Velocidad del viento al impacto (kt), si es posible estimarla como continua.
#   Esta es la más importante (sustituye a las categorías del huracán).

library("plyr")
library("dplyr")
library("tidyr")
library("lubridate")
library("ggplot2")
library("forcats")
library("stringi")
source("0_config.R")

################################################################################
# 1. Leyendo data frame que contiene la información anterior
################################################################################

datos_remuestreos_sitio_snapshots_huracanes <- readRDS(
  rutas_archivos_productos["datos_remuestreos_sitio_snapshots_huracanes"])
glimpse(datos_remuestreos_sitio_snapshots_huracanes)
View(datos_remuestreos_sitio_snapshots_huracanes)

################################################################################
# 2. Revisiones previas antes de crear el data frame para el análisis
################################################################################

# Revisando cuántos remuestreos hay en total
datos_remuestreos_sitio_snapshots_huracanes %>%
  distinct(id_remuestreo) %>%
  nrow()
# Hay 836 remuestreos en total en la base.

# Revisando cuántos remuestreos hay con más de un huracán asociado:
datos_remuestreos_sitio_snapshots_huracanes %>%
  distinct(id_remuestreo, llave_huracan) %>%
  group_by(id_remuestreo) %>%
  tally() %>%
  ungroup() %>%
  filter(n >= 2) %>%
  nrow()
# Hay 357 remuestreos de sitio que tuvieron 2 o más huracanes asociados

################################################################################
# 4. Limpiando  y revisando"datos_remuestreos_sitio_snapshots_huracanes"
################################################################################

# Esta limpieza se realizará previamente al cálculo de otros data frames para
# facilitar este proceso. Cabe destacar que las columnas sucias son las de
# "radio_maximo..." y "exposicion".
datos_limpios_remuestreos_sitio_snapshots_huracanes <- datos_remuestreos_sitio_snapshots_huracanes %>%
  gather("variable", "radio_maximo_nm", contains("radio_maximo")) %>%
  mutate(
    cuadrante = stri_match_first_regex(variable, ".*_(\\w\\w)_nm")[,2],
    velocidad_viento_kt = stri_extract_first_regex(variable, "\\d\\d"),
    # Ordenando la "exposicion" por "orden_exposicion"
    exposicion = fct_reorder(exposicion, orden_exposicion)
  ) %>%
  # Reorganizando columnas (notar que se eliminarán "variable" y "orden_exposicion")
  select(
    id_remuestreo,
    nombre_sitio,
    latitud_sitio,
    longitud_sitio,
    fecha_muestra_inicial,
    fecha_muestra_final,
    porcentaje_cobertura_coral_muestra_inicial,
    porcentaje_cobertura_coral_muestra_final,
    fuente,
    datos_especificos_analisis_huracanes,
    tipo_arrecife,
    zona_arrecifal,
    profundidad_media_m,
    exposicion,
    muestreo_muestra_inicial,
    muestreo_muestra_final,
    tabla_muestra_inicial,
    tabla_muestra_final,
    llave_huracan,
    nombre_huracan,
    numero_snapshots_huracan,
    marejada_media_m_imputacion,
    id_snapshot,
    latitud_snapshot,
    longitud_snapshot,
    datum_snapshot,
    fecha_hora_snapshot,
    tipo_registro_snapshot,
    estado_sistema_snapshot,
    viento_sostenido_maximo_kt_snapshot,
    presion_central_mb_snapshot,
    cuadrante,
    velocidad_viento_kt,
    radio_maximo_nm,
    distancia_snapshot_sitio_m
  )
glimpse(datos_limpios_remuestreos_sitio_snapshots_huracanes)

# Notar que para "datos_limpios_remuestreos_sitio_snapshots_huracanes" cada
# registro equivale a una combinación de ("cuadrante", "velocidad_viento_kt")
# asociado a un snapshot de huracán que impactó a un sitio determinado.

# Revisando que los niveles de "exposicion" estén en el orden adecuado
datos_limpios_remuestreos_sitio_snapshots_huracanes %>%
  pull(exposicion) %>%
  levels()
saveRDS(datos_limpios_remuestreos_sitio_snapshots_huracanes,
  rutas_archivos_productos["datos_limpios_remuestreos_sitio_snapshots_huracanes"])

################################################################################
# 4. Creando un data frame para el análisis de cambios en porcentajes de
# cobertura de coral, para remuestreos con sólo 1 huracán asociado
################################################################################

datos_analisis_coberturas_un_huracan_asociado <- datos_limpios_remuestreos_sitio_snapshots_huracanes %>%
  
  # Filtrando remuestreos que tuvieron únicamente un huracán asociado
  distinct(id_remuestreo, llave_huracan) %>%
  group_by(id_remuestreo) %>%
  tally() %>%
  ungroup() %>%
  filter(n == 1) %>%
  select(-n) %>%
  inner_join(datos_limpios_remuestreos_sitio_snapshots_huracanes, by = "id_remuestreo") %>%
  
  # Transformaciones previas al análisis
  mutate(
    titulos_radio_maximo = paste0("radio_maximo_vientos_", velocidad_viento_kt, "_kt_nm"),
    distancia_snapshot_sitio_nm = distancia_snapshot_sitio_m / 1852
  ) %>%
  select(-velocidad_viento_kt) %>%
  
  # Transformando el data frame anterior a una forma que nos facilite estimar
  # la velocidad del viento que impactó a determinado sitio.
  spread(titulos_radio_maximo, radio_maximo_nm) %>%
  
  mutate(
    # Revisando en qué cuadrante del snapshot cae el sitio correspondiente
    cuadrante_sitio_eje_x = case_when(
      longitud_sitio >= longitud_snapshot ~ "e",
      longitud_sitio < longitud_snapshot ~ "w",
      TRUE ~ "*"
    ),
    
    cuadrante_sitio_eje_y = case_when(
      latitud_sitio >= latitud_snapshot ~ "n",
      latitud_sitio < latitud_snapshot ~ "s",
      TRUE ~ "*"
    ),
    
    cuadrante_sitio = paste0(cuadrante_sitio_eje_y, cuadrante_sitio_eje_x)
  ) %>%
  select(
    -cuadrante_sitio_eje_x,
    -cuadrante_sitio_eje_y
  ) %>%
  
  # Quedándonos sólo con los cuadrantes que nos interesan para cada combinación
  # de snapshot y sitio (sólo nos interesan los vientos que afectaron a un sitio
  # determinado).
  filter(cuadrante_sitio == cuadrante) %>%
  
  mutate(
    velocidad_viento_impacto_kt_cat = case_when(
      distancia_snapshot_sitio_nm <= radio_maximo_vientos_64_kt_nm ~ "Mayor a 64 kt",
      radio_maximo_vientos_64_kt_nm < distancia_snapshot_sitio_nm &
        distancia_snapshot_sitio_nm <= radio_maximo_vientos_50_kt_nm ~ "Entre 50 kt y 64 kt",
      radio_maximo_vientos_50_kt_nm < distancia_snapshot_sitio_nm &
        distancia_snapshot_sitio_nm <= radio_maximo_vientos_34_kt_nm ~ "Entre 34 kt y 50 kt",
      radio_maximo_vientos_34_kt_nm < distancia_snapshot_sitio_nm ~ "Menos de 34 kt",
      TRUE ~ NA_character_
    ) %>%
      # Se ordenan para poder sacar el máximo entre factores
      factor(levels = c(
        "Menos de 34 kt",
        "Entre 34 kt y 50 kt",
        "Entre 50 kt y 64 kt",
        "Mayor a 64 kt"), ordered = TRUE)
    # # Estimando la velocidad del viento al impacto con interpolación lineal:
    # velocidad_viento_impacto_kt_estimada = case_when(
    #   distancia_snapshot_sitio_nm <= radio_maximo_vientos_64_kt_nm ~
    #     viento_sostenido_maximo_kt_snapshot +
    #     (64 - viento_sostenido_maximo_kt_snapshot) *
    #     (distancia_snapshot_sitio_nm - radio_maximo_vientos_64_kt_nm) / 
    #     ((radio_maximo_vientos_64_kt_nm - distancia_snapshot_sitio_nm) * viento_sostenido_maximo_kt_snapshot +
    #     distancia_snapshot_sitio_nm * 64) / radio_maximo_vientos_64_kt_nm,
    #   radio_maximo_vientos_64_kt_nm < distancia_snapshot_sitio_nm &
    #     distancia_snapshot_sitio_nm <= radio_maximo_vientos_50_kt_nm ~
    #     (distancia_snapshot_sitio_nm - radio_maximo_vientos_64_kt_nm)
    # 
    # 
    # )
  ) %>%
  # Para seleccionar la mayor categoría de impacto por parejita de remuestreos.
  arrange(id_remuestreo, desc(velocidad_viento_impacto_kt_cat)) %>%
  
  group_by(id_remuestreo) %>%
  summarise(
    nombre_sitio = first(nombre_sitio),
    latitud_sitio = first(latitud_sitio),
    longitud_sitio = first(longitud_sitio),
    fecha_muestra_inicial = first(fecha_muestra_inicial),
    fecha_muestra_final = first(fecha_muestra_final),
    porcentaje_cobertura_coral_muestra_inicial = first(porcentaje_cobertura_coral_muestra_inicial),
    porcentaje_cobertura_coral_muestra_final = first(porcentaje_cobertura_coral_muestra_final),
    fuente = first(fuente),
    datos_especificos_analisis_huracanes = first(datos_especificos_analisis_huracanes),
    tipo_arrecife = first(tipo_arrecife),
    zona_arrecifal = first(zona_arrecifal),
    profundidad_media_m = first(profundidad_media_m),
    exposicion = first(exposicion),
    llave_huracan = first(llave_huracan),
    nombre_huracan = first(nombre_huracan),
    marejada_media_m_imputacion = first(marejada_media_m_imputacion),
    fecha_hora_llegada_huracan = min(fecha_hora_snapshot),
    fecha_hora_salida_huracan = max(fecha_hora_snapshot),
    maxima_velocidad_viento_impacto_kt_cat = first(velocidad_viento_impacto_kt_cat),
    
    # Calculando algunas variables nuevas
    viento_sostenido_maximo_medio_kt = weighted.mean(viento_sostenido_maximo_kt_snapshot,
      1/distancia_snapshot_sitio_m),
    presion_central_media_mb = weighted.mean(presion_central_mb_snapshot,
      1/distancia_snapshot_sitio_m),
    distancia_minima_sitio_m = min(distancia_snapshot_sitio_m)
  ) %>%
  
  # Generando variables adicionales
  mutate(
    categoria_huracan = cut(
      viento_sostenido_maximo_medio_kt,
      breaks = c(0, 34, 64, 83, 96, 114, 135, 1000),
      right = FALSE,
      labels = c("TD", "TS", "H1", "H2", "H3", "H4", "H5"),
      ordered_result = TRUE),
    
    # Otras variables de interés
    exposicion = case_when(
      stri_detect_fixed(exposicion, "Sotavento") ~ "Sotavento",
      stri_detect_fixed(exposicion, "Barlovento") ~ "Barlovento",
      TRUE ~ NA_character_
    ) %>%
      as_factor(),
    
    tiempo_exposicion_huracan_horas = as.integer(
      difftime(fecha_hora_salida_huracan, fecha_hora_llegada_huracan, units = "hours")),
    
    diferencia_muestra_final_salida_huracan_dias = as.integer(
      difftime(fecha_muestra_final, fecha_hora_salida_huracan, units = "days")),
    diferencia_muestra_final_salida_huracan_dias_cat = cut(
      diferencia_muestra_final_salida_huracan_dias,
      breaks = quantile(diferencia_muestra_final_salida_huracan_dias),
      right = FALSE, include.lowest = TRUE),
    
    diferencia_tiempo_remuestreos_dias = as.integer(
      difftime(fecha_muestra_final, fecha_muestra_inicial)),
    tasa_cambio_cobertura_coral = (log(porcentaje_cobertura_coral_muestra_final) -
        log(porcentaje_cobertura_coral_muestra_inicial)) / diferencia_tiempo_remuestreos_dias
  ) %>%
  select(
    -diferencia_tiempo_remuestreos_dias
  )
glimpse(datos_analisis_coberturas_un_huracan_asociado)
summary(datos_analisis_coberturas_un_huracan_asociado)

saveRDS(datos_analisis_coberturas_un_huracan_asociado,
  rutas_archivos_productos["datos_analisis_coberturas_un_huracan_asociado"])

# Faltan las revisiones pero esas se quedan para otra ocasión
# Análisis pendiente:
# - Comparar los perfiles de tasa de cambio de cobertura usando categoría del
#   huracán contra los obtenidos usando categoría de velocidad al impacto. Para
#   ver las mejoras
# - Ver qué pasa si filtro los datos donde el lapso del tiempo hasta el remuestreo
#   no sea muy grande.
# - Estimar velocidad del viento al impacto con una regresión... Buscar artículos.
# - Ajustar los modelos con las variables finales.
