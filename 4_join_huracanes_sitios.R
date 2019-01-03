# Este script consiste en un conjunto de funciones que permiten realizar el
# siguiente procedimiento:

# Supongamos que se tienen:
# 1. Snapshots de huracanes, cada uno tomado en un espacio y un tiempo
# determinado (estos snapshots corresponden a los datos de huracanes con los que
# cuenta la base de la NOAA)
# 2. Dado un sitio geográfico, se tiene un buffer espacial alrededor de el, y un
# buffer temporal entre dos de sus muestreos

# Se quiere:
# 1. Encontrar los snapshots de huracanes que caen en ambos buffers
# (intersección de conjuntos).
# 2. De entre todos los snapshots seleccionados de un mismo huracán, encontrar
# el más cercano al sitio.

# Referencias:
# Base de datos: https://www.nhc.noaa.gov/data/#hurdat
# Metadatos: https://www.nhc.noaa.gov/data/hurdat/hurdat2-format-atlantic.pdf

library("plyr")
library("dplyr")
library("tidyr")
library("readr")
library("readxl")
library("stringi")
library("lubridate")
library("geosphere") # Obtener distancias geográficas con la función "distGeo()"
library("ggplot2")
source("0_config.R")

# Cabe destacar que cuando se realice el análisis por complejidad arrecifal, los
# datos de remuestreos de sitio corresponderán a la integración de los de
# complejidad y cobertura.

################################################################################
# 1. Leyendo los data frames de remuestreos de sitio y de snapshots de huracanes
################################################################################

datos_snapshots_huracanes <- readRDS(
  rutas_archivos_productos["datos_limpios_snapshots_huracanes_marejada"])
glimpse(datos_snapshots_huracanes)
summary(datos_snapshots_huracanes)

datos_remuestreos_sitio <- readRDS(
  rutas_archivos_productos["datos_cobertura_integrados_variables_adicionales_no_fetch"])
glimpse(datos_remuestreos_sitio)
summary(datos_remuestreos_sitio)

################################################################################
# 2. Creando una función que permita obtener los huracanes que están a cierta
# distancia del sitio de interés
################################################################################

# La siguiente función permite encontrar, dado un buffer espacial alrededor de
# un sitio, y un intervalo de tiempo definido por dos fechas de muestro, todos
# los snapshots que caen a esa distancia espacio temporal del sitio. 

# Parámetros:
# longitud_sitio / latitud_sitio: coordenadas del sitio en WGS84.
# fecha_hora_muestreo_inicial / fecha_hora_muestreo_final: objetos de tipo datetime con
# zona horaria UTC.
# radio_buffer_m: radio del buffer en metros.
# datos_snapshots_huracanes: tabla cuyos registros son snapshots de huracanes. Debe tener
# las columnas "latitud" / "longitud (double), "fecha_hora" (datetime),  y "id_snapshot"

# Nota: es importante notar que esta función no es vectorizada, por lo que si se
# desea repetir la operación para n sitios distintos, se deberá invocar n veces.

encuentra_snapshots_cercanos_sitio <- function(longitud_sitio, latitud_sitio,
  fecha_hora_muestreo_inicial, fecha_hora_muestreo_final, radio_buffer_m,
  datos_snapshots_huracanes){
  
  intervalo_tiempo_muestreos <- interval(fecha_hora_muestreo_inicial,
    fecha_hora_muestreo_final)
  
  # Generando el resultado que corresponde al conjunto de snapshots en 
  # "datos_snapshots_huracanes" que caen dentro del buffer espacial del sitio e intervalo
  # de tiempo entre muestreos. Por eficiencia, primero se encontrarán los
  # snapshots que caen en el intervalo de tiempo especificado, y después se
  # seleccionarán los que están cercanos al
  # sitio.
  
  resultado_filtro_temporal <- datos_snapshots_huracanes %>%
    filter(fecha_hora %within% intervalo_tiempo_muestreos)
  
  if(nrow(resultado_filtro_temporal) > 0){
    
    resultado <- resultado_filtro_temporal %>%
    
    # Calculando la distancia geográfica entre el sitio de interés y cada uno
    # de los snapshots que cayeron dentro del intervalo deseado
    mutate(
      # La función distGeo() puede aceptar data frames del mismo tamaño como input
      # y calcular distancias entre los i-ésimos renglones de cada uno. O bien,
      # puede recibir un data frame y un único vector y calcular la distancia
      # entre cada elemento del data frame y el vector
      distancia_sitio_m = distGeo(
        data_frame("longitud" = longitud, "latitud" = latitud),
        c(longitud_sitio, latitud_sitio)
      )
    ) %>%
      
      # Finalmente, se eliminan los snapshots que disten del sitio algo mayor que
      # el buffer
      filter(distancia_sitio_m <= radio_buffer_m)
    
  } else{
    
    resultado <- resultado_filtro_temporal
  }
  
  return(resultado)
}

################################################################################
# 3. Encontrando snapshots de huracanes que se encuentren temporalmente entre dos
# muestreos del mismo sitio, y espacialmente caigan dentro de un buffer de 100Km
# del mismo
################################################################################

# Para cada registro del data frame anterior, se encontrarán snapshots de
# huracanes que caigan en el intervalo especificado y que caigan en el buffer
# definido.
radio_buffer_m <- 100000
datos_snapshots_huracanes <- datos_snapshots_huracanes

datos_remuestreos_sitio_snapshots_huracanes <- datos_remuestreos_sitio %>%
  
  apply(1, function(registro){
    #print(registro)
    resultado <- encuentra_snapshots_cercanos_sitio(
      longitud_sitio = registro["longitud"],
      latitud_sitio = registro["latitud"],
      fecha_hora_muestreo_inicial = ymd(registro["fecha_muestra_inicial"]),
      fecha_hora_muestreo_final = ymd(registro["fecha_muestra_final"]),
      radio_buffer_m = radio_buffer_m,
      datos_snapshots_huracanes = datos_snapshots_huracanes
    )
    
    if(nrow(resultado) > 0){
      resultado <- resultado %>%
        mutate(
          id_remuestreo = as.numeric(registro["id_remuestreo"])
        )
    }
    return(resultado)
  }) %>%
  Reduce(rbind.fill, .) %>%
  rename(
    latitud_snapshot = latitud,
    longitud_snapshot = longitud
  ) %>%
  inner_join(datos_remuestreos_sitio %>%
    rename(
      latitud_sitio = latitud,
      longitud_sitio = longitud
    ), by = c("id_remuestreo")) %>%
  # Ordenando las columnas
  select(
    id_remuestreo,
    nombre_sitio,
    latitud_sitio,
    longitud_sitio,
    fecha_muestra_inicial,
    fecha_muestra_final,
    porcentaje_cobertura_coral_muestra_final,
    porcentaje_cobertura_coral_muestra_inicial,
    
    fuente,
    datos_especificos_analisis_huracanes,
    
    tipo_arrecife,
    zona_arrecifal,
    profundidad_media_m,
    exposicion,
    orden_exposicion,
    
    muestreo_muestra_inicial,
    muestreo_muestra_final,
    tabla_muestra_inicial,
    tabla_muestra_final,
    muestreo_muestra_inicial,
    muestreo_muestra_final,
    
    llave_huracan,
    nombre_huracan,
    numero_snapshots_huracan,
    marejada_media_m_imputacion = marejada_tormenta_media_m_imputacion,
    
    id_snapshot,
    latitud_snapshot,
    longitud_snapshot,
    datum_snapshot = datum,
    fecha_hora_snapshot = fecha_hora,
    tipo_registro_snapshot = tipo_registro,
    estado_sistema_snapshot = estado_sistema,
    viento_sostenido_maximo_kt_snapshot = viento_sostenido_maximo_kt,
    presion_central_mb_snapshot = presion_central_mb,
    contains("radio"),
    distancia_snapshot_sitio_m = distancia_sitio_m
    )
glimpse(datos_remuestreos_sitio_snapshots_huracanes)
summary(datos_remuestreos_sitio_snapshots_huracanes)
View(datos_remuestreos_sitio_snapshots_huracanes)

saveRDS(datos_remuestreos_sitio_snapshots_huracanes,
  rutas_archivos_productos["datos_remuestreos_sitio_snapshots_huracanes"])

#####r##########################################################################
# 4. Revisiones finales antes de crear la tabla para el análisis.
################################################################################

# Revisando si los radios máximos se miden desde el ojo del huracán o desde que
# comienza el siguiente.
datos_snapshots_huracanes %>%
  gather("variable", "radio_maximo_nm", contains("radio")) %>%
  mutate(
    cuadrante = stri_match_first_regex(variable, ".*_(\\w\\w)_nm")[,2],
    velocidad_viento_kt = stri_extract_first_regex(variable, "\\d\\d")
  ) %>%
  ggplot(aes(x = velocidad_viento_kt, y = radio_maximo_nm,
    group = id_snapshot)) +
  geom_line(alpha = 0.02) +
  facet_wrap(~cuadrante)
# Perfecto, es lo que esperaba: los radios disminuyen a mayor velocidad del
# viento (están anidados).

# Revisando el significado de los 0's en las variables de "radio_maximo..."
datos_snapshots_huracanes %>%
  select(
    id_snapshot,
    viento_sostenido_maximo_kt,
    contains("radio")
  ) %>%
  gather("variable", "radio_maximo_nm", contains("radio")) %>% # nm = milla náutica
  # Calculando para cada snapshot de huracán el radio máximo con vientos de
  # x kt (x = 34, 50, 64), independientemente de su cuadrante
  mutate(
    velocidad_viento_kt = stri_extract_first_regex(variable, "\\d\\d")
  ) %>%
  group_by(id_snapshot, velocidad_viento_kt) %>%
  summarise(
    viento_sostenido_maximo_kt = first(viento_sostenido_maximo_kt),
    radio_maximo_nm = max(radio_maximo_nm, na.rm = TRUE)
    # Los valores de -Inf son aquellos donde se calcular el mínimo de un vector
    # de longitud 0.
  ) %>%
  ggplot(aes(
    x = viento_sostenido_maximo_kt,
    y = radio_maximo_nm,
    group = velocidad_viento_kt, colour = velocidad_viento_kt)) +
  geom_jitter() +
  facet_wrap(~velocidad_viento_kt)
# Súper bien! Esta gráfica nos enseña que los 0's son efectivamente ceros y no
# errores, por lo que los datos están limpios en ese aspecto (lo que marcan es
# lo que se supone que deben de marcar.)

# Revisando que en todos los sitios en los que debieran haberse encontrado
# huracanes, efectivamente así haya sido:
datos_remuestreos_sitio_snapshots_huracanes %>%
  filter(datos_especificos_analisis_huracanes) %>%
  pull(id_remuestreo) %>%
  unique() %>%
  length()

read_excel(
  rutas_archivos_insumos[
    "remuestreos_porcentajes_cobertura_datos_especiales_huracanes"]) %>%
  nrow()
# Hubo 5 parejas de remuestreos que se supone que tenían huracanes asociados
# pero estos no se detectaron. Puede ser porque los huracanes pasaron un poco
# lejos.