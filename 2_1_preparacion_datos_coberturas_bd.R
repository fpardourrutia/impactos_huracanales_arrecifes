# En este script se preparan los datos de remuestreos de sitio provenientes de
# la base de datos para que adopten la forma:
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

# Es decir, queremos que cada registro de la nueva tabla corresponda a información
# de porcentaje de cobertura de dos muestreos adyacentes sobre el mismo sitio.
# (Obviamente se eliminarán los muestreos correspondientes a sitios no remuestreados).

library("plyr")
library("dplyr")
library("tidyr")
library("lubridate")
source("0_config.R")

################################################################################
# 1. Obteniendo los datos apropiados de la base de datos
################################################################################

conexion_barco_db <- src_sqlite(rutas_archivos_insumos["barco_db"])
src_tbls(conexion_barco_db)

Vista_porcentaje_coberturas_bentos_sitio_nivel_3 <- conexion_barco_db %>%
  tbl("Vista_porcentaje_coberturas_bentos_sitio_nivel_3") %>%
  collect() %>%
  select(
    muestreo,
    muestra_sitio_id,
    nombre_sitio,
    fecha,
    pais,
    tipo_arrecife,
    zona_arrecifal,
    profundidad_m,
    latitud,
    longitud,
    datum,
    tabla,
    porcentaje_cobertura_coral
  )
glimpse(Vista_porcentaje_coberturas_bentos_sitio_nivel_3)

################################################################################
# 2. Construyendo y guardando el data frame especificado con anterioridad
################################################################################

remuestreos_porcentajes_cobertura_base <- Vista_porcentaje_coberturas_bentos_sitio_nivel_3 %>%
  
  # Primero se encontrarán los sitios con remuestreos
  group_by(nombre_sitio) %>%
  tally() %>%
  filter(n > 1) %>%
  
  # Teniendo los sitios con remuestreos, y suponiendo que no hay muestreos de
  # sitio duplicados en la base de datos, se procederán a definir intervalos entre
  # dos muestras adyacentes para cada sitio.
  
  inner_join(Vista_porcentaje_coberturas_bentos_sitio_nivel_3, by = "nombre_sitio") %>%
  mutate(
    fecha = ymd(fecha)
  ) %>%
  
  # Definiendo la latitud y longitud de cada sitio, recordando que se supone
  # que sitios con el mismo nombre son el mismo.
  
  group_by(nombre_sitio) %>%
  mutate(
    pais = first(pais),
    tipo_arrecife = first(tipo_arrecife),
    zona_arrecifal = first(zona_arrecifal),
    longitud = first(longitud),
    latitud = first(latitud)
  ) %>%
  ungroup() %>%
  
  # Eliminando muestreos realizados en sitios en los que no se tienen la latitud
  # o la longitud
  
  filter(!is.na(latitud), !is.na(longitud)) %>%
  
  # Ordenando los muestreos de sitio por nombre y fecha para poder definir
  # remuestreos adyacentes fácilmente.
  arrange(
    nombre_sitio,
    fecha
  ) %>%
  
  # Definiendo inicios y fines de intervalos de muestreo mediante dichas fechas
  # adyacentes. Se eliminan las columnas innecesarias, sabiendo que siempre
  # se pueden recuperar con ayuda de los id's.
  transmute(
    nombre_sitio = nombre_sitio,
    pais = pais,
    tipo_arrecife = tipo_arrecife,
    zona_arrecifal = zona_arrecifal,
    latitud = latitud,
    longitud = longitud,
    numero_muestreos = n,
    fecha_muestra_inicial = fecha,
    fecha_muestra_final = lead(fecha, 1),
    id_muestra_inicial = muestra_sitio_id,
    id_muestra_final = lead(muestra_sitio_id, 1),
    muestreo_muestra_inicial = muestreo,
    muestreo_muestra_final = lead(muestreo, 1),
    tabla_muestra_inicial = tabla,
    tabla_muestra_final = lead(tabla, 1),
    profundidad_m_muestra_inicial = profundidad_m,
    profundidad_m_muestra_final = lead(profundidad_m, 1),
    porcentaje_cobertura_coral_muestra_inicial = porcentaje_cobertura_coral,
    porcentaje_cobertura_coral_muestra_final = lead(porcentaje_cobertura_coral, 1)
    
  ) %>%
  
  # Para cada nombre, se eliminará el último registro. Esto debido a que
  # n fechas definen n-1 intervalos de tiempo entre ellas.
  
  ddply(.(nombre_sitio), function(df){
    numero_muestreos <- unique(df$numero_muestreos)
    
    resultado <- df %>%
      slice(-numero_muestreos)
    
    return(resultado)
  }) %>%
  
  # Finalmente, se filtran los registros donde "fecha_muestreo_inicial" es igual
  # a "fecha_muestreo_final", pues dan problemas a la hora de definir intervalos
  # de tiempo
  filter(fecha_muestra_inicial != fecha_muestra_final) %>%
  mutate(
    # Variables informativas
    fuente = "base de datos",
    # Aquí hay unas cuantas excepciones que en scripts subsecuentes se eliminarán
    datos_especificos_analisis_huracanes = FALSE
  )

glimpse(remuestreos_porcentajes_cobertura_base)
saveRDS(remuestreos_porcentajes_cobertura_base,
  rutas_archivos_productos["datos_cobertura_bd"])
