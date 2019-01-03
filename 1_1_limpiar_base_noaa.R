# En este script se prepara la base de datos de huracanes de la NOAA para poder
# trabajar con ella.

# Características de la base de datos de huracanes de la NOAA:
# - La base contiene dos tipos de registros para cada huracán: hay un título y
# luego siguen los registros de cada una de sus mediciones (que llamaré snapshots
# o fotografías)
# - Todos los registros que corresponden al título empiezan con letra, y los que
# corresponden a fotografías, con número.

# Referencias:
# Base de datos: https://www.nhc.noaa.gov/data/#hurdat
# Metadatos: https://www.nhc.noaa.gov/data/hurdat/hurdat2-format-atlantic.pdf

library("plyr")
library("dplyr")
library("tidyr")
library("stringi")
library("lubridate")
library("readr")
source("0_config.R")

################################################################################
# 1. Leyendo los datos de la base de la NOAA
################################################################################

# Se usa read.csv() porque la base tiene un formato raro que no es compatible
# con read_csv()
datos_crudos <- read.csv(rutas_archivos_insumos["datos_crudos_snapshots_huracanes"],
  header = FALSE,
  stringsAsFactors = FALSE)
glimpse(datos_crudos)

################################################################################
# 2 Realizando transformaciones a los datos
################################################################################

# Obteniendo los registros que corresponden a títulos de huracanes.
titulos_huracanes <- datos_crudos %>%
  filter(stri_detect_regex(V1, "[:alpha:]")) %>%
  transmute(
    llave = V1,
    # Quitando los espacios que están de más en los nombres de los huracanes
    nombre = stri_replace_all_regex(V2, pattern = "[:blank:]", replacement = ""),
    numero_snapshots = as.numeric(V3)
  )

# Revisando que llave sea efectivamente una llave de "titulos huracanes"
nrow(titulos_huracanes) == titulos_huracanes %>%
  distinct(llave) %>% 
  nrow()

# Notando que a cada registro de título le siguen "V3" registros de información,
# se puede parsear la tabla fácilmente.
datos_limpios_snapshots_huracanes <- ldply(1:nrow(titulos_huracanes), function(i){
  
  # Obteniendo los datos correspondientes al i-ésimo huracán
  titulo_huracan <- titulos_huracanes %>%
    slice(i)
  
  # Obteniendo la llave del huracán, con el fin de buscarla en "datos_crudos"
  # y obtener el row name correspondiente a dicho huracán.
  llave_huracan <- titulo_huracan$llave
  
  # Ahora se obtiene el nombre del renglón del huracán con esta llave. Esto es
  # útil para usar la información en la columna de "numero_registros_datos_huracan".
  # Notar que si se usara un filter se perdería el número de renglón que es el
  # que nos interesa para usar la información en V3.
  
  numero_renglon_llave_datos_crudos <- datos_crudos[datos_crudos$V1 == llave_huracan,] %>%
    row.names() %>%
    as.numeric()
  
  # Calculando los índices del primer y último registro de datos correspondientes
  # a un huracán determinado
  primer_registro <- numero_renglon_llave_datos_crudos + 1
  ultimo_registro <- numero_renglon_llave_datos_crudos +
    titulo_huracan$numero_snapshots
  
  # Encontrando los registros correspondientes al huracán en cuestión
  datos_huracan <- datos_crudos[primer_registro:ultimo_registro,]
  
  # Eliminando row names para evitar warnings:
  rownames(titulo_huracan) <- NULL
  
  # Agregando la información de "título de huracán" a cada uno de los datos
  # correspondientes a snapshots del mismo
  resultado <- cbind(titulo_huracan, datos_huracan)
}) %>%
  
  # Generando un identificador de snapshot
  mutate(
    id_snapshot = 1:nrow(.)
  ) %>%
  
  # Generando la fecha y hora de impacto de los huracanes
  mutate(
    anio = stri_sub(V1, from = 1, to = 4),
    mes = stri_sub(V1, from = 5, to = 6),
    dia = stri_sub(V1, from = 7, to = 8),
    hora = stri_sub(V2, from = 2, to = 3),
    minutos = stri_sub(V2, from = 4, to = 5),
    fecha_hora = paste0(anio, "-", mes, "-", dia, "_", hora, ":", minutos) %>%
      ymd_hm(tz = "UTC")
  ) %>%
  select(
    -anio,
    -mes,
    -dia,
    -hora,
    -minutos
  ) %>%
  
  # Limpiando los campos de tipo de registro y estado del sistema al momento de
  # la snapshot
  mutate(
    tipo_registro = stri_replace_all_regex(V3, pattern = "[:blank:]", replacement = ""),
    tipo_registro = ifelse(tipo_registro == "", NA, tipo_registro),
    estado_sistema = stri_replace_all_regex(V4, pattern = "[:blank:]", replacement = "")
  ) %>%

  # Generando los campos de latitud y longitud
  mutate(
    direccion_latitud = stri_sub(V5, from = nchar(V5), to = nchar(V5)),
    latitud = ifelse(direccion_latitud == "N",
      as.numeric(stri_sub(V5, from = 1, to = nchar(V5) - 1)),
      -1 * as.numeric(stri_sub(V5, from = 1, to = nchar(V5) - 1))
    ),
    
    direccion_longitud = stri_sub(V6, from = nchar(V6), to = nchar(V6)),
    longitud = ifelse(direccion_longitud == "W",
      -1 * as.numeric(stri_sub(V6, from = 1, to = nchar(V6) - 1)),
      as.numeric(stri_sub(V6, from = 1, to = nchar(V6) - 1))
    ),
    datum = "WGS84"
  ) %>%
  select(
    -direccion_latitud,
    -direccion_longitud
  ) %>%
  
  # Nombrando y revisando las demás columnas
  mutate(
    viento_sostenido_maximo_kt = ifelse(as.numeric(V7) < 0, NA, as.numeric(V7)),
    presion_central_mb = ifelse(as.numeric(V8) < 0, NA, as.numeric(V8)),
    radio_maximo_vientos_34_kt_ne_nm = ifelse(as.numeric(V9) < 0, NA, as.numeric(V9)),
    radio_maximo_vientos_34_kt_se_nm = ifelse(as.numeric(V10) < 0, NA, as.numeric(V10)),
    radio_maximo_vientos_34_kt_sw_nm = ifelse(as.numeric(V11) < 0, NA, as.numeric(V11)),
    radio_maximo_vientos_34_kt_nw_nm = ifelse(as.numeric(V12) < 0, NA, as.numeric(V12)),
    
    radio_maximo_vientos_50_kt_ne_nm = ifelse(as.numeric(V13) < 0, NA, as.numeric(V13)),
    radio_maximo_vientos_50_kt_se_nm = ifelse(as.numeric(V14) < 0, NA, as.numeric(V14)),
    radio_maximo_vientos_50_kt_sw_nm = ifelse(as.numeric(V15) < 0, NA, as.numeric(V15)),
    radio_maximo_vientos_50_kt_nw_nm = ifelse(as.numeric(V16) < 0, NA, as.numeric(V16)),
    
    radio_maximo_vientos_64_kt_ne_nm = ifelse(as.numeric(V17) < 0, NA, as.numeric(V17)),
    radio_maximo_vientos_64_kt_se_nm = ifelse(as.numeric(V18) < 0, NA, as.numeric(V18)),
    radio_maximo_vientos_64_kt_sw_nm = ifelse(as.numeric(V19) < 0, NA, as.numeric(V19)),
    radio_maximo_vientos_64_kt_nw_nm = ifelse(as.numeric(V20) < 0, NA, as.numeric(V20))
  ) %>%
  
  # Seleccionando las columnas de interés en el órden adecuado
  select(
    id_snapshot,
    llave_huracan = llave,
    nombre_huracan = nombre,
    numero_snapshots_huracan = numero_snapshots,
    latitud,
    longitud,
    datum,
    fecha_hora,
    tipo_registro,
    estado_sistema,
    viento_sostenido_maximo_kt,
    presion_central_mb,
    starts_with("radio")
  )

glimpse(datos_limpios_snapshots_huracanes)
# Perfecto!

# La tabla de datos limpios se guarda en formato CSV para que Esme la pueda
# utilizar también
saveRDS(datos_limpios_snapshots_huracanes, rutas_archivos_productos[["datos_limpios_snapshots_huracanes"]])