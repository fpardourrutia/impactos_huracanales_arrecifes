# En este script se agrega la variable de "marejada de tormenta", y, en caso de
# que aporte información importante, se agregará también la variable de "marea
# de tormenta".
# Cabe destacar que estas variables fueron tomadas en diferentes localizaciones,
# y con muy pocos registros por huracán, por lo que es posible que sean muy
# ruidosas y poco útiles.
# Las variables adicionales que se consideran son las siguientes:
# 1. Storm surge (m): marejada de tormenta. Elevación adicional del nivel del
# mar causada por una tormenta. obtenerlo de "globalpeaksurgedb.csv"
# 2. Storm tide (m): marea de tormenta. Es la marejada de tormenta + marea usual.
# 3. Oleaje (m): altura media de las olas, en nuestro caso durante un huracán.
# Obtenerlo de "globalpeaksurgedb.csv" No hay suficientes datos.

# Referencias:
# globalpeaksurgedb: http://surge.srcc.lsu.edu/data.html

library("plyr")
library("dplyr")
library("tidyr")
library("stringi")
library("lubridate")
library("readr")
library("ggplot2")
source("0_config.R")
source("funciones_auxiliares.R")

################################################################################
# 1. Leyendo los datos de interés para este análisis
################################################################################

datos_limpios_snapshots_huracanes <- readRDS(rutas_archivos_productos["datos_limpios_snapshots_huracanes"])
glimpse(datos_limpios_snapshots_huracanes)

global_peak_surge_db <- read_csv(rutas_archivos_insumos["global_peak_surge_db"]) %>%
  mutate(
    surge_m = as.numeric(surge_m)
  )
glimpse(global_peak_surge_db)
summary(global_peak_surge_db)
View(global_peak_surge_db)

global_peak_surge_db_paises_interes <- read_csv(
  rutas_archivos_insumos["global_peak_surge_db_paises_interes"]) %>%
  filter(
    pais_de_interes == "SI"
  ) %>%
  select(
    -pais_de_interes
  )

################################################################################
# 2. Revisiones varias de la "globalpeaksurgedb"
################################################################################

tablas_revision_global_peak_surge_db <- revisa_valores(global_peak_surge_db)
l_ply(1:length(tablas_revision_global_peak_surge_db), function(i){
  print(names(tablas_revision_global_peak_surge_db)[i])
  print(class(global_peak_surge_db[[names(tablas_revision_global_peak_surge_db)[i]]]))
  View(tablas_revision_global_peak_surge_db[[i]])
  readline(prompt="Presionar [enter] para continuar")
})
# Columnas de interés: "confidence", "lat", "lon", "storm_dates", "storm_id",
# "storm_name", "storm_tide_ft", "storm_tide_m", "surge_ft", "surge_id", "surge_m",
# "year". También "country" y "estate" porque los datos están tan sucios que sólo
# se arreglarán los registros correspondientes a los países de interés.

# Revisando datos que sólo tienen storm surge y storm tide en una unidad, ya sea
# pies o metros.
global_peak_surge_db %>%
  mutate(
    hay_storm_tide_ft = !is.na(storm_tide_ft),
    hay_storm_tide_m = !is.na(storm_tide_m)
  ) %>%
  select(hay_storm_tide_ft, hay_storm_tide_m) %>%
  table(useNA = "ifany")
# Hay renglones que sólo tienen una o la otra, por lo que habrá que agrupar
# ambas columnas en una sola.

global_peak_surge_db %>%
  mutate(
    hay_surge_ft = !is.na(surge_ft),
    hay_surge_m= !is.na(surge_m)
  ) %>%
  select(hay_surge_ft, hay_surge_m) %>%
  table(useNA = "ifany")
# Hay renglones que sólo tienen una o la otra, por lo que habrá que agrupar
# ambas columnas en una sola.

# Revisando si alguna de "storm_id" o "surge_id" es una llave numérica para la
# anterior tabla:

global_peak_surge_db %>%
  nrow()

global_peak_surge_db %>%
  distinct(storm_id) %>%
  nrow()

global_peak_surge_db %>%
  distinct(surge_id) %>%
  nrow()
# No son, existen varios registros que pertenecen a la misma tormenta o mismo
# surge. Revisándolos:

global_peak_surge_db %>%
  group_by(storm_id) %>%
  tally() %>%
  filter(n >= 2) %>%
  ungroup() %>%
  inner_join(global_peak_surge_db, by = "storm_id") %>%
  arrange(
    storm_id
  ) %>%
  View()

global_peak_surge_db %>%
  group_by(surge_id) %>%
  tally() %>%
  filter(n >= 2) %>%
  ungroup() %>%
  inner_join(global_peak_surge_db, by = "surge_id") %>%
  arrange(
    surge_id
  ) %>%
  View()
# No me hacen mucho sentido esas variables, además de que tienen NA muchos
# registros... mejor hacer el join con la tabla de la NOAA y eliminar artefactos
# del join por registros duplicados si ocurren.

# Revisando que los valores de "storm_tide" y "surge" coincidan para los
# registros que las reportan tanto en pies como en metros

global_peak_surge_db %>%
  filter(!is.na(surge_m), !is.na(surge_ft)) %>%
  mutate(
    porcentaje_error = (abs(surge_m - surge_ft / 3.28) / surge_m) * 100
  ) %>%
  pull(porcentaje_error) %>%
  quantile(probs = seq(0, 1, by = 0.01))
  
global_peak_surge_db %>%
  filter(!is.na(storm_tide_m), !is.na(storm_tide_ft)) %>%
  mutate(
    porcentaje_error = (abs(storm_tide_m - storm_tide_ft / 3.28) / storm_tide_m) * 100
  ) %>%
  pull(porcentaje_error) %>%
  quantile(probs = seq(0, 1, by = 0.01))
# No se ve nada mal, yo creo que para evitar complicaciones lo dejamos así.

################################################################################
# 3. Generando la primera versión del data frame con las variables adicionales
# mencionadas
################################################################################

datos_marea_marejada_borrador <- global_peak_surge_db %>%
  mutate(
    anio = year,
    nombre_huracan = estandariza_strings(storm_name),
    # Quitando de una vez espacios en las fechas sucias
    fechas_sucias_huracan = storm_dates %>%
      stri_replace_all_regex(pattern = "\\h+", replacement = "") %>%
      tolower() %>%
      stri_replace_all_regex(pattern = "sept", replacement = "sep"),
    clave_pais = country, # Va a servir para limpiar únicamente los datos de interés,
    clave_estado_eua = state,
    latitud = lat,
    longitud = lon,
    marejada_tormenta_m = ifelse(!is.na(surge_m), surge_m, surge_ft / 3.28),
    marea_tormenta_m = ifelse(!is.na(storm_tide_m), storm_tide_m, storm_tide_ft / 3.28),
    confianza = confidence
  ) %>%
  inner_join(global_peak_surge_db_paises_interes, by = "clave_pais") %>%
  # De entre los estados de EUA, nos interesa únicamente Florida
  filter(is.na(clave_estado_eua) | stri_detect_fixed(clave_estado_eua, "FL")) %>%
  
  mutate(
    # Componiendo las fechas, porque están en diversos_formatos
    fechas_limpias_inicio_huracan = case_when(
      # Formato tipo 26-sep
      stri_detect_regex(fechas_sucias_huracan, "^\\d\\d-\\w\\w\\w$") ~
        paste0(fechas_sucias_huracan, "-", anio),
      
      # Formato tipo 10/13-10/19
      stri_detect_regex(fechas_sucias_huracan, "^\\d{1,2}/\\d{1,2}-\\d{1,2}/\\d{1,2}") ~
        stri_extract_first_regex(fechas_sucias_huracan, "\\d{1,2}/\\d{1,2}") %>%
        paste0(anio, "/", .),
      
      #  Formato tipo oct8-12
      stri_detect_regex(fechas_sucias_huracan, "\\w\\w\\w\\d{1,2}-\\d{1,2}") ~
        paste0(
          # Día
          stri_extract_first_regex(fechas_sucias_huracan, "\\d{1,2}-"),
          # Mes
          stri_extract_first_regex(fechas_sucias_huracan, "\\w\\w\\w"), "-",
          # Anio
          anio
        ),
      
      # Formato tipo jun30-jul7
      stri_detect_regex(fechas_sucias_huracan, "\\w\\w\\w\\d{1,2}-\\w\\w\\w\\d{1,2}") ~
        paste0(
          # Día
          stri_extract_first_regex(fechas_sucias_huracan, "\\d{1,2}-"),
          # Mes
          stri_extract_first_regex(fechas_sucias_huracan, "\\w\\w\\w"), "-",
          # Anio
          anio
        )
      ) %>%
      # Transformando al formato estándar de fecha
      parse_date_time(c("dmy", "ymd")),
    
    fechas_limpias_fin_huracan = case_when(
      # Formato tipo 26-sep
      stri_detect_regex(fechas_sucias_huracan, "^\\d\\d-\\w\\w\\w$") ~
        paste0(fechas_sucias_huracan, "-", anio),
      
      # Formato tipo 10/13-10/19
      stri_detect_regex(fechas_sucias_huracan, "^\\d{1,2}/\\d{1,2}-\\d{1,2}/\\d{1,2}") ~
        stri_extract_last_regex(fechas_sucias_huracan, "\\d{1,2}/\\d{1,2}") %>%
        paste0(anio, "/", .),
      
      #  Formato tipo oct8-12
      stri_detect_regex(fechas_sucias_huracan, "\\w\\w\\w\\d{1,2}-\\d{1,2}") ~
        paste0(
          # Día (recordar que los matches de stringi son greedy por default)
          stri_extract_last_regex(fechas_sucias_huracan, "\\d{1,2}"), "-",
          # Mes
          stri_extract_first_regex(fechas_sucias_huracan, "\\w\\w\\w"), "-",
          # Anio
          anio
        ),
      
      # Formato tipo jun30-jul7
      stri_detect_regex(fechas_sucias_huracan, "\\w\\w\\w\\d{1,2}-\\w\\w\\w\\d{1,2}") ~
        paste0(
          # Día
          stri_extract_last_regex(fechas_sucias_huracan, "\\d{1,2}"), "-",
          # Mes
          stri_extract_last_regex(fechas_sucias_huracan, "\\w\\w\\w"), "-",
          # Anio
          anio
        )
      ) %>%
      # Transformando al formato adecuado
      parse_date_time(c("dmy", "ymd"))
    )

glimpse(datos_marea_marejada_borrador)
View(datos_marea_marejada_borrador)

################################################################################
# 4. Revisiones varias
################################################################################

# Revisando que las fechas se hayan reformateado apropiadamente:
datos_marea_marejada_borrador %>%
  select(
    fechas_sucias_huracan,
    anio,
    fechas_limpias_inicio_huracan,
    fechas_limpias_fin_huracan) %>%
  View()
# Perfecto!


# Revisando que no hayamos perdido datos al hacer el join
nrow(global_peak_surge_db_paises_interes)
datos_marea_marejada_borrador %>%
  distinct(clave_pais) %>%
  nrow()
# Perfecto, coinciden!

# Tabla de contingencia para valores de marea de tormenta y marejada de tormenta
datos_marea_marejada_borrador %>%
  transmute(
    hay_marea_tormenta_m = !is.na(marea_tormenta_m),
    hay_marejada_tormenta_m = !is.na(marejada_tormenta_m)
  ) %>%
  table(useNA = "ifany")
# Sólo hay 22 datos en los que se comparten ambas variables.

# Haciendo un plot de "marea_tormenta_m" y "marejada_tormenta_m" para ver la
# correlación entre ambas variables
ggplot(datos_marea_marejada_borrador, aes(x = marea_tormenta_m, y = marejada_tormenta_m)) +
  geom_point()

# Revisando la correlación entre ambas variables:
datos_marea_marejada_borrador %>%
  select(
    marea_tormenta_m,
    marejada_tormenta_m
  ) %>%
  filter(!is.na(marea_tormenta_m), !is.na(marejada_tormenta_m)) %>%
  cor()
# Altísima correlación, veamos si hay más datos qué pasa.

# Revisando la correlación entre ambas variables utilizando todos los datos
# existentes:
global_peak_surge_db %>%
  transmute(
    marejada_tormenta_m = ifelse(!is.na(surge_m), surge_m, surge_ft / 3.28),
    marea_tormenta_m = ifelse(!is.na(storm_tide_m), storm_tide_m, storm_tide_ft / 3.28)
  ) %>%
  filter(!is.na(marea_tormenta_m), !is.na(marejada_tormenta_m)) %>%
  cor()
# Disminuye, posiblemente debido a diferencias en los comportamientos entre
# distintas masas de agua.

# Realizando el plot
global_peak_surge_db %>%
  transmute(
    marejada_tormenta_m = ifelse(!is.na(surge_m), surge_m, surge_ft / 3.28),
    marea_tormenta_m = ifelse(!is.na(storm_tide_m), storm_tide_m, storm_tide_ft / 3.28)
  ) %>%
  filter(!is.na(marea_tormenta_m), !is.na(marejada_tormenta_m)) %>%
  ggplot(aes(x = marea_tormenta_m, y = marejada_tormenta_m)) +
  geom_point() # Claramente se ve que se puerde fuerza en la correlación

# En vista de los anteriores resultados, decidí hacer una imputación de los
# valores de "marejada_tormenta_m" a partir de los valores de "marea_tormenta_m"
# únicamente para la región del Caribe. Obviamente los resultados indican que
# la variable "marea_tormenta_m" no aporta información nueva y por lo tanto será
# eliminada del data frame que se generará al final de este script.

# Revisando las localizaciones geográficas de los datos con ambos valores para
# ver si constituyen una muestra representativa del total de datos a imputar
datos_marea_marejada_borrador %>%
  mutate(
    datos_entrenamiento = ifelse(
      !is.na(marejada_tormenta_m) & !is.na(marea_tormenta_m),
      TRUE, FALSE)
    ) %>%
  ggplot(aes(x = longitud, y = latitud, colour = datos_entrenamiento)) +
  geom_point()

# Hay dos datos que están más separados, por lo que podemos ver dónde se sitúan
# en el diagrama de dispersión
datos_marea_marejada_borrador %>%
  mutate(
    datos_separados = ifelse(latitud < 10, TRUE, FALSE)
  ) %>%
  ggplot(aes(x = marea_tormenta_m, y = marejada_tormenta_m, colour = datos_separados)) +
  geom_point() +
  geom_smooth(method = "lm")
# Nada mal, nada mal! Esto justifica perfectamente nuestro uso de la imputación.

# Revisando los QQ plots de la distribución del predictor "marea_tormenta_m"
# para datos con "marejada_tormenta_m" faltante y no faltante, con el fin de ver
# si las distribuciones son las mismas
qqplot(
  datos_marea_marejada_borrador %>%
    filter(!is.na(marejada_tormenta_m), !is.na(marea_tormenta_m)) %>%
    pull(marea_tormenta_m),
  datos_marea_marejada_borrador %>%
    filter(is.na(marejada_tormenta_m), !is.na(marea_tormenta_m)) %>%
    pull(marea_tormenta_m),
  xlab = "Datos completos", ylab = "Datos incompletos"
  )
# Se ve una recta razonable, por lo que es razonable pensar que las distribuciones
# son las mismas, lo que va de acuerdo con la posibilidad de imputación.

# Comparando QQplot de "marea_tormenta_m" que tienen datos de "marejada_tormenta_m"
# contra los de una normal canónica se ve que el supuesto de normalidad es razonable.
qqnorm(datos_marea_marejada_borrador %>%
    filter(!is.na(marejada_tormenta_m), !is.na(marea_tormenta_m)) %>%
    pull(marea_tormenta_m))
qqline(datos_marea_marejada_borrador %>%
    filter(!is.na(marejada_tormenta_m), !is.na(marea_tormenta_m)) %>%
    pull(marea_tormenta_m))

# Comparando QQplot de "marea_tormenta_m" que tienen no
# datos de "marejada_tormenta_m" contra los de una normal canónica se ve que el
# supuesto de normalidad también es razonable.
qqnorm(datos_marea_marejada_borrador %>%
    filter(is.na(marejada_tormenta_m), !is.na(marea_tormenta_m)) %>%
    pull(marea_tormenta_m))
qqline(datos_marea_marejada_borrador %>%
    filter(is.na(marejada_tormenta_m), !is.na(marea_tormenta_m)) %>%
    pull(marea_tormenta_m))

# Lo anterior sugiere que agregar error aleatorio normal a la imputación podría
# ser razonable. No obstante, prefiero usar el estimador puntual para no meter
# más errores al hacer Machine Learning.

################################################################################
# 5. Ajustando el modelo para imputar los datos de "marejada_tormenta_m" a
# partir de los valores de "marea_tormenta_m" usando regresión lineal y
# revisándolo
################################################################################

modelo_imputacion <- lm(marejada_tormenta_m ~ marea_tormenta_m,
  data = datos_marea_marejada_borrador %>%
    filter(!is.na(marea_tormenta_m), !is.na(marejada_tormenta_m)))
summary(modelo_imputacion)
plot(modelo_imputacion)

# Ploteando la gráfica de regresión lineal:
datos_marea_marejada_borrador %>%
  mutate(
    marejada_tormenta_m_prediccion = predict(modelo_imputacion, .)
  ) %>%
  filter(!is.na(marejada_tormenta_m), !is.na(marejada_tormenta_m_prediccion)) %>%
  gather("variable", "valor", marejada_tormenta_m, marejada_tormenta_m_prediccion) %>%
  ggplot(aes(x = marea_tormenta_m, y = valor, group = variable, colour = variable)) +
  geom_point() +
  geom_smooth(method = "lm") # Todo salió bien.

################################################################################
# 6. Revisiones previas antes de crear la tabla final
################################################################################

# Revisando si hay distintos registros de marejadas asociados a un mismo huracán.
# Esto requerirá revisar por separado los huracanes sin nombre ("unnamed"), ya
# que para diferenciarlos no basta el "anio", sino las fechas de inicio y fin
# completas. Para diferenciar huracanes nombrados, puedo suponer que "nombre_huracan"
# y "anio" son suficientes
datos_marea_marejada_borrador %>%
  filter(nombre_huracan == "unnamed",
    !is.na(fechas_limpias_inicio_huracan), !is.na(fechas_limpias_fin_huracan)) %>%
  group_by(fechas_limpias_inicio_huracan, fechas_limpias_fin_huracan) %>%
  tally() %>%
  ungroup() %>%
  filter(n >= 2) %>%
  inner_join(datos_marea_marejada_borrador,
    by = c("fechas_limpias_inicio_huracan", "fechas_limpias_fin_huracan")) %>%
  View()
# Si hay varios registros de marejada distintos para algunos huracanes "unnamed",
# por ello, debemos eliminarlos

datos_marea_marejada_borrador %>%
  filter(nombre_huracan != "unnamed", !is.na(anio), !is.na(nombre_huracan)) %>%
  group_by(anio, nombre_huracan) %>%
  tally() %>%
  ungroup() %>%
  filter(n >= 2) %>%
  inner_join(datos_marea_marejada_borrador,
    by = c("anio", "nombre_huracan")) %>%
  View()
# También los hay para algunos huracanes con nombre.
  
################################################################################
# 6. Creando la tabla final
################################################################################

datos_marejada_desagregados <- datos_marea_marejada_borrador %>%
  # Creando variables adicionales
  mutate(
    marejada_id = 1:nrow(.),
    marejada_tormenta_m_prediccion = predict(modelo_imputacion, .),
    marejada_tormenta_m_imputacion= case_when(
      !is.na(marejada_tormenta_m) ~ marejada_tormenta_m,
      is.na(marejada_tormenta_m) & !is.na(marea_tormenta_m) ~ marejada_tormenta_m_prediccion,
      TRUE ~ NA_real_
    ) %>%
    round(2),
  
    marejada_tormenta_m_imputada = case_when(
      !is.na(marejada_tormenta_m) ~ FALSE,
      is.na(marejada_tormenta_m) & !is.na(marea_tormenta_m) ~ TRUE,
      TRUE ~ NA
    )
  ) %>%
  select(
    marejada_id,
    nombre_huracan,
    clave_pais,
    pais,
    clave_estado_eua,
    latitud,
    longitud,
    anio,
    fecha_inicial_huracan = fechas_limpias_inicio_huracan,
    fecha_final_huracan = fechas_limpias_fin_huracan,
    marejada_tormenta_m_imputacion,
    marejada_tormenta_m_imputada
  ) %>%
  filter(!is.na(marejada_tormenta_m_imputada))

datos_marejada_agregados_por_huracan <- rbind(
  # Caso 1: para huracanes sin nombre, se tomará como llave natural
  # ("fecha_inicial_huracan", "fecha_final_huracan")
  datos_marejada_desagregados %>%
    filter(nombre_huracan == "unnamed",
      !is.na(fecha_inicial_huracan) | !is.na(fecha_final_huracan)) %>%
    group_by(fecha_inicial_huracan, fecha_final_huracan) %>%
    summarise(
      nombre_huracan = first(nombre_huracan),
      anio = first(anio),
      marejada_tormenta_media_m_imputacion = mean(marejada_tormenta_m_imputacion, na.rm = TRUE)
    ),
  
  # Caso 2: para huracanes nombrados, se tomará como llave natural
  # ("anio", "nombre_huracan")
  datos_marejada_desagregados %>%
    filter(nombre_huracan != "unnamed",
      !is.na(anio), !is.na(nombre_huracan)) %>%
    group_by(anio, nombre_huracan) %>%
    summarise(
      fecha_inicial_huracan = first(fecha_inicial_huracan),
      fecha_final_huracan = first(fecha_final_huracan),
      marejada_tormenta_media_m_imputacion = mean(marejada_tormenta_m_imputacion, na.rm = TRUE)
    )
) %>%
  ungroup() %>%
  mutate(
    marejada_huracan_id = 1:nrow(.)
  )
glimpse(datos_marejada_agregados_por_huracan)
summary(datos_marejada_agregados_por_huracan)

saveRDS(datos_marejada_agregados_por_huracan,
  rutas_archivos_productos["datos_marejada_agregados_por_huracan"])

################################################################################
# 7. Revisiones varias
################################################################################

datos_marejada_agregados_por_huracan %>%
  filter(nombre_huracan == "unnamed") %>%
  group_by(fecha_inicial_huracan, fecha_final_huracan) %>%
  tally() %>%
  filter(n >= 2) %>%
  nrow()
# Perfecto! ("fecha_inicial_huracan", "fecha_final_huracan") es una llave natural
# para los datos de marejada por huracán correspondientes huracanes sin nombre.

datos_marejada_agregados_por_huracan %>%
  filter(nombre_huracan != "unnamed") %>%
  group_by(anio, nombre_huracan) %>%
  tally() %>%
  filter(n >= 2) %>%
  nrow()
# Lo mismo! ("anio", "nombre_huracan") es una llave natural para los datos de
# marejadas por huracán para huracanes con nombre

################################################################################
# 8. Creando y revisando un data frame con información de huracanes
# (no snapshots) obtenida de la base de la NOAA
################################################################################

datos_huracanes <- datos_limpios_snapshots_huracanes %>%
  mutate(
    nombre = estandariza_strings(nombre_huracan)
  ) %>%
  group_by(llave_huracan) %>%
  summarise(
    nombre = first(nombre),
    fecha_inicial = min(fecha_hora) %>%
      as_date(),
    fecha_final = max(fecha_hora) %>%
      as_date()
  ) %>%
  ungroup() %>%
  mutate(
    anio = year(fecha_inicial) 
  )
glimpse(datos_huracanes)
summary(datos_huracanes)

# Verificando si no hay NA's en el data frame de huracanes
nrow(datos_huracanes) == sum(complete.cases(datos_huracanes))
# No hay!

# Revisando que para el data frame anterior se cumpla lo siguiente:
# - ("fecha_inicial", "fecha_final") es una llave natural para los huracanes
# "unnamed".
# - ("nombre", "anio") es una llave natural para los huracanes con nombre.
# Por lo anterior, a la hora de hacer el join, todos los huracanes "unnamed" sin
# información de "fecha_inicial" o "fecha_final" se quedarán sin datos de
# marejada.

datos_huracanes %>%
  filter(nombre == "unnamed") %>%
  group_by(fecha_inicial, fecha_final) %>%
  tally() %>%
  filter(n >= 2) %>%
  inner_join(datos_huracanes, by = c("fecha_inicial", "fecha_final")) %>%
  View()
# Perfecto, sólo hay cuatro huracanes que coinciden en esta fecha inicial, y es
# muy poco probable que caigan en nuestra región de interés.

datos_huracanes %>%
  filter(nombre != "unnamed") %>%
  group_by(anio, nombre) %>%
  tally() %>%
  filter(n >= 2) %>%
  inner_join(datos_huracanes, by = c("anio", "nombre")) %>%
  View()
# Perfecto, sólo está duplicado el huracán Alice.

################################################################################
# 7. Asociando los datos de marejada con sus correspondientes huracanes, y
# revisando el data frame resultante
################################################################################

datos_huracanes_marejada <- rbind(
  # Caso 1: huracanes que no tienen nombre
  datos_huracanes %>%
    filter(nombre == "unnamed") %>%
    # Haciendo el producto cartesiano con los datos de marejada correspondientes
    # a huracanes sin nombre
    crossing(
      datos_marejada_agregados_por_huracan %>%
        filter(nombre_huracan == "unnamed")
    ) %>%
    mutate(
      indice_coincidencia = case_when(
        # Si "fecha_inicial_huracan" y "fecha_final_huracan" son distintas de NA
        # (sabemos que las otras dos siempre son distintas de NA)
        !is.na(fecha_inicial_huracan) & !is.na(fecha_final_huracan) ~
          # Para cada pareja ordenada en el producto cartesiano de "datos_huracanes"
          # con "datos_marejada_agregados_por_huracan", calcular el overlap (en días)
          # entre las fechas iniciales y finales del huracán almacenadas en cada
          # base de datos. Después se tomará la pareja que maximice este overlap.
          intersect(
            interval(fecha_inicial, fecha_final),
            interval(fecha_inicial_huracan, fecha_final_huracan)
          ) %>%
          as.period("days") %>%
          day(),
        # Si falta algún extremo de un intervalo simplemente calcular las distancias
        # entre los extremos que sí se tienen.
        !is.na(fecha_inicial_huracan) & is.na(fecha_final_huracan) ~
          difftime(fecha_inicial_huracan, fecha_inicial) %>%
          as.numeric() %>%
          abs(),
        is.na(fecha_inicial_huracan) & !is.na(fecha_final_huracan) ~
          difftime(fecha_final_huracan, fecha_final) %>%
          as.numeric() %>%
          abs()
      )
    ) %>%
    filter(!is.na(indice_coincidencia)) %>%
    # A cada huracán se le asociará el registro de marejada que maximice su "indice_coincidencia"
    # Notar que el supuesto implícito es que todos los huracanes que tienen datos
    # de marejada se encuentran en la base de la NOAA.
    arrange(llave_huracan, desc(indice_coincidencia)) %>%
    group_by(llave_huracan) %>%
    summarise(
      nombre = first(nombre),
      fecha_inicial = first(fecha_inicial),
      fecha_final = first(fecha_final),
      marejada_tormenta_media_m_imputacion = first(marejada_tormenta_media_m_imputacion)
      # indice_coincidencia = first(indice_coincidencia)
    ),
  
  # Caso 2: huracanes nombrados
  datos_huracanes %>%
    filter(nombre != "unnamed") %>%
    # "datos_marejada_agregados_por_huracan" se formó cuidando la unicidad por
    # "nombre" y "anio" para los huracanes con nombre, por ello, no hay riesgo
    # de generar artefactos del join
    inner_join(datos_marejada_agregados_por_huracan,
      by = c("anio" = "anio", "nombre" = "nombre_huracan")) %>%
    select(
      llave_huracan,
      nombre,
      fecha_inicial,
      fecha_final,
      marejada_tormenta_media_m_imputacion
    )
  )
glimpse(datos_huracanes_marejada)
saveRDS(datos_huracanes_marejada,
  rutas_archivos_productos["datos_huracanes_marejada"])

nrow(datos_huracanes_marejada) == nrow(distinct(datos_huracanes_marejada, llave_huracan))
# Perfecto!

################################################################################
# 7. Haciendo el join del data frame de marejadas por huracán con
# "datos_limpios_snapshots_huracanes"
################################################################################

datos_limpios_snapshots_huracanes_marejada <- datos_limpios_snapshots_huracanes %>%
  # No queremos perder datos que no tienen marejada
  left_join(datos_huracanes_marejada, by = "llave_huracan") %>%
  select(
    -nombre,
    -fecha_inicial,
    -fecha_final
  )
glimpse(datos_limpios_snapshots_huracanes_marejada)
summary(datos_limpios_snapshots_huracanes_marejada)
nrow(datos_limpios_snapshots_huracanes_marejada) == nrow(datos_limpios_snapshots_huracanes)
# Perfecto! No se generaron artefactos del join.

saveRDS(datos_limpios_snapshots_huracanes_marejada,
  rutas_archivos_productos["datos_limpios_snapshots_huracanes_marejada"])

