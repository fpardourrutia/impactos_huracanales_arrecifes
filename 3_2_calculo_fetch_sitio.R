# En este script se calcula el fetch por sitio basándose únicamente en las
# características geográficas del mismo.

# El fetch (m) se define como la distancia de agua máxima sobre la cuál un viento
# puede soplar. Entre más fetch se esperan olas más energéticas. Para calcularlo
# usaré el shape file de América.

# Cabe destacar que el cálculo del fetch implica reproyectar las coordenadas de
# WGS84 (esféricas) a UTM (cartesianas), esto debido a que se necesita calcular
# distancias.

library("plyr")
library("dplyr")
library("tidyr")
library("readxl")
library("readr")
library("rgdal")
library("rgeos")

# Para reproyectar de WGS84 a Mercator antes de calcular el fetch (distancias)
library("PBSmapping")
library("fetchR") # Para calcular los fetches de cada sitio
source("0_config.R")
source("funciones_auxiliares.R")

################################################################################
# 1. Leyendo los data frames y shape files necesarios para calcular el fetch
################################################################################

# Leyendo el shape file de América. Dice Mau que la generalización (el objeto
# mínimo caracterizable es de aproximandamente 1 Km^2), sin embargo, Esme lo vio
# y dio el visto bueno. Además, es el mapa más sencillo que podemos encontrar.
america_sp <- readOGR(rutas_archivos_insumos["shape_america"])
plot(america_sp)
is(america_sp, "SpatialPolygons") # Perfecto
proj4string(america_sp) #Perfecto, WGS84

# Leyendo el shape file con los cuadrantes de interés, con el fin de clasificar
# los datos que pertenecen a diferentes "zonas UTM" (franjas verticales). Cabe
zonas_utm_15_21_sp <- readOGR(rutas_archivos_insumos["zonas_utm_15_21"])
glimpse(zonas_utm_15_21_sp@data)
proj4string(zonas_utm_15_21_sp) #WGS84

# Leyendo el data frame que contiene la información de los sitios de interés
datos_cobertura_integrados_variables_adicionales_no_fetch <-
  readRDS(rutas_archivos_productos["datos_cobertura_integrados_variables_adicionales_no_fetch"])

################################################################################
# 2. Creando un data frame espacializado con los sitios de interés
################################################################################

sitios_sp <- datos_cobertura_integrados_variables_adicionales_no_fetch %>%
  dplyr::select(
    nombre_sitio,
    latitud,
    longitud
  ) %>%
  distinct(nombre_sitio, .keep_all = TRUE)

coordinates(sitios_sp) <-~ longitud + latitud
proj4string(sitios_sp) <- proj4string(america_sp)

writeOGR(sitios_sp, "~/Desktop", "sitios_sf", driver = "ESRI Shapefile", verbose = FALSE, overwrite_layer = TRUE)

################################################################################
# 2. Obteniendo las intersecciones del shape de América con las zonas utm
# apropiadas.
################################################################################

# En realidad se necesitan sólo 7 zonas, pero como el shape de zonas UTM lo
# tenemos seccionado también "por renglones", entonces se iterará sobre cada uno
# de estos
intersecciones_zonas_utm_regiones_america_sp <- gIntersection(
  zonas_utm_15_21_sp, america_sp, byid = TRUE, drop_lower_td = TRUE)
plot(intersecciones_zonas_utm_regiones_america_sp)
points(sitios_sp, col = "red", pch = 20, cex = 1)

################################################################################
# 3. Revisando y componiendo que no caen en el mar
################################################################################

# Revisando sitios que no caen en el mar
gIntersection(america_sp, sitios_sp, byid = TRUE)
# Hay 41 sitios que no caen en el mar, sino en tierra

##### ME QUEDÉ AQUÍ, LO QUE TENGO QUE HACER ES PEDIRLE A MAU AYUDA PARA QUE
##### LOS SITIOS QUE CAEN EN TIERRA ME LOS SNAPEE A LAS ORILLAS DE LOS POLÍGONOS.

# Para componerlos:
# 1. Crearé un búffer alrededor de ellos
# 2. Obtendré la diferencia de conjuntos entre el búffer y el shape de América
# 3. Promediaré los puntos que quedaron del búffer para asignarle nuevas
# coordenadas al sitio
# 4. Revisaré el resultado
america_raster <- raster()
extent(america_raster) <- extent(america_sp)
projection(america_raster) <- projection(america_sp)
america_raster <- rasterize(america_sp, raster(ext = extent(america_sp), crs = proj(america_sp)), field = 1)
plot(america_raster)

gIntersection(america_sp, sitios_sp, byid = TRUE) %>%
  gBuffer(byid = TRUE, width = 0.01) %>%
  gDifference(america_sp, byid = TRUE) %>%
  as_data_frame() %>%
  View()





# Reproyectando los datos anteriores a UTM (ver mapa en referencias). UTM
# básicamente divide la superficie terrestre en 60 zonas (franjas verticales),
# y en cada zona se definen coordenadas cartesianas (en m). Por ello, si
# revisamos el mapa que me pasó Esme, no es necesario el "id de fila", sólo
# el "id de columna".


my_fetch_proj <- fetch(polygon_layer = america_sp,
  site_layer = sitios_sp,
  max_dist = 300,
  n_directions = 9,
  site_names = sitios_sp$nombre_sitio)

my_fetch_proj



