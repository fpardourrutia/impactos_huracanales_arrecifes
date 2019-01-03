# Este es un archivo de configuración donde se asignarán los parámetros que son
# utilizados a lo largo de todas los scripts utilizados en este análisis.

# Ruta carpeta raiz (relativa a la carpeta donde se encuentra este script)
ruta_carpeta_raiz <- ".."

################################################################################
# Insumos
################################################################################

# Definiendo el nombre y la ruta de la carpeta que contiene los insumos a utilizar:
nombre_carpeta_insumos <- "datos"
ruta_carpeta_insumos <- paste0(ruta_carpeta_raiz, "/", nombre_carpeta_insumos)
dir.exists(ruta_carpeta_insumos)

# Definiendo los nombres y la ruta de cada uno de los insumos individuales.
# Estos se deben encontrar dentro de la carpeta "nombre_carpeta_insumos".

# Agregar aquí el identificador y nombre de cada archivo de insumos
nombres_archivos_insumos <- c(
  "datos_crudos_snapshots_huracanes" = "base_huracanes_hurdat2_1851_2017_050118.csv",
  "global_peak_surge_db" = "globalpeaksurgedb.csv",
  "global_peak_surge_db_paises_interes" = "global_peak_surge_db_paises_interes.csv",
  
  "barco_db" = "barco_db.sqlite",
  
  "remuestreos_porcentajes_cobertura_datos_especiales_huracanes" =
    "remuestreos_porcentajes_cobertura_datos_especiales_huracanes.xlsx",
  "remuestreos_porcentajes_cobertura_florida_islas_virgenes_no_se_sabe_si_hay_huracanes" =
    "remuestreos_porcentajes_cobertura_florida_islas_virgenes_no_se_sabe_si_hay_huracanes.xlsx",
  "remuestreos_porcentajes_cobertura_no_se_sabe_si_hay_huracanes" =
    "remuestreos_porcentajes_cobertura_no_se_sabe_si_hay_huracanes.xlsx",
  "remuestreos_rugosidad_bd_lorenzo_solo_anio_tasa_cambio_calculada" =
    "remuestreos_rugosidad_bd_lorenzo_solo_anio_tasa_cambio_calculada.xlsx",
  "remuestreos_rugosidad_fecha_no_se_sabe_si_hay_huracanes" =
    "remuestreos_rugosidad_fecha_no_se_sabe_si_hay_huracanes.xlsx",
  
  "variables_adicionales_todos_sitios" = "variables_adicionales_todos_sitios.xlsx",
  
  "shape_america" = "shape_america",
  "zonas_utm_15_21" = "zonas_utm_15_21"
)
rutas_archivos_insumos <- paste0(ruta_carpeta_insumos,
  "/", nombres_archivos_insumos)
names(rutas_archivos_insumos) <- names(nombres_archivos_insumos)

################################################################################
# Productos
################################################################################

# Definiendo el nombre y la ruta de la carpeta donde se guardarán los productos:
nombre_carpeta_productos <- "productos"
ruta_carpeta_productos <- paste0(ruta_carpeta_raiz, "/", nombre_carpeta_productos)
if(!dir.exists(ruta_carpeta_productos)){
  dir.create(ruta_carpeta_productos)
}

# Definiendo los nombres y la ruta de cada uno de los productos individuales.
# Estos se almacenarán en la carpeta "nombre_carpeta_productos"

# Agregar aquí el identificador y nombre de cada archivo de productos
nombres_archivos_productos <- c(
  "datos_limpios_snapshots_huracanes" = "1_1_datos_limpios_snapshots_huracanes.rds",
  "datos_marejada_agregados_por_huracan" = "1_2_datos_marejada_agregados_por_huracan.rds",
  "datos_huracanes_marejada" = "1_2_datos_huracanes_marejada",
  "datos_limpios_snapshots_huracanes_marejada" = "1_2_datos_limpios_snapshots_huracanes_marejada.rds",
  
  "datos_cobertura_bd" = "2_1_datos_cobertura_bd.rds",
  "datos_cobertura_archivos_excel" = "2_2_datos_cobertura_archivos_excel.rds",
  "datos_cobertura_integrados" = "2_3_datos_cobertura_integrados.rds",
  
  "duplicados_tabla_variables_adicionales" = "3_1_duplicados_tabla_variables_adicionales.csv",
  "sitios_sin_exposicion" = "3_1_sitios_sin_exposicion.csv",
  "exposiciones_sin_sitio" = "3_1_exposiciones_sin_sitio.csv",
  "datos_cobertura_integrados_variables_adicionales_no_fetch" =
    "3_1_datos_cobertura_integrados_variables_adicionales_no_fetch.rds",
  
  "datos_remuestreos_sitio_snapshots_huracanes" = "4_datos_remuestreos_sitio_snapshots_huracanes.rds",
  
  "datos_limpios_remuestreos_sitio_snapshots_huracanes" = "5_datos_limpios_remuestreos_sitio_snapshots_huracanes.rds",
  "datos_analisis_coberturas_un_huracan_asociado" = "5_datos_analisis_coberturas_un_huracan_asociado.rds"
)

rutas_archivos_productos <- paste0(ruta_carpeta_productos,
  "/", nombres_archivos_productos)
names(rutas_archivos_productos) <- names(nombres_archivos_productos)