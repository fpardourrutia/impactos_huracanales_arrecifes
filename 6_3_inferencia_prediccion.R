# En este script se ajustan los modelos adecuados a los datos para las labores
# de inferencia y predicción que se requieren

library("plyr")
library("dplyr")
library("tidyr")
library("readr")
library("readxl")
library("stringi")
library("lubridate")
library("ggplot2")
source("0_config.R")

################################################################################
# 1. Leyendo los datos y creando un data frame común para el análisis
################################################################################

datos_analisis_coberturas_un_huracan_asociado <- readRDS(
  rutas_archivos_productos["datos_analisis_coberturas_un_huracan_asociado"])
glimpse(datos_analisis_coberturas_un_huracan_asociado)
summary(datos_analisis_coberturas_un_huracan_asociado)

################################################################################
# 2. Revisando si un modelo con autocorrelación temporal es suficiente, o, por
# el contrario, es necesario hacer regresión kriging espacio-temporal
################################################################################

# Para este capítulo utilizaré las notas de la materia de Statistical Modelling
# de St Andrews.



