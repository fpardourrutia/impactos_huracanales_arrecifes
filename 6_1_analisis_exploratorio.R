# Este script consiste en un análisis exploratorio para impactos huracanales
# sobre la cobertura de coral de los sitios de interés

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
# 2. Explorando los datos perfilando adicionalmente contra velocidad del viento
# al impacto o categoría del huracán para comparar poder predictivo de estas
# dos últimas variables
################################################################################

### Contrastando tasas de cambio de coberturas contra intensidad del huracán
### y velocidad del viento al impacto para ver cuál es más ruidosa

## Importante ##

datos_analisis_coberturas_un_huracan_asociado %>%
  ggplot(aes(x = categoria_huracan, y = tasa_cambio_cobertura_coral)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.7, aes(colour = llave_huracan)) +
  xlab("Categoría del huracán") +
  ylab("Tasa de cambio de cobertura de coral") +
  ggtitle("Tasa de cambio de cobertura de coral por \ncategoría del huracán") +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"),
    legend.position="none")

## Importante ##

datos_analisis_coberturas_un_huracan_asociado %>%
  filter(!is.na(maxima_velocidad_viento_impacto_kt_cat)) %>%
  ggplot(aes(x = maxima_velocidad_viento_impacto_kt_cat,
    y = tasa_cambio_cobertura_coral)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.7, aes(colour = llave_huracan)) +
  xlab("Velocidad del viento al impacto (kt)") +
  ylab("Tasa de cambio de cobertura de coral") +
  ggtitle(paste0("Tasa de cambio de cobertura de coral ",
    "por \nmáxima velocidad del viento al impacto")) +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"),
    legend.position="none")
# Claramente se ve que es mejor velocidad del viento al impacto porque es menos
# ruidosa que la categoría del huracán. Se ve una distinción en el daño arrecifal
# causado por vientos de más de 64 nudos.

################################################################################

### Contrastando tasas de cambio de coberturas de coral entre datos específicos
### y no específicos para análisis huracanales

## Importante para la base de datos ##

ggplot(datos_analisis_coberturas_un_huracan_asociado,
  aes(x = datos_especificos_analisis_huracanes, y = tasa_cambio_cobertura_coral)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.4) +
  xlab("Datos especificos analisis huracanes") +
  ylab("Tasa de cambio de cobertura de coral") +
  ggtitle("Tasa de cambio de cobertura de coral por tipo de datos") +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"))
# Claramente las tasas de cambio de cambio muestran mayor cambio negativo en los
# datos que específicamente fueron colectados para análisis de este tipo. Puede
# deberse a sesgo de publicación (sólo se publican resultados estadísticamente
# significativos); a que los estudios para detectar efectos de los huracanes
# sobre el cambio de la cobertura se realizan muestreando los sitios más
# afectados; etc.

################################################################################

### Contrastando tasas de cambio de coberturas de coral contra condición inicial
### de los arrecifes

datos_analisis_coberturas_un_huracan_asociado %>%
  filter(!is.na(maxima_velocidad_viento_impacto_kt_cat)) %>%
  ggplot(aes(x = porcentaje_cobertura_coral_muestra_inicial,
    y = tasa_cambio_cobertura_coral)) +
  geom_point(alpha = 0.4, aes(
    colour = diferencia_muestra_final_salida_huracan_dias_cat)) +
  geom_smooth() +
  facet_wrap(~maxima_velocidad_viento_impacto_kt_cat) +
  xlab("Cobertura de coral en la muestra inicial (%)") +
  ylab("Tasa de cambio de cobertura de coral") +
  labs(color='Tiempo de espera hasta \nremuestreo (días)') +
  ggtitle(paste0("Tasa de cambio de cobertura de coral por \nporcentaje inicial ",
    "de cobertura y máxima velocidad del viento al impacto")) +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"))
# Se puede observar que, en general, a mayor porcentaje de cobertura en la muestra
# inicial, y mayor velocidad del viento al impacto, mayor daño a la cobertura
# de coral. Cabe destacar que para velocidades mayores a 64 kt, el efecto pudiera
# deberse en parte a diferencias en tiempo de espera hasta remuestreo, no obstante,
# las tasas de cambio de cobertura alcanzadas con esta velocidad del viento para
# algunos sitios son mucho más bajas que para otras categorías, por lo que
# creemos que las tendencias también aplican para este caso.

datos_analisis_coberturas_un_huracan_asociado %>%
  ggplot(aes(x = porcentaje_cobertura_coral_muestra_inicial,
    y = tasa_cambio_cobertura_coral)) +
  geom_point(alpha = 0.4, aes(
    colour = diferencia_muestra_final_salida_huracan_dias_cat)) +
  geom_smooth() +
  facet_wrap(~categoria_huracan) +
  xlab("Cobertura de coral en la muestra inicial (%)") +
  ylab("Tasa de cambio de cobertura de coral") +
  labs(color='Tiempo de espera hasta \nremuestreo (días)') +
  ggtitle(paste0("Tasa de cambio de cobertura de coral por \nporcentaje inicial ",
    "de cobertura y categoría del huracán")) +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"))
# Al contrastar porcentaje de cobertura inicial contra tasa de cambio en la
# cobertura de coral, no se ve tanto efecto perfilando por categoría del
# huracán como por velocidad del viento al impacto, en cuyo caso, las tendencias
# decrecientes son evidentes.

##  Importante para la base de datos ##
datos_analisis_coberturas_un_huracan_asociado %>%
  filter(!is.na(maxima_velocidad_viento_impacto_kt_cat)) %>%
  ggplot(aes(x = porcentaje_cobertura_coral_muestra_inicial,
    y = tasa_cambio_cobertura_coral)) +
  geom_point(alpha = 0.4, aes(
    colour = datos_especificos_analisis_huracanes)) +
  geom_smooth() +
  facet_wrap(~maxima_velocidad_viento_impacto_kt_cat) +
  xlab("Cobertura de coral en la muestra inicial (%)") +
  ylab("Tasa de cambio de cobertura de coral") +
  labs(color='Datos específicos \npara el análisis de huracanes') +
  ggtitle(paste0("Tasa de cambio de cobertura de coral por \nporcentaje inicial ",
    "de cobertura y velocidad del viento al impacto")) +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"))
# Se puede ver como los datos no específicos de la base complementan muy bien los
# datos específicos.

# Ahora considerando datos únicamente en el primer cuartil de "tiempo hasta
# remuestreo:
datos_analisis_coberturas_un_huracan_asociado %>%
  filter(diferencia_muestra_final_salida_huracan_dias < 202) %>%
  filter(!is.na(maxima_velocidad_viento_impacto_kt_cat)) %>%
  ggplot(aes(x = porcentaje_cobertura_coral_muestra_inicial,
    y = tasa_cambio_cobertura_coral)) +
  geom_point(alpha = 0.4, aes(
    colour = datos_especificos_analisis_huracanes)) +
  geom_smooth() +
  facet_wrap(~maxima_velocidad_viento_impacto_kt_cat, scales = "free") +
  xlab("Cobertura de coral en la muestra inicial (%)") +
  ylab("Tasa de cambio de cobertura de coral") +
  labs(color='Datos específicos \npara el análisis de huracanes') +
  ggtitle(paste0("Tasa de cambio de cobertura por porcentaje \ninicial ",
    "de cobertura y máxima velocidad del viento al impacto, t < 202 días")) +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"))
# Se ven tendencias muy leves pero se ver al final de cuentas.

################################################################################

### Graficando la tasa de cambio de la cobertura de coral contra diversas
### variables

## Importante para la base de datos ##
datos_analisis_coberturas_un_huracan_asociado %>%
  filter(datos_especificos_analisis_huracanes) %>%
  gather("variable", "valor",
    profundidad_media_m,
    marejada_media_m_imputacion,
    viento_sostenido_maximo_medio_kt,
    presion_central_media_mb,
    distancia_minima_sitio_m,
    tiempo_exposicion_huracan_horas) %>%
  ggplot(aes(x = valor, y = tasa_cambio_cobertura_coral)) +
  geom_point(alpha = 0.4,
    aes(colour = diferencia_muestra_final_salida_huracan_dias_cat)) +
  geom_smooth() +
  facet_wrap(~variable, scales = "free_x") +
  xlab("") +
  ylab("Tasa de cambio de cobertura de coral") +
  labs(color='Tiempo de espera hasta \nremuestreo (días)') +
  ggtitle(paste0("Tasa de cambio de cobertura de coral por diferentes variables ",
    "\npara datos específicos para el análisis de huracanes.")) +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"))
# En general las tendencias no son tan claras, y esto parece no deberse únicamente
# a sesgos introducidos por el tiempo de espera hasta remuestreo (días)

## Importante para la base de datos ##
# Datos tomados con otros fines
datos_analisis_coberturas_un_huracan_asociado %>%
  filter(!datos_especificos_analisis_huracanes) %>%
  gather("variable", "valor",
    profundidad_media_m,
    marejada_media_m_imputacion,
    viento_sostenido_maximo_medio_kt,
    presion_central_media_mb,
    distancia_minima_sitio_m,
    tiempo_exposicion_huracan_horas) %>%
  ggplot(aes(x = valor, y = tasa_cambio_cobertura_coral)) +
  geom_point(alpha = 0.4,
    aes(colour = diferencia_muestra_final_salida_huracan_dias_cat)) +
  geom_smooth() +
  facet_wrap(~variable, scales = "free_x") +
  facet_wrap(~variable, scales = "free_x") +
  xlab("") +
  ylab("Tasa de cambio de cobertura de coral") +
  labs(color="Tiempo de espera hasta \nremuestreo (días)") +
  ggtitle(paste0("Tasa de cambio de cobertura de coral por diferentes variables ",
    "\npara datos no tomados para el análisis de impactos huracanales.")) +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"))
# En general las tendencias no son tan claras, y esto parece no deberse únicamente
# a sesgos introducidos por el tiempo de espera hasta remuestreo (días). Ver, por
# ejemplo, la gráfica de "viento sostenido máximo (kt)" vs "tasa de cambio en
# el porcentaje de cobertura". A pesar de que los vientos máximos coinciden con
# sitios que se muestrearon más prontamente, y los vientos mínimos con sitios
# que demoraron más en muestrearse, no hay gran diferencia.

## Importante para la base de datos ##
datos_analisis_coberturas_un_huracan_asociado %>%
  gather("variable", "valor",
    profundidad_media_m,
    marejada_media_m_imputacion,
    viento_sostenido_maximo_medio_kt,
    presion_central_media_mb,
    distancia_minima_sitio_m,
    tiempo_exposicion_huracan_horas) %>%
  ggplot(aes(x = valor,
    y = tasa_cambio_cobertura_coral)) +
  geom_point(aes(colour = datos_especificos_analisis_huracanes),
    alpha = 0.4) +
  geom_smooth() +
  facet_wrap(~variable, scales = "free_x") +
  xlab("") +
  ylab("Tasa de cambio de cobertura de coral") +
  labs(color='Datos específicos') + 
  ggtitle("Tasa de cambio de cobertura de coral contra diversas variables") +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"))
# Podemos ver cómo al usar todos los datos se complementa la información que
# de otra manera tendría claramente un rango reducido en las variables de
# interés, logrando que las tendencias aparezcan cuando se usan todos los datos.

# Cabe destacar que es de esperarse que los datos específicos para el análisis
# de huracanes estén sesgados hacia los valores extremos de las variables, pues
# si voy a estudiar el daño que causa un huracán a los arrecifes, voy a incluir
# en mi estudio sitios que me sirvan para cuantificar dicho daño. Los datos
# generales complementan esos datos para abarca un mayor rango de las variables
# de interés, y por ello, en su conjunto, hacen que resalten los patrones en los
# datos.

# Este hecho es muy útil para justificar la existencia de la base de
# datos.

datos_analisis_coberturas_un_huracan_asociado %>%
  gather("variable", "valor",
    profundidad_media_m,
    marejada_media_m_imputacion,
    viento_sostenido_maximo_medio_kt,
    presion_central_media_mb,
    distancia_minima_sitio_m,
    tiempo_exposicion_huracan_horas) %>%
  ggplot(aes(x = valor,
    y = tasa_cambio_cobertura_coral)) +
  geom_point(aes(colour = diferencia_muestra_final_salida_huracan_dias_cat),
    alpha = 0.4) +
  geom_smooth() +
  facet_wrap(~variable, scales = "free_x") +
  xlab("") +
  ylab("Tasa de cambio de cobertura de coral") +
  labs(color="Tiempo de espera hasta \nremuestreo (días)") + 
  ggtitle("Tasa de cambio de cobertura de coral contra diversas variables") +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"))
# Se aprecia que ruidos en las tendencias generales posiblemente se deban a
# discrepancias en tiempos de espera hasta remuestreos, (ver, por ejemplo,
# "presión central mb").

# Todos los datos utilizando sólo la primera categoría del tiempo de espera
# hasta remuestreo:
datos_analisis_coberturas_un_huracan_asociado %>%
  filter(diferencia_muestra_final_salida_huracan_dias < 202) %>%
  gather("variable", "valor",
    profundidad_media_m,
    marejada_media_m_imputacion,
    viento_sostenido_maximo_medio_kt,
    presion_central_media_mb,
    distancia_minima_sitio_m,
    tiempo_exposicion_huracan_horas) %>%
  ggplot(aes(x = valor,
    y = tasa_cambio_cobertura_coral)) +
  geom_point(aes(colour = datos_especificos_analisis_huracanes),
    alpha = 0.4) +
  geom_smooth() +
  facet_wrap(~variable, scales = "free_x") +
  xlab("") +
  ylab("Tasa de cambio de cobertura de coral") +
  labs(color='Datos específicos') + 
  ggtitle(paste0("Tasa de cambio de cobertura de coral contra diversas variables\n",
    "t < 202 días")) +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"))

## Importante para huracanes ##
# Todos los datos:
datos_analisis_coberturas_un_huracan_asociado %>%
  gather("variable", "valor",
    profundidad_media_m,
    marejada_media_m_imputacion,
    viento_sostenido_maximo_medio_kt,
    presion_central_media_mb,
    distancia_minima_sitio_m,
    tiempo_exposicion_huracan_horas) %>%
  ggplot(aes(x = valor,
    y = tasa_cambio_cobertura_coral)) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  facet_wrap(~variable, scales = "free_x") +
  xlab("") +
  ylab("Tasa de cambio de cobertura de coral") +
  labs(color='Datos específicos') + 
  ggtitle("Tasa de cambio de cobertura de coral contra diversas variables") +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"))
# Esta gráfica es para la consultoría de huracanes, la otra, para justificar
# la existencia de la base de datos.

################################################################################

### Revisando la correlación entre "presion_central_media_mb" y
### "viento_sostenido_maximo_medio_kt"

datos_analisis_coberturas_un_huracan_asociado %>%
  ggplot(aes(x = presion_central_media_mb, y = viento_sostenido_maximo_medio_kt)) +
  geom_point(alpha = 0.05) +
  geom_smooth()

# Están muy correlacionadas ambas variables

################################################################################

### Revisando los efectos de la exposición contra cobertura arrecifal.
datos_analisis_coberturas_un_huracan_asociado %>%
  filter(!is.na(exposicion)) %>%
  ggplot(aes(x = categoria_huracan, y = tasa_cambio_cobertura_coral)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.7, aes(colour = llave_huracan)) +
  facet_wrap(~exposicion, scales = "free_x") +
  xlab("Categoría de huracán") +
  ylab("Tasa de cambio de cobertura de coral") +
  ggtitle("Tasa de cambio de cobertura de coral por \nexposición y categoría del huracán") +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"),
    legend.position="none")
# Notar que en sitios con sotavento, entre mayor la categoría del huracán, mayor
# varianza en el daño arrecifal. Para barlovento, hay una curva en "U" que
# posiblemente se deba a que los arrecifes de barlovento están acostumbrados a
# los golpeteos de las olas y pueden recuperar su cobertura si se rompen...
# o puede simplemente ser ruido aleatorio.

datos_analisis_coberturas_un_huracan_asociado %>%
  filter(!is.na(exposicion), !is.na(maxima_velocidad_viento_impacto_kt_cat)) %>%
  ggplot(aes(x = maxima_velocidad_viento_impacto_kt_cat, y = tasa_cambio_cobertura_coral)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.7, aes(colour = llave_huracan)) +
  facet_wrap(~exposicion, scales = "free_x") +
  xlab("Velocidad del viento al impacto kt") +
  ylab("Tasa de cambio de cobertura de coral") +
  ggtitle(paste0("Tasa de cambio de cobertura de coral por \nexposición y ",
    "velocidad del viento al impacto")) +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"),
    legend.position="none",
    axis.text.x = element_text(angle = 90, hjust = 1)
  )
# Ya teniendo una variable menos ruidosa: "velocidad del viento al impacto" en
# lugar de "categoría del huracán", desaparece el patrón en U, por lo que es
# posible que se deba a ruido aleatorio. Se ve que en sotavento hay un daño
# mayor considerando vientos de más de 64 kt. Esto se puede deber a que las
# especies del sotavento no están acostumbradas a golpeteos fuertes, sin embargo,
# su posición las protege contra vientos moderados pero posiblemente ya no contra
# vientos fuertes. En algunos diagramas de caja y brazos vemos mucha variabilidad
# por lo que es necesario perfilar por otras variables:

## Importante para huracanes ##
datos_analisis_coberturas_un_huracan_asociado %>%
  filter(!is.na(exposicion), !is.na(maxima_velocidad_viento_impacto_kt_cat)) %>%
  ggplot(aes(
    x = maxima_velocidad_viento_impacto_kt_cat,
    y = tasa_cambio_cobertura_coral,
    fill = exposicion)) +
  geom_boxplot(outlier.size = 0) +
  geom_point(pch = 21, alpha = 0.7, position = position_jitterdodge()) +
  facet_wrap(~diferencia_muestra_final_salida_huracan_dias_cat, scales = "free_x") +
  xlab("Velocidad del viento al impacto kt") +
  ylab("Tasa de cambio de cobertura de coral") +
  ggtitle(paste0("Tasa de cambio de cobertura de coral por \nexposición y ",
    "velocidad del viento al impacto")) +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"),
    axis.text.x = element_text(angle = 90, hjust = 1)
  )
# Eliminando diagramas de caja y brazos con pocos datos, podemos ver que el mayor
# efecto sobre la tasa de cambio de cobertura de coral se detectó con vientos
# mayores a 64 kt y en sotavento muestreado rápidamente (desfortunadamente no
# tenemos esos datos para barlovento.) Idea: modelar una interacción específica
# para el diagrama de caja y brazos raro, para quitarnos esa variable de encima.

################################################################################

### Revisando los efectos del tipo de arrecife contra tasa de cambio en la
### cobertura arrecifal

datos_analisis_coberturas_un_huracan_asociado %>%
  filter(!is.na(tipo_arrecife)) %>%
  ggplot(aes(x = categoria_huracan, y = tasa_cambio_cobertura_coral)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.7, aes(colour = llave_huracan)) +
  facet_wrap(~tipo_arrecife) +
  xlab("Categoría de huracán") +
  ylab("Tasa de cambio de cobertura de coral") +
  ggtitle(paste0("Tasa de cambio de cobertura de coral por \ntipo de arrecife y ",
    "categoría del huracán")) +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"),
    legend.position="none")
# En general, podemos ver que los arrecifes de categoría 4 tienen más impacto
# negativo en la cobertura, en particular para los arrecifes de barrera y
# posiblemente parche, sin embargo, no es muy claro. Veamos por verlo por
# velocidad del viento al impacto en lugar de categoría.

## Importante para huracanes ##
datos_analisis_coberturas_un_huracan_asociado %>%
  filter(!is.na(tipo_arrecife), !is.na(maxima_velocidad_viento_impacto_kt_cat)) %>%
  ggplot(aes(x = maxima_velocidad_viento_impacto_kt_cat,
    y = tasa_cambio_cobertura_coral)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.7, aes(colour = llave_huracan)) +
  facet_wrap(~tipo_arrecife) +
  xlab("Velocidad del viento al impacto kt") +
  ylab("Tasa de cambio de cobertura de coral") +
  ggtitle(paste0("Tasa de cambio de cobertura de coral por \ntipo de arrecife y ",
    "velocidad del viento al impacto")) +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"),
    legend.position="none",
    axis.text.x = element_text(angle = 90, hjust = 1)
  )
# Se ve un comportamiento distinto para arrecifes de "Barrera" y "Costeros" para
# vientos de más de 64 kt. Hay que perfilar contra otras variables para estar
# seguros

## Importante para huracanes ##
datos_analisis_coberturas_un_huracan_asociado %>%
  filter(!is.na(tipo_arrecife), !is.na(maxima_velocidad_viento_impacto_kt_cat)) %>%
  ggplot(aes(
    x = maxima_velocidad_viento_impacto_kt_cat,
    y = tasa_cambio_cobertura_coral,
    fill = tipo_arrecife)) +
  geom_boxplot(outlier.size = 0) +
  geom_point(pch = 21, alpha = 0.7, position = position_jitterdodge()) +
  facet_wrap(~diferencia_muestra_final_salida_huracan_dias_cat, scales = "free_x") +
  xlab("Velocidad del viento al impacto kt") +
  ylab("Tasa de cambio de cobertura de coral") +
  ggtitle(paste0("Tasa de cambio de cobertura de coral por \ntipo de arrecife, ",
    "velocidad del viento al impacto y tiempo hasta remuestreo")) +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"),
    axis.text.x = element_text(angle = 90, hjust = 1)
  )
# Vemos que la diferencia entre la tasa de cambio de cobertura para los arrecifes
# de "Barrera" y "Costeros" puede ser explicada por diferencias en el tiempo de
# hasta remuestreo... parece ser artificial.

################################################################################

### Revisando los efectos de la zona arrecifal contra tasa de cambio en la
### cobertura arrecifal

datos_analisis_coberturas_un_huracan_asociado %>%
  filter(!is.na(zona_arrecifal)) %>%
  mutate(
    zona_arrecifal = ifelse(zona_arrecifal == "Laguana", "Laguna", zona_arrecifal)
  ) %>%
  ggplot(aes(x = categoria_huracan, y = tasa_cambio_cobertura_coral)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.7, aes(colour = llave_huracan)) +
  facet_wrap(~zona_arrecifal) +
  xlab("Categoría de huracán") +
  ylab("Tasa de cambio de cobertura de coral") +
  ggtitle("Tasa de cambio de cobertura de coral por \nzona arrecifal y categoría del huracán") +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"),
    legend.position="none")
# No hay tendencias muy claras, hay que revisar por velocidad de los vientos al
# impacto. Nótese la "U" que se ve en frontal, posiblemente sea ruido.

datos_analisis_coberturas_un_huracan_asociado %>%
  filter(!is.na(zona_arrecifal), !is.na(maxima_velocidad_viento_impacto_kt_cat)) %>%
  mutate(
    zona_arrecifal = ifelse(zona_arrecifal == "Laguana", "Laguna", zona_arrecifal)
  ) %>%
  ggplot(aes(x = maxima_velocidad_viento_impacto_kt_cat,
    y = tasa_cambio_cobertura_coral)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.7, aes(colour = llave_huracan)) +
  facet_wrap(~zona_arrecifal) +
  xlab("Velocidad del viento al impacto kt") +
  ylab("Tasa de cambio de cobertura de coral") +
  ggtitle(paste0("Tasa de cambio de cobertura de coral por \nzona arrecifal ",
    "y velocidad del viento al impacto")) +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"),
    legend.position="none",
    axis.text.x = element_text(angle = 90, hjust = 1)
  )
# Se ve algo interesante: tenemos datos de sitios impactados por vientos de
# más de 64 kt para las zonas frontal y posterior del arrecife, y parece que la
# zona frontal sufre más que la posterior. Fuera de eso no se ve nada y desaparece
# el patrón en U. Perfilemos nuevamente por tiempo hasta remuestreo.

datos_analisis_coberturas_un_huracan_asociado %>%
  filter(!is.na(zona_arrecifal), !is.na(maxima_velocidad_viento_impacto_kt_cat)) %>%
  ggplot(aes(
    x = maxima_velocidad_viento_impacto_kt_cat,
    y = tasa_cambio_cobertura_coral,
    fill = zona_arrecifal)) +
  geom_boxplot(outlier.size = 0) +
  geom_point(pch = 21, alpha = 0.7, position = position_jitterdodge()) +
  facet_wrap(~diferencia_muestra_final_salida_huracan_dias_cat) +
  xlab("Velocidad del viento al impacto kt") +
  ylab("Tasa de cambio de cobertura de coral") +
  ggtitle(paste0("Tasa de cambio de cobertura de coral por \ntipo de arrecife, ",
    "velocidad del viento al impacto y tiempo hasta remuestreo")) +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"),
    axis.text.x = element_text(angle = 90, hjust = 1)
  )


################################################################################

### Revisando los efectos del tiempo hasta remuestreo en la tasa de cambio
### de cobertura de coral

datos_analisis_coberturas_un_huracan_asociado %>%
  ggplot(aes(
    x = categoria_huracan, y = tasa_cambio_cobertura_coral,
    fill = diferencia_muestra_final_salida_huracan_dias_cat)) +
  geom_boxplot(outlier.size = 0) +
  geom_point(pch = 21, alpha = 0.7, position = position_jitterdodge()) +
  xlab("Categoría de huracán") +
  ylab("Tasa de cambio de cobertura de coral") +
  labs(color='Tiempo hasta remuestreo después del impacto (días)') + 
  ggtitle(paste0("Tasa de cambio de cobertura de coral por \ntiempo ",
    "hasta remuestreo y categoría del huracán")) +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold")
  )
# Se ven algunas tendencias que parecen señalar recuperación de los arrecifes en
# el tiempo, así como diagramas de caja y brazos más tirados hacia abajo a medida
# que la categoría del huracán avanza, lo que significa que a huracanes más fuertes,
# más sitios afectados... sin embargo, que el tamaño de los diagramas de caja y 
# brazos vaya incrementando con la categoría del huracán parece indicar que esta
# variable es muy ruidosa, por lo que conviene hacer el análisis por velocidad de
# los vientos al impacto.

datos_analisis_coberturas_un_huracan_asociado %>%
  filter(!is.na(maxima_velocidad_viento_impacto_kt_cat)) %>%
  ggplot(aes(
    x = maxima_velocidad_viento_impacto_kt_cat, y = tasa_cambio_cobertura_coral,
    fill = diferencia_muestra_final_salida_huracan_dias_cat)) +
  geom_boxplot(outlier.size = 0) +
  geom_point(pch = 21, alpha = 0.7, position = position_jitterdodge()) +
  xlab("Velocidad del viento al impacto kt") +
  ylab("Tasa de cambio de cobertura de coral") +
  labs(fill='Tiempo hasta remuestreo å(días)') + 
  ggtitle(paste0("Tasa de cambio de cobertura de coral por \ntiempo ",
    "hasta remuestreo y velocidad del viento al impacto")) +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"),
    axis.text.x = element_text(angle = 90, hjust = 1)
  )
# Se ve un gran efecto del tiempo de remuestreo hasta el impacto, en particular
# para sitios impactados por vientos de más de 64 kt.

# Perfilando por "datos especificos análisis huracanes"
datos_analisis_coberturas_un_huracan_asociado %>%
  filter(!is.na(maxima_velocidad_viento_impacto_kt_cat)) %>%
  ggplot(aes(
    x = maxima_velocidad_viento_impacto_kt_cat, y = tasa_cambio_cobertura_coral,
    fill = diferencia_muestra_final_salida_huracan_dias_cat)) +
  geom_boxplot(outlier.size = 0) +
  geom_point(pch = 21, alpha = 0.7, position = position_jitterdodge()) +
  xlab("Velocidad del viento al impacto kt") +
  ylab("Tasa de cambio de cobertura de coral") +
  labs(fill='Tiempo hasta remuestreo (días)') + 
  ggtitle(paste0("Tasa de cambio de cobertura de coral por \ntiempo ",
    "hasta remuestreo y velocidad del viento al impacto")) +
  facet_wrap(~datos_especificos_analisis_huracanes) +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"),
    axis.text.x = element_text(angle = 90, hjust = 1)
  )
# Perfecto! se ve como se complementan los datos específicos para el análisis
# de huracanes con los no específicos para tener datos en rangos más amplios de
# las variables. En este caso, para la velocidad del viento al impacto.

################################################################################

modelo <- lm(
  tasa_cambio_cobertura_coral ~
    tipo_arrecife +
    zona_arrecifal +
    exposicion +
    maxima_velocidad_viento_impacto_kt_cat +
    viento_sostenido_maximo_medio_kt +
    # categoria_huracan +
    diferencia_muestra_final_salida_huracan_dias +
    porcentaje_cobertura_coral_muestra_inicial +
    tiempo_exposicion_huracan_horas,
  data = datos_analisis_coberturas_un_huracan_asociado)
summary(modelo)
plot(modelo)