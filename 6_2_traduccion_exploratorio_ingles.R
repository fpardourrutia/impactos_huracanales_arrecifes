# Este script consiste en la traducción de las gráficas principales del análisis
# exploratorio para la presentación en inglés de Esme.

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

# 1
datos_analisis_coberturas_un_huracan_asociado %>%
  ggplot(aes(x = categoria_huracan, y = tasa_cambio_cobertura_coral)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.7, aes(colour = llave_huracan)) +
  xlab("Hurricane Category") +
  ylab("Rate of change in coral cover") +
  ggtitle("Rate of change in coral cover by \nhurricane category") +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"),
    legend.position="none")

# 2
datos_analisis_coberturas_un_huracan_asociado %>%
  filter(!is.na(maxima_velocidad_viento_impacto_kt_cat)) %>%
  ggplot(aes(x = maxima_velocidad_viento_impacto_kt_cat, y = tasa_cambio_cobertura_coral)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.7, aes(colour = llave_huracan)) +
  xlab("Maximum wind speed at impact (kt)") +
  ylab("Rate of change in coral cover") +
  ggtitle("Rate of change in coral cover by \nmaximum wind speed at impact") +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"),
    legend.position="none")

# 3
datos_analisis_coberturas_un_huracan_asociado %>%
  mutate(
    maximum_wind_speed_impact_kt_cat = case_when(
      maxima_velocidad_viento_impacto_kt_cat == "Menos de 34 kt" ~ "< 34 kt",
      maxima_velocidad_viento_impacto_kt_cat == "Entre 34 kt y 50 kt" ~ "34 kt - 50 kt",
      maxima_velocidad_viento_impacto_kt_cat == "Entre 50 kt y 64 kt" ~ "50 kt - 64 kt",
      maxima_velocidad_viento_impacto_kt_cat == "Mayor a 64 kt" ~ "> 64 kt",
      TRUE ~ NA_character_
    ) %>%
      factor(
        levels = c(
          "< 34 kt",
          "34 kt - 50 kt",
          "50 kt - 64 kt",
          "> 64 kt"), ordered = TRUE)
  ) %>%
  filter(!is.na(maximum_wind_speed_impact_kt_cat)) %>%
  ggplot(aes(x = porcentaje_cobertura_coral_muestra_inicial,
    y = tasa_cambio_cobertura_coral)) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  facet_wrap(~maximum_wind_speed_impact_kt_cat) +
  xlab("Coral cover before hurricane (%)") +
  ylab("Rate of change in coral cover") +
  ggtitle(paste0("Rate of change in coral cover by \n initial cover ",
    "and maximum wind speed at impact")) +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"))

# 4
datos_analisis_coberturas_un_huracan_asociado %>%
  mutate(
    maximum_wind_speed_impact_kt_cat = case_when(
      maxima_velocidad_viento_impacto_kt_cat == "Menos de 34 kt" ~ "< 34 kt",
      maxima_velocidad_viento_impacto_kt_cat == "Entre 34 kt y 50 kt" ~ "34 kt - 50 kt",
      maxima_velocidad_viento_impacto_kt_cat == "Entre 50 kt y 64 kt" ~ "50 kt - 64 kt",
      maxima_velocidad_viento_impacto_kt_cat == "Mayor a 64 kt" ~ "> 64 kt",
      TRUE ~ NA_character_
    ) %>%
      factor(
        levels = c(
          "< 34 kt",
          "34 kt - 50 kt",
          "50 kt - 64 kt",
          "> 64 kt"), ordered = TRUE)
  ) %>%
  filter(diferencia_muestra_final_salida_huracan_dias < 202) %>%
  filter(!is.na(maximum_wind_speed_impact_kt_cat)) %>%
  ggplot(aes(x = porcentaje_cobertura_coral_muestra_inicial,
    y = tasa_cambio_cobertura_coral)) +
  geom_point(alpha = 0.4, aes(
    colour = datos_especificos_analisis_huracanes)) +
  geom_smooth() +
  facet_wrap(~maximum_wind_speed_impact_kt_cat, scales = "free") +
  xlab("Coral cover before hurricane (%)") +
  ylab("Rate of change in coral cover") +
  labs(color='Data collected for \nhurricane analyses') +
  ggtitle(paste0("Rate of change in coral cover by initial cover ",
    "and maximum wind speed at impact,\nonly taking into account time until resampling < 202 ")) +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"))
# Se ven tendencias muy leves pero se ver al final de cuentas.



# 5
datos_analisis_coberturas_un_huracan_asociado %>%
  rename(
    mean_depth_m = profundidad_media_m,
    mean_surge_m_imputation = marejada_media_m_imputacion,
    maximum_sustained_wind_kt = viento_sostenido_maximo_medio_kt,
    central_pressure_mb = presion_central_media_mb,
    minimum_distance_m = distancia_minima_sitio_m,
    time_exposition_hurricane_winds_h = tiempo_exposicion_huracan_horas
  ) %>%
  gather("variable", "valor",
    mean_depth_m,
    mean_surge_m_imputation,
    maximum_sustained_wind_kt,
    central_pressure_mb,
    minimum_distance_m,
    time_exposition_hurricane_winds_h) %>%
  ggplot(aes(x = valor,
    y = tasa_cambio_cobertura_coral)) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  facet_wrap(~variable, scales = "free_x") +
  xlab("") +
  ylab("Rate of change in coral cover") +
  labs(color='Data collected for hurricane analyses') + 
  ggtitle("Rate of change in coral cover against different variables") +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"))

# 6
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
  ylab("Rate of change in coral cover") +
  labs(color='Data collected for \nhurricane analyses') + 
  ggtitle(paste0("Rate of change in coral cover against different variables\n",
    "only taking into account times until resampling < 202")) +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"))

# 7.
datos_analisis_coberturas_un_huracan_asociado %>%
  mutate(
    maximum_wind_speed_impact_kt_cat = case_when(
      maxima_velocidad_viento_impacto_kt_cat == "Menos de 34 kt" ~ "< 34 kt",
      maxima_velocidad_viento_impacto_kt_cat == "Entre 34 kt y 50 kt" ~ "34 kt - 50 kt",
      maxima_velocidad_viento_impacto_kt_cat == "Entre 50 kt y 64 kt" ~ "50 kt - 64 kt",
      maxima_velocidad_viento_impacto_kt_cat == "Mayor a 64 kt" ~ "> 64 kt",
      TRUE ~ NA_character_
    ) %>%
      factor(
        levels = c(
          "< 34 kt",
          "34 kt - 50 kt",
          "50 kt - 64 kt",
          "> 64 kt"), ordered = TRUE),
    
    exposition = case_when(
      exposicion == "Sotavento" ~ "Leeward",
      exposicion == "Barlovento" ~ "Windward",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(exposicion), !is.na(maxima_velocidad_viento_impacto_kt_cat)) %>%
  ggplot(aes(x = maxima_velocidad_viento_impacto_kt_cat, y = tasa_cambio_cobertura_coral)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.7, aes(colour = llave_huracan)) +
  facet_wrap(~exposition, scales = "free_x") +
  xlab("Maximum wind speed at impact (kt") +
  ylab("Rate of change in coral cover") +
  ggtitle(paste0("Rate of change in coral cover by \nexposition and ",
    "maximum wind speed at impact")) +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"),
    legend.position="none",
    axis.text.x = element_text(angle = 90, hjust = 1)
  )


# 8
datos_analisis_coberturas_un_huracan_asociado %>%
  mutate(
    maximum_wind_speed_impact_kt_cat = case_when(
      maxima_velocidad_viento_impacto_kt_cat == "Menos de 34 kt" ~ "< 34 kt",
      maxima_velocidad_viento_impacto_kt_cat == "Entre 34 kt y 50 kt" ~ "34 kt - 50 kt",
      maxima_velocidad_viento_impacto_kt_cat == "Entre 50 kt y 64 kt" ~ "50 kt - 64 kt",
      maxima_velocidad_viento_impacto_kt_cat == "Mayor a 64 kt" ~ "> 64 kt",
      TRUE ~ NA_character_
    ) %>%
      factor(
        levels = c(
          "< 34 kt",
          "34 kt - 50 kt",
          "50 kt - 64 kt",
          "> 64 kt"), ordered = TRUE),
    
    exposition = case_when(
      exposicion == "Sotavento" ~ "Leeward",
      exposicion == "Barlovento" ~ "Windward",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(exposition), !is.na(maximum_wind_speed_impact_kt_cat)) %>%
  ggplot(aes(
    x = maximum_wind_speed_impact_kt_cat,
    y = tasa_cambio_cobertura_coral,
    fill = exposition)) +
  geom_boxplot(outlier.size = 0) +
  geom_point(pch = 21, alpha = 0.7, position = position_jitterdodge()) +
  facet_wrap(~diferencia_muestra_final_salida_huracan_dias_cat) +
  xlab("Wind speed at impact (kt)") +
  ylab("Rate of change in coral cover") +
  labs(fill = "Exposition") + 
  ggtitle(paste0("Rate of change in coral cover by \nexposition, ",
    "wind speed at impact, and time until resampling")) +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"),
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

# 9.
datos_analisis_coberturas_un_huracan_asociado %>%
  mutate(
    maximum_wind_speed_impact_kt_cat = case_when(
      maxima_velocidad_viento_impacto_kt_cat == "Menos de 34 kt" ~ "< 34 kt",
      maxima_velocidad_viento_impacto_kt_cat == "Entre 34 kt y 50 kt" ~ "34 kt - 50 kt",
      maxima_velocidad_viento_impacto_kt_cat == "Entre 50 kt y 64 kt" ~ "50 kt - 64 kt",
      maxima_velocidad_viento_impacto_kt_cat == "Mayor a 64 kt" ~ "> 64 kt",
      TRUE ~ NA_character_
    ) %>%
      factor(
        levels = c(
          "< 34 kt",
          "34 kt - 50 kt",
          "50 kt - 64 kt",
          "> 64 kt"), ordered = TRUE),
    
    reef_type = case_when(
      tipo_arrecife == "Atolón" ~ "Atoll",
      tipo_arrecife == "Banco" ~ "Bank",
      tipo_arrecife == "Barrera" ~ "Barrier",
      tipo_arrecife == "Costero" ~ "Fringing",
      tipo_arrecife == "Parche" ~ "Patch",
      tipo_arrecife == "Plataforma" ~ "Platform",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(reef_type), !is.na(maximum_wind_speed_impact_kt_cat)) %>%
  ggplot(aes(x = maximum_wind_speed_impact_kt_cat,
    y = tasa_cambio_cobertura_coral)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.7, aes(colour = llave_huracan)) +
  facet_wrap(~reef_type) +
  xlab("Maximum wind speed at impact (kt)") +
  ylab("Rate of change in coral cover") +
  ggtitle(paste0("Rate of change in coral cover by \nreef type and ",
    "maximum wind speed at impact")) +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"),
    legend.position="none",
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

# 10.
datos_analisis_coberturas_un_huracan_asociado %>%
  mutate(
    maximum_wind_speed_impact_kt_cat = case_when(
      maxima_velocidad_viento_impacto_kt_cat == "Menos de 34 kt" ~ "< 34 kt",
      maxima_velocidad_viento_impacto_kt_cat == "Entre 34 kt y 50 kt" ~ "34 kt - 50 kt",
      maxima_velocidad_viento_impacto_kt_cat == "Entre 50 kt y 64 kt" ~ "50 kt - 64 kt",
      maxima_velocidad_viento_impacto_kt_cat == "Mayor a 64 kt" ~ "> 64 kt",
      TRUE ~ NA_character_
    ) %>%
      factor(
        levels = c(
          "< 34 kt",
          "34 kt - 50 kt",
          "50 kt - 64 kt",
          "> 64 kt"), ordered = TRUE),
    
    reef_type = case_when(
      tipo_arrecife == "Atolón" ~ "Atoll",
      tipo_arrecife == "Banco" ~ "Bank",
      tipo_arrecife == "Barrera" ~ "Barrier",
      tipo_arrecife == "Costero" ~ "Fringing",
      tipo_arrecife == "Parche" ~ "Patch",
      tipo_arrecife == "Plataforma" ~ "Platform",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(reef_type), !is.na(maximum_wind_speed_impact_kt_cat)) %>%
  ggplot(aes(x = maximum_wind_speed_impact_kt_cat,
    y = tasa_cambio_cobertura_coral)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.7, aes(colour = llave_huracan)) +
  facet_wrap(~reef_type) +
  xlab("Maximuym wind speed at impact (kt)") +
  ylab("Rate of change in coral cover") +
  ggtitle(paste0("Rate of change in coral cover by \nreef type, ",
    "and maximum wind speed at impact")) +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"),
    legend.position="none",
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

# 11.
datos_analisis_coberturas_un_huracan_asociado %>%
  mutate(
    maximum_wind_speed_impact_kt_cat = case_when(
      maxima_velocidad_viento_impacto_kt_cat == "Menos de 34 kt" ~ "< 34 kt",
      maxima_velocidad_viento_impacto_kt_cat == "Entre 34 kt y 50 kt" ~ "34 kt - 50 kt",
      maxima_velocidad_viento_impacto_kt_cat == "Entre 50 kt y 64 kt" ~ "50 kt - 64 kt",
      maxima_velocidad_viento_impacto_kt_cat == "Mayor a 64 kt" ~ "> 64 kt",
      TRUE ~ NA_character_
    ) %>%
      factor(
        levels = c(
          "< 34 kt",
          "34 kt - 50 kt",
          "50 kt - 64 kt",
          "> 64 kt"), ordered = TRUE),
    
    reef_type = case_when(
      tipo_arrecife == "Atolón" ~ "Atoll",
      tipo_arrecife == "Banco" ~ "Bank",
      tipo_arrecife == "Barrera" ~ "Barrier",
      tipo_arrecife == "Costero" ~ "Fringing",
      tipo_arrecife == "Parche" ~ "Patch",
      tipo_arrecife == "Plataforma" ~ "Platform",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(reef_type), !is.na(maximum_wind_speed_impact_kt_cat)) %>%
  ggplot(aes(
    x = maximum_wind_speed_impact_kt_cat,
    y = tasa_cambio_cobertura_coral,
    fill = reef_type)) +
  geom_boxplot(outlier.size = 0) +
  geom_point(pch = 21, alpha = 0.7, position = position_jitterdodge()) +
  facet_wrap(~diferencia_muestra_final_salida_huracan_dias_cat, scales = "free_x") +
  xlab("Wind speed at impact (kt)") +
  ylab("Rate of change in coral cover") +
  ggtitle(paste0("Rate of change in coral cover by \nreef type, ",
    "maximum wind speed at impact and time until resampling")) +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"),
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

# 12.
datos_analisis_coberturas_un_huracan_asociado %>%
  mutate(
    maximum_wind_speed_impact_kt_cat = case_when(
      maxima_velocidad_viento_impacto_kt_cat == "Menos de 34 kt" ~ "< 34 kt",
      maxima_velocidad_viento_impacto_kt_cat == "Entre 34 kt y 50 kt" ~ "34 kt - 50 kt",
      maxima_velocidad_viento_impacto_kt_cat == "Entre 50 kt y 64 kt" ~ "50 kt - 64 kt",
      maxima_velocidad_viento_impacto_kt_cat == "Mayor a 64 kt" ~ "> 64 kt",
      TRUE ~ NA_character_
    ) %>%
      factor(
        levels = c(
          "< 34 kt",
          "34 kt - 50 kt",
          "50 kt - 64 kt",
          "> 64 kt"), ordered = TRUE),
    
    reef_zone = case_when(
      zona_arrecifal == "Cresta" ~ "Crest",
      zona_arrecifal == "Laguna" ~ "Lagoon",
      zona_arrecifal == "Laguana" ~ "Lagoon",
      zona_arrecifal == "Frontal" ~ "Fore reef",
      zona_arrecifal == "Posterior" ~ "Back reef",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(reef_zone), !is.na(maximum_wind_speed_impact_kt_cat)) %>%
  ggplot(aes(x = maximum_wind_speed_impact_kt_cat,
    y = tasa_cambio_cobertura_coral)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.7, aes(colour = llave_huracan)) +
  facet_wrap(~reef_zone) +
  xlab("Maximum wind speed at impact kt") +
  ylab("Rate of change coral cover") +
  ggtitle(paste0("Rate of change coral cover by \nreef zone ",
    "and maximum wind speed at impact")) +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"),
    legend.position="none",
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

# 13.
datos_analisis_coberturas_un_huracan_asociado %>%
  mutate(
    maximum_wind_speed_impact_kt_cat = case_when(
      maxima_velocidad_viento_impacto_kt_cat == "Menos de 34 kt" ~ "< 34 kt",
      maxima_velocidad_viento_impacto_kt_cat == "Entre 34 kt y 50 kt" ~ "34 kt - 50 kt",
      maxima_velocidad_viento_impacto_kt_cat == "Entre 50 kt y 64 kt" ~ "50 kt - 64 kt",
      maxima_velocidad_viento_impacto_kt_cat == "Mayor a 64 kt" ~ "> 64 kt",
      TRUE ~ NA_character_
    ) %>%
      factor(
        levels = c(
          "< 34 kt",
          "34 kt - 50 kt",
          "50 kt - 64 kt",
          "> 64 kt"), ordered = TRUE),
    reef_zone = case_when(
      zona_arrecifal == "Cresta" ~ "Crest",
      zona_arrecifal == "Laguna" ~ "Lagoon",
      zona_arrecifal == "Laguana" ~ "Lagoon",
      zona_arrecifal == "Frontal" ~ "Fore reef",
      zona_arrecifal == "Posterior" ~ "Back reef",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(reef_zone), !is.na(maximum_wind_speed_impact_kt_cat)) %>%
  ggplot(aes(
    x = maximum_wind_speed_impact_kt_cat,
    y = tasa_cambio_cobertura_coral,
    fill = reef_zone)) +
  geom_boxplot(outlier.size = 0) +
  geom_point(pch = 21, alpha = 0.7, position = position_jitterdodge()) +
  facet_wrap(~diferencia_muestra_final_salida_huracan_dias_cat) +
  xlab("Maximum wind speed at impact (kt)") +
  ylab("Rate of change coral cover (%)") +
  ggtitle(paste0("Rate of change coral cover by \nreef zone, ",
    "maximum wind speed at impact and time until resampling")) +
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, face="bold"),
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

