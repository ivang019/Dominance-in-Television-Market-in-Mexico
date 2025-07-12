library(dplyr)
library(readr)
library(writexl)
library(ggplot2)
library(lubridate)


# Paso 1: Leer con codificación UTF-8
TD_ACC_TVRES_ITE_VA <- read_csv("C:/Users/carlos.torres/Downloads/TD_ACC_TVRES_ITE_VA.csv", locale = locale(encoding = "UTF-8"))

# Paso 2: Crear campo único para municipio
TD_ACC_TVRES_ITE_VA <- TD_ACC_TVRES_ITE_VA %>%
  mutate(K_E_M = paste(K_ENTIDAD, K_MUNICIPIO, sep = "-"))

# Paso 3: Limpiar columnas de texto
TD_ACC_TVRES_ITE_VA <- TD_ACC_TVRES_ITE_VA %>%
  mutate(across(where(is.character), ~iconv(., from = "", to = "UTF-8", sub = " ")))

# Paso 4: Crear data.frame con municipios dominantes, no dominantes y monopolio para cada mes entre 2013 y 2024
STAR_SERIE <- TD_ACC_TVRES_ITE_VA %>%
  group_by(ANIO, MES, K_E_M) %>%
  mutate(A_TOTAL_M = sum(A_TOTAL_E)) %>%
  ungroup() %>%
  mutate(MARKET_S = A_TOTAL_E / A_TOTAL_M * 100) %>%
  group_by(ANIO, MES, K_E_M, GRUPO) %>%
  summarise(MARKET_S_GRUPO = sum(MARKET_S), .groups = "drop") %>%
  group_by(ANIO, MES, K_E_M) %>%
  mutate(
    IHH_Municipio = sum(MARKET_S_GRUPO^2),
    lider = MARKET_S_GRUPO[1]/100,
    seguidor = MARKET_S_GRUPO[2]/100,
    S_D = 0.5 * (1 - (lider^2 - seguidor^2)),
    participacion_lider = lider * 100,
    dominancia = case_when(
      is.na(S_D) ~ "monopolio",
      MARKET_S_GRUPO/100 > S_D ~ "dominancia",
      TRUE ~ "no dominancia"
    )
  ) %>%
  arrange(desc(MARKET_S_GRUPO), .by_group = TRUE) %>%
  slice_head(n = 1) %>%  # Solo el grupo líder por municipio
  ungroup()

#Paso 5: Contar los municipios dominantes, no dominantes y monopolio

dominancia_temporal <- STAR_SERIE %>%
  group_by(ANIO, MES, dominancia) %>%
  summarise(num_municipios = n(), .groups = "drop") %>%
  group_by(ANIO, MES) %>%
  mutate(porcentaje = round(100 * num_municipios / sum(num_municipios), 1)) %>%
  ungroup()

#Paso 6: Contar dominancia por grupo 

dominancia_por_grupo_temporal <- STAR_SERIE %>%
  filter(dominancia == "dominancia") %>%
  group_by(ANIO, MES, GRUPO) %>%
  summarise(num_municipios = n(), .groups = "drop")

#Paso 7: Gráfico de evolución de dominancia

dominancia_temporal <- dominancia_temporal %>%
  mutate(FECHA = make_date(year = ANIO, month = MES, day = 1))

ggplot(dominancia_temporal, aes(x = FECHA, y = porcentaje, color = dominancia)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Evolución mensual de tipos de dominancia en municipios",
    x = "Fecha",
    y = "Porcentaje de municipios",
    color = "Tipo de dominancia"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Paso 8: Evolución por grupo económico
dominancia_por_grupo_temporal <- dominancia_por_grupo_temporal %>%
  mutate(FECHA = make_date(year = ANIO, month = MES, day = 1))

ggplot(dominancia_por_grupo_temporal, aes(x = FECHA, y = num_municipios, fill = GRUPO)) +
  geom_col() +
  labs(
    title = "Municipios dominados por grupo económico (por mes)",
    x = "Fecha",
    y = "Número de municipios",
    fill = "Grupo económico"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
