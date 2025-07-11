library(dplyr)
library(readr)
library(writexl)

# Paso 1: Leer con codificación UTF-8
TD_ACC_TVRES_ITE_VA <- read_csv("C:/Users/carlos.torres/Downloads/TD_ACC_TVRES_ITE_VA.csv", locale = locale(encoding = "UTF-8"))

# Paso 2: Limpiar columnas de texto
TD_ACC_TVRES_ITE_VA <- TD_ACC_TVRES_ITE_VA %>%
  mutate(across(where(is.character), ~iconv(., from = "", to = "UTF-8", sub = " ")))

# Paso 3: Procesamiento y dominancia
STAR_ACCESOS <- TD_ACC_TVRES_ITE_VA %>%
  filter(ANIO == 2023, MES == 9) %>%
  mutate(K_E_M = paste(K_ENTIDAD, K_MUNICIPIO, sep = "-")) %>%
  group_by(K_E_M) %>%
  mutate(A_TOTAL_M = sum(A_TOTAL_E)) %>%
  ungroup() %>%
  mutate(MARKET_S = A_TOTAL_E / A_TOTAL_M * 100) %>%
  group_by(K_E_M, GRUPO) %>%
  summarise(MARKET_S_GRUPO = sum(MARKET_S), .groups = "drop") %>%
  group_by(K_E_M) %>%
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
  slice_head(n = 1) %>%  # ← Aquí seleccionamos solo el grupo líder por municipio
  ungroup()

# Paso 4: Exportar a Excel
write_xlsx(STAR_ACCESOS, path = "STAR_ACCESOS_MUNICIPIO_IHH.xlsx")