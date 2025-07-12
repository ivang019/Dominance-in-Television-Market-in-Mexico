library(dplyr)
library(readxl)
library(writexl)
library(ggplot2)
# Paso 1: Leer base
STAR_ACCESOS_MUNICIPIO <- read_excel("STAR_ACCESOS_MUNICIPIO_IHH.xlsx")

# Paso 2: Limpiar columnas de texto
STAR_ACCESOS_MUNICIPIO <- STAR_ACCESOS_MUNICIPIO %>%
  mutate(across(where(is.character), ~iconv(., from = "", to = "UTF-8", sub = " ")))
# Paso 3: Conteo de municipios en situación de dominancia
resumen_dominancia <- STAR_ACCESOS_MUNICIPIO %>%
  count(dominancia) %>%
  mutate(porcentaje = round(100 * n / sum(n), 1))

#Paso 4: Gráfica de pastel sobre status de dominancia

ggplot(resumen_dominancia, aes(x = "", y = porcentaje, fill = dominancia)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  labs(title = "Distribución de Dominancia en Municipios (STAR - Sep 2023)",
       fill = "Tipo de situación") +
  geom_text(aes(label = paste0(porcentaje, "%")), 
            position = position_stack(vjust = 0.5), size = 5) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "right"
  )

#Paso 5: Dominancia por empresa

dominancia_por_empresa <- STAR_ACCESOS_MUNICIPIO %>%
  filter(dominancia == "dominancia") %>%
  group_by(GRUPO) %>%
  summarise(num_municipios = n(), .groups = "drop") %>%
  arrange(desc(num_municipios))

#Paso 6: Histograma de dominancia por empresa
ggplot(dominancia_por_empresa, aes(x = reorder(GRUPO, -num_municipios), y = num_municipios, fill = GRUPO)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Número de municipios con dominancia por grupo económico",
    x = "Grupo económico",
    y = "Número de municipios"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))