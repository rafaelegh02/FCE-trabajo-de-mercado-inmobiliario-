# == INFORMACIÓN DEL ESTUDIANTE ===================================
# Nombre: Evelyn Condori – Legajo: 882910 – Email: e.condori94@hotmail.com
# Nombre: Ezequiel Fontana – Legajo: 889309 – Email: ezequielfontana15@gmail.com
# Nombre: Rafael González – Legajo: 909454 – Email: rgonzalezh2002@gmail.com

# == CONFIGURACIÓN INICIAL ========================================

# Establecer directorio de trabajo (ajustar si es necesario)
# setwd("~/Documents/GitHub/TP-CDD")
setwd("C:/FCE-trabajo-de-mercado-inmobiliario--main")

# Si no tenés janitor instalado, correr esto una vez:
# install.packages("janitor")

# Cargar librerías
library(tidyverse)
library(janitor)
# library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

# == 01 - CARGA DE DATOS ORIGINALES ====================================

# Leer archivos (lee los archivos separados por ; y considera como marca decimal la ,
# y como marca del mil el .
# Luego usa la funcion clean_names para limpiar los nombres (ej le saca la enie a an~o)

df_venta <- read_delim("raw/precio-venta-deptos.csv",
                       delim = ";",
                       locale = locale(decimal_mark = ",", grouping_mark = "."),
                       show_col_types = FALSE) %>% clean_names()

df_alquiler  <- read_delim("raw/precio-alquiler-deptos.csv",
                           delim = ";",
                           locale = locale(decimal_mark = ",", grouping_mark = "."),
                           show_col_types = FALSE) %>% clean_names()

df_superficie <- read_delim("raw/superficie-deptos.csv",
                            delim = ";",
                            locale = locale(decimal_mark = ",", grouping_mark = "."),
                            show_col_types = FALSE) %>% clean_names()

df_prestamos <- read_delim("raw/montos-creditos-hipotecarios-uva.csv",
                           delim = ";",
                           locale = locale(decimal_mark = ",", grouping_mark = "."),
                           show_col_types = FALSE) %>% clean_names()

df_actividad <- read_delim("raw/actas-notariales-compra-venta-inmuebles-hipotecas.csv",
                           delim = ";",
                           locale = locale(decimal_mark = ",", grouping_mark = "."),
                           show_col_types = FALSE) %>% clean_names()

# == 02 - EXPLORACION DE DATOS ====================================
# (ejemplo con df_venta)

cat("\n--- df_venta: Precio de venta de departamentos ---\n")
# Medidas de resumen de la estructura del df:
glimpse(df_venta)
summary(df_venta)

cat("\nValores faltantes por columna:\n")
print(colSums(is.na(df_venta))) # precio promedio tiene muchos datos faltantes (4,211 de 7,296 - mas del 50%)

cat("\nBarrios en los que se registraron ventas de departamentos:\n")
print(unique(df_venta$barrio))

cat("\nCantidad de Ambientes que poseen los departamentos:\n")
print(unique(df_venta$ambientes))

cat("\nRango de precios promedio:\n")
print(range(as.numeric(df_venta$precio_prom), na.rm = TRUE))

# == 03 - LIMPIEZA Y TRANSFORMACIÓN ====================================

# df_venta
df_venta <- df_venta %>%
  rename(anio = ano) %>%                                 # Renombra la columna ano a anio para que sea consistente con las otras tablas
  mutate(
    precio_prom = as.numeric(precio_prom),               # Considera el precio promedio como caa numérico
    ambientes = as.factor(ambientes),                    # Convierte los ambientes en variable categórica
    estado = as.factor(estado),                          # Convierte el estado del inmueble en factor
    fecha_trim = paste0(anio, "-Q", trimestre),           # Crea una columna de fecha con el anio y el trimestre             
    barrio = str_to_title(barrio)                        # Pone en mayúscula la primera letra de cada palabra del barrio
  ) %>% filter(precio_prom > 500, precio_prom < 10000)   # Filtra precios fuera de rango 500-10000 (los consideramos outliers o errores)

# df_alquiler
meses_esp <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
               "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

df_alquiler <- df_alquiler %>%                              
  mutate(
    anio = as.integer(anio),                                   # Convierte el año a número entero
    precio_prom = as.numeric(precio_prom),                     # Convierte el precio a numérico
    fecha = make_date(anio, match(mes, meses_esp), 1),         # Crea una fecha del 1 dia del mes y anio del alquiler
    ambientes = as.factor(ambientes),                          # Convierte los ambientes en variable categórica
    barrio = str_to_title(barrio),                             # Pone en mayuscula la primera letra del barrio
  ) %>% filter(precio_prom > 10000, precio_prom < 1000000)     # Filtra precios fuera de rango 10000-1000000 (los consideramos outliers o errores)

# df_superficie
df_superficie <- df_superficie %>%
    rename(anio = ano) %>%                                     # Renombra la columna ano a anio para que sea consistente con las otras tablas
    mutate(
    superficie = as.numeric(superficie),                       # Convierte la superficie a numérico
    fecha = make_date(anio, match(mes, meses_esp), 1),                            # Crea una fecha del 1 dia del mes y anio
    barrio = str_to_title(barrio)                              # Pone en mayuscula la primera letra del barrio
  ) %>% filter(superficie > 50, superficie < 50000)            # Filtra superficies fuera de rango 50-50000 (los consideramos outliers o errores)

# df_prestamos
df_prestamos <- df_prestamos %>%
  rename(monto = monto_operado_prestamos_de_uva_en_miles_de_pesos) %>%  # Renombra la columna a monto
  separate(ano_mes, into = c("anio", "mes"), sep = "-") %>%             # Separa el anio y el mes
  mutate(
    monto = as.numeric(monto),                                          # Monto queda como valor numerico
    anio = as.integer(anio),                                            # Anio como valor entero
    mes = as.integer(mes),                                              # Mes como valor entero
    fecha = make_date(anio, mes, 1),                                    # Usa el anio y mes para definir la fecha (1er dia del mes)
    trimestre = case_when(                                              # Agrega una columna con la informacion del trimestre a partir del mes
      mes %in% 1:3 ~ "1",
      mes %in% 4:6 ~ "2",
      mes %in% 7:9 ~ "3",
      mes %in% 10:12 ~ "4",
      TRUE ~ NA_character_
    ),
    fecha_trim = paste0(anio, "-Q", trimestre)                         # Se agrega la columna de la fecha trimestral
  ) %>%
  select(fecha, monto, fecha_trim, anio, mes)                          # Nos quedamos con las columnas fecha, monto, fecha trimestral, anio y mes
                                             

# Guardar versión limpia de cada una de las tablas - bd
if (!dir.exists("input/clean")) dir.create("input/clean")
saveRDS(df_venta,      "input/clean/venta_clean.rds")
saveRDS(df_alquiler,   "input/clean/alquiler_clean.rds")
saveRDS(df_superficie, "input/clean/superficie_clean.rds")
saveRDS(df_prestamos,  "input/clean/prestamos_clean.rds")

# == 04 - VISUALIZACIONES ==============================================

# Evolución del precio de venta (USD/m²) a lo largo del tiempo 
graf_venta_tiempo <- df_venta %>%
  group_by(fecha_trim) %>%
  summarise(precio_m2 = mean(precio_prom, na.rm = TRUE)) %>%
  ggplot(aes(x = fecha_trim, y = precio_m2, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(title = "Evolución del precio promedio de venta (USD/m²)", x = "Fecha", y = "USD por m²") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

graf_venta_tiempo

if (!dir.exists("images")) dir.create("images")
ggsave(filename = "images/evolucionVenta.png", plot = graf_venta_tiempo, width = 8, height = 6, dpi = 300)


# Monto mensual de préstamos UVA
graf_prestamos <- df_prestamos %>%
  ggplot(aes(x = fecha, y = monto)) +
  geom_line(color = "red", linewidth = 1) +
  labs(title = "Monto de préstamos hipotecarios UVA otorgados", x = "Fecha", y = "Miles de pesos") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
graf_prestamos
ggsave(filename = "images/graf_prestamos.png", plot = graf_prestamos, width = 8, height = 6, dpi = 300)

# RELACION ENTRE EL MONTO MENSUAL DE PRESTAMOS UVA Y EL PRECIO DE VENTA
# 1. Armamos una tabla unificada considerando el rango de fechas con datos en ambas tablas
# entre 2016 y 2020, a partir de las fechas trimestrales
df_venta_grafico2 <- df_venta %>%
  filter(anio > 2016, anio < 2020) %>%
  group_by(fecha_trim) %>%
  summarise(valor = mean(precio_prom, na.rm = TRUE)) %>%
  mutate(tipo = "Venta (USD/m²)")

df_prestamos_grafico2 <- df_prestamos %>%
  filter(anio > 2016, anio < 2020) %>%
  group_by(fecha_trim) %>%
  summarise(valor = sum(monto, na.rm = TRUE)) %>%
  mutate(tipo = "Préstamos UVA (mil ARS)")

df_combinado_grafico2 <- bind_rows(df_venta_grafico2, df_prestamos_grafico2) %>%
  mutate(fecha_trim = factor(fecha_trim, levels = sort(unique(fecha_trim))))

# 2. Graficamos combinados los prestamos y la venta de los inmuebles
graf_prestamos_venta <- ggplot(df_combinado_grafico2, aes(x = fecha_trim, y = valor)) +
  geom_line(aes(color = tipo, group = tipo), linewidth = 1) +
  facet_wrap(~tipo, ncol = 1, scales = "free_y") +
  labs(
    title = "Evolución de precios de venta y préstamos UVA",
    x = "Trimestre", y = NULL
  ) +
  scale_color_manual(values = c("Venta (USD/m²)" = "steelblue", "Préstamos UVA (mil ARS)" = "darkred")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

graf_prestamos_venta

ggsave(filename = "images/graf_prestamos_venta.png", plot = graf_prestamos_venta, width = 8, height = 6, dpi = 300)

# 3. Correlacion entre ambos datos
df_venta_grafico2b <- df_venta %>%
  group_by(fecha_trim) %>%
  summarise(precio_prom = mean(precio_prom, na.rm = TRUE))

df_prestamos_grafico2b <- df_prestamos %>%
  group_by(fecha_trim) %>%
  summarise(monto = sum(monto, na.rm = TRUE))

df_corr <- inner_join(df_venta_grafico2b, df_prestamos_grafico2b, by = "fecha_trim")

graf_prestamos_venta_corr <- ggplot(df_corr, aes(x = precio_prom, y = monto, label = fecha_trim)) +
  geom_point(color = "steelblue", size = 3) +
  # geom_smooth(method = "lm", se = FALSE, color = "darkred", linetype = "dashed") +
  geom_text(vjust = -0.5, size = 3) +  # Etiqueta con el trimestre
  labs(
    title = "Correlación entre precio de venta y préstamos UVA",
    subtitle = "Por trimestre",
    x = "Precio promedio de venta (USD/m²)",
    y = "Préstamos UVA (mil ARS)"
  ) +
  theme_minimal()

graf_prestamos_venta_corr

ggsave(filename = "images/graf_prestamos_venta_corr.png", plot = graf_prestamos_venta_corr, width = 8, height = 6, dpi = 300)

# Top 10 barrios más caros (venta)
graf_venta_barrios <- df_venta %>%
  group_by(barrio) %>%
  summarise(promedio = mean(precio_prom, na.rm = TRUE)) %>%
  arrange(desc(promedio)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = promedio , y = reorder(barrio, promedio), fill = barrio)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 10 barrios más caros (venta USD/m²)", x = "USD por m² promedio entre 2010 y 2019", y = "Barrio") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() + theme(legend.position = "none")

graf_venta_barrios
ggsave(filename = "images/graf_venta_barrios.png", plot = graf_venta_barrios, width = 8, height = 6, dpi = 300)

# Evolución del precio de alquiler
graf_alquiler_tiempo <- df_alquiler %>%
  group_by(fecha) %>%
  summarise(precio_alq = mean(precio_prom, na.rm = TRUE)) %>%
  ggplot(aes(x = fecha, y = precio_alq, group = 1)) +
  geom_line(color = "forestgreen", linewidth = 1) +
  labs(title = "Evolución anual del precio promedio de alquiler (ARS)", x = "Año", y = "ARS") +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

graf_alquiler_tiempo
ggsave(filename = "images/graf_alquiler_tiempo.png", plot = graf_alquiler_tiempo, width = 8, height = 6, dpi = 300)

# Top 10 barrios más caros (alquiler)
graf_alquiler_barrios <- df_alquiler %>%
  group_by(barrio) %>%
  summarise(promedio = mean(precio_prom, na.rm = TRUE)) %>%
  arrange(desc(promedio)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = promedio , y = reorder(barrio, promedio), fill = barrio)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 10 barrios más caros (alquiler mensual)", x = "ARS promedio del 2015-2019", y = "Barrios") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() + theme(legend.position = "none")

graf_alquiler_barrios
ggsave(filename = "images/graf_alquiler_barrios.png", plot = graf_alquiler_barrios, width = 8, height = 6, dpi = 300)

# Superficie total disponible en alquiler
graf_superficie <- df_superficie %>%
  group_by(fecha) %>%
  summarise(total_m2 = sum(superficie, na.rm = TRUE)) %>%
  ggplot(aes(x = fecha, y = total_m2)) +
  geom_line(color = "purple", linewidth = 1) +
  labs(title = "Superficie total disponible en alquiler (m²)", x = "Fecha", y = "Metros cuadrados") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
graf_superficie
ggsave(filename = "images/graf_superficie.png", plot = graf_superficie, width = 8, height = 6, dpi = 300)

