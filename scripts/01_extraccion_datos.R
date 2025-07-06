# Script 1 - Carga y limpieza de datos
script_1 <- '
# == INFORMACIÓN DEL ESTUDIANTE ===================================
# Evelyn Condori – Legajo: 882910
# Ezequiel Fontana – Legajo: 889309
# Rafael González – Legajo: 909454

# == CARGA DE LIBRERÍAS Y CONFIGURACIÓN ===========================
setwd("~/Ciencia de Datos 2025/FCE-trabajo-de-mercado-inmobiliario-")

library(tidyverse)
library(janitor)
library(lubridate)
library(ggplot2)
library(scales)

# == CARGA DE DATOS ===============================================
df_venta <- read_delim("raw/precio-venta-deptos.csv", delim = ";",
                       locale = locale(decimal_mark = ",", grouping_mark = "."), show_col_types = FALSE) %>% clean_names()

df_alquiler <- read_delim("raw/precio-alquiler-deptos.csv", delim = ";",
                          locale = locale(decimal_mark = ",", grouping_mark = "."), show_col_types = FALSE) %>% clean_names()

df_superficie <- read_delim("raw/superficie-deptos.csv", delim = ";",
                            locale = locale(decimal_mark = ",", grouping_mark = "."), show_col_types = FALSE) %>% clean_names()

df_prestamos <- read_delim("raw/montos-creditos-hipotecarios-uva.csv", delim = ";",
                           locale = locale(decimal_mark = ",", grouping_mark = "."), show_col_types = FALSE) %>% clean_names()

df_actividad <- read_delim("raw/actas-notariales-compra-venta-inmuebles-hipotecas.csv", delim = ";",
                           locale = locale(decimal_mark = ",", grouping_mark = "."), show_col_types = FALSE) %>% clean_names()

# == GUARDADO TEMPORAL ============================================
if (!dir.exists("input")) dir.create("input")
saveRDS(df_venta, "input/venta.rds")
saveRDS(df_alquiler, "input/alquiler.rds")
saveRDS(df_superficie, "input/superficie.rds")
saveRDS(df_prestamos, "input/prestamos.rds")
saveRDS(df_actividad, "input/actividad.rds")
'