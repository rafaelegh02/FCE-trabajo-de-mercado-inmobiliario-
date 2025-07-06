# Script 2 - Transformación de datos
script_2 <- '
library(tidyverse)
library(janitor)
library(lubridate)

df_venta <- readRDS("input/venta.rds")
df_alquiler <- readRDS("input/alquiler.rds")
df_superficie <- readRDS("input/superficie.rds")
df_prestamos <- readRDS("input/prestamos.rds")

df_venta <- df_venta %>% mutate(
  precio_prom = as.numeric(precio_prom),
  ambientes = as.factor(ambientes),
  estado = as.factor(estado),
  fecha = paste0(ano, "-Q", trimestre),
  barrio = str_to_title(barrio)
) %>% filter(precio_prom > 500, precio_prom < 10000)

meses_esp <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
df_alquiler <- df_alquiler %>% mutate(
  anio = as.integer(anio),
  precio_prom = as.numeric(precio_prom),
  fecha = make_date(anio, match(mes, meses_esp), 1),
  ambientes = as.factor(ambientes),
  barrio = str_to_title(barrio)
) %>% filter(precio_prom > 10000, precio_prom < 1000000)

df_superficie <- df_superficie %>% rename(anio = ano) %>% mutate(
  superficie = as.numeric(superficie),
  fecha = make_date(anio, match(mes, meses_esp), 1),
  barrio = str_to_title(barrio)
) %>% filter(superficie > 50, superficie < 50000)

df_prestamos <- df_prestamos %>% rename(monto = monto_operado_prestamos_de_uva_en_miles_de_pesos) %>% mutate(
  monto = as.numeric(monto),
  fecha = ym(ano_mes)
) %>% select(fecha, monto)

if (!dir.exists("input/clean")) dir.create("input/clean")
saveRDS(df_venta, "input/clean/venta_clean.rds")
saveRDS(df_alquiler, "input/clean/alquiler_clean.rds")
saveRDS(df_superficie, "input/clean/superficie_clean.rds")
saveRDS(df_prestamos, "input/clean/prestamos_clean.rds")
'