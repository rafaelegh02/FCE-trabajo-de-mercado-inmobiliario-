# Script 3 - Visualización
script_3 <- '
library(tidyverse)
library(ggplot2)
library(scales)
library(lubridate)

df_venta <- readRDS("input/clean/venta_clean.rds")
df_alquiler <- readRDS("input/clean/alquiler_clean.rds")
df_superficie <- readRDS("input/clean/superficie_clean.rds")
df_prestamos <- readRDS("input/clean/prestamos_clean.rds")
