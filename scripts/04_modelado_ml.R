# Gráficos

graf_venta_tiempo <- df_venta %>%
  group_by(fecha) %>%
  summarise(precio_m2 = mean(precio_prom, na.rm = TRUE)) %>%
  ggplot(aes(x = fecha, y = precio_m2, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(title = "Evolución del precio promedio de venta (USD/m²)", x = "Fecha", y = "USD por m²") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

graf_venta_barrios <- df_venta %>%
  group_by(barrio) %>%
  summarise(promedio = mean(precio_prom, na.rm = TRUE)) %>%
  arrange(desc(promedio)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(barrio, promedio), y = promedio)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(title = "Top 10 barrios más caros (venta USD/m²)", x = "Barrio", y = "USD por m²") +
  theme_minimal()

graf_alquiler_tiempo <- df_alquiler %>%
  group_by(fecha) %>%
  summarise(precio_alq = mean(precio_prom, na.rm = TRUE)) %>%
  ggplot(aes(x = fecha, y = precio_alq, group = 1)) +
  geom_line(color = "forestgreen", linewidth = 1) +
  labs(title = "Evolución anual del precio promedio de alquiler (ARS)", x = "Año", y = "ARS") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

graf_alquiler_barrios <- df_alquiler %>%
  group_by(barrio) %>%
  summarise(promedio = mean(precio_prom, na.rm = TRUE)) %>%
  arrange(desc(promedio)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(barrio, promedio), y = promedio)) +
  geom_col(fill = "skyblue") +
  coord_flip() +
  labs(title = "Top 10 barrios más caros (alquiler mensual)", x = "Barrio", y = "ARS") +
  theme_minimal()

graf_superficie <- df_superficie %>%
  group_by(fecha) %>%
  summarise(total_m2 = sum(superficie, na.rm = TRUE)) %>%
  ggplot(aes(x = fecha, y = total_m2)) +
  geom_line(color = "purple", linewidth = 1) +
  labs(title = "Superficie total disponible en alquiler (m²)", x = "Fecha", y = "Metros cuadrados") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

graf_prestamos <- df_prestamos %>%
  ggplot(aes(x = fecha, y = monto)) +
  geom_line(color = "red", linewidth = 1) +
  labs(title = "Monto de préstamos hipotecarios UVA otorgados", x = "Fecha", y = "Miles de pesos") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
'

# Guardar scripts
writeLines(script_1, "01_carga_y_limpieza.R")
writeLines(script_2, "02_transformacion.R")
writeLines(script_3, "03_visualizacion.R")