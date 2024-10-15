# This file shows how to connect to odk central to download data collected using odk collect.
# Later it shows with example how to display plots and maps
# Inti Luna
# 2024-10-15

# Enable the rOpenSci universe
options(repos = c(
  ropensci = "https://ropensci.r-universe.dev",
  CRAN = "https://cloud.r-project.org"
))
#install.packages("ruODK")

library(ggplot2)
library(dplyr)
library(ruODK)
library(leaflet)


svc <- "https://geobcn2024odcentral.chickenkiller.com/v1/projects/1/forms/hn_evento.svc"
un <- "inti.luna.aviles@gmail.com"
pw <- "geobcn2024"
tz <- NULL
#tz <- "America/Managua"
verbose_var <- TRUE


# Data access and setup ----
# https://docs.ropensci.org/ruODK/reference/ru_setup.html

ruODK::ru_setup(
  svc = svc,
  un = un,
  pw = pw,
  tz = tz,
  verbose = verbose_var # great for demo or debugging
)

srv <- ruODK::odata_service_get()
srv %>% knitr::kable(.)

# Form submissions
data <- ruODK::odata_submission_get(local_dir = fs::path("vignettes/media"),
                                    odkc_version = get_test_odkc_version(),
                                    parse=T)
# # str(data,1)
names(data)
data$ha_est
data$lat
data$gps

# Plot 1 ----
# group by event
data_eventos <- data %>%
  group_by(evento) %>%
  summarise(cantidad_casos = n())

# create histogram
ggplot(data_eventos, aes(x = evento, y = cantidad_casos)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Cantidad de casos por tipo de evento",
       x = "Tipo de evento",
       y = "Cantidad de casos") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 2 ----
# ensure fecha_evento as date
data <- data %>%
  mutate(fecha_evento = as.Date(fecha_evento))

data_fechas_eventos <- data %>%
  group_by(fecha_evento, evento) %>%
  summarise(total_ha_est = sum(ha_est, na.rm = TRUE)) 

# barplot
ggplot(data_fechas_eventos, aes(x = fecha_evento, y = total_ha_est, fill = evento)) +
  geom_bar(stat = "identity", position = "dodge") +  # Barras en posición 'dodge' para separarlas
  theme_minimal() +
  labs(title = "Hectáreas afectadas por fecha y tipo de evento",
       x = "Fecha del evento",
       y = "Hectáreas afectadas",
       fill = "Tipo de evento") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar las etiquetas del eje X para mejor legibilidad

# Create mapa ----

# explore data
data$gps_latitude
data$gps_longitude

# create colorpalette
pal <- colorFactor(topo.colors(10), domain = data$evento)
pal


# create map with markers
leaflet(data) %>%
  addTiles() %>%  
  addCircleMarkers(~gps_longitude, ~gps_latitude, 
                   color = ~pal(evento), 
                   popup = ~evento, 
                   radius = 5, 
                   fillOpacity = 0.7) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~evento, 
            title = "Tipo de evento")

# change max zoom
leaflet(data) %>%
  addTiles(options = tileOptions(maxZoom = 22)) %>%  # Permitir zoom hasta el nivel 22
  addCircleMarkers(~gps_longitude, ~gps_latitude, 
                   color = ~pal(evento),  
                   popup = ~evento, 
                   radius = 5, 
                   fillOpacity = 0.7, 
                   stroke = FALSE, 
                   fillColor = ~pal(evento)) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~evento, 
            title = "Tipo de evento") %>%
  fitBounds(~min(gps_longitude), ~min(gps_latitude), ~max(gps_longitude), ~max(gps_latitude))  # Ajustar los límites del mapa según tus puntos

# manually set colors
# #color for each event
mis_colores <- c("te_caza" = "red", "te_deforestacion" = "blue", "Evento C" = "green")
pal <- colorFactor(mis_colores, domain = data$evento)

leaflet(data) %>%
  addTiles(options = tileOptions(maxZoom = 22)) %>%  
  addCircleMarkers(~gps_longitude, ~gps_latitude, 
                   color = ~pal(evento),  
                   popup = ~evento, 
                   radius = 5, 
                   fillOpacity = 0.7, 
                   stroke = FALSE, 
                   fillColor = ~pal(evento)) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~evento, 
            title = "Tipo de evento") %>%
  fitBounds(~min(gps_longitude), ~min(gps_latitude), ~max(gps_longitude), ~max(gps_latitude))  # Ajustar los límites del mapa según tus puntos

