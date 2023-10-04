
#Como no conocia la funcion built-in months, para separar por meses iba a utilizar esta funcion que me habia armado
t <- bike_trips_2022_5_to_60_min %>%
  mutate(date_character = as.character(fecha_origen_recorrido)) %>% 
  select(date_character) %>% 
  rowwise() %>% 
  mutate(month = case_when(grepl("2022-01", as.character(date_character)) == T ~ "Enero", 
                           grepl("2022-02", as.character(date_character)) == T ~ "Febrero",
                           grepl("2022-03", as.character(date_character)) == T ~ "Marzo",
                           grepl("2022-04", as.character(date_character)) == T ~ "Abril",
                           grepl("2022-05", as.character(date_character)) == T ~ "Mayo",
                           grepl("2022-06", as.character(date_character)) == T ~ "Junio",
                           grepl("2022-07", as.character(date_character)) == T ~ "Julio",
                           grepl("2022-08", as.character(date_character)) == T ~ "Agosto",
                           grepl("2022-09", as.character(date_character)) == T ~ "Septiembre",
                           grepl("2022-10", as.character(date_character)) == T ~ "Octubre",
                           grepl("2022-11", as.character(date_character)) == T ~ "Noviembre",
                           grepl("2022-12", as.character(date_character)) == T ~ "Diciembre"))





#MAPA
#FORMA 1----
#install.packages("mapproj")
#library(mapproj)
#library(maps)

#coord_map(projection = "mercator",
#xlim = 40,
# ylim = 50)

# Prepare a map of NZ
#ggplot(map_data("world"), aes(x = long, y = lat, group = group)) +
# geom_polygon(fill = "white", colour = "black") 

#bike_trips_2022_5_to_60_min %>% 
# select(long_estacion_origen, lat_estacion_origen) %>% 
# distinct() %>% 
#ggplot(mapping = aes(x = long_estacion_origen, y = lat_estacion_origen)) + 
# geom_point() + 
# ggplot(map_data("world"), aes(x = long, y = lat, group = group)) +
# geom_polygon(fill = "white", colour = "black") 



#---- FORMA2
#FORMA 2 ----
#library(ggplot2)
#library(sf)

# Load spatial data for Buenos Aires province (you may need to replace this with your own data)
# This example uses a simplified shapefile of Buenos Aires province from the 'maps' package.
# You can replace this with your own shapefile or spatial data source.
#buenos_aires <- st_read(system.file("shape/argentina/", "buenosaires.geojson", package = "sf"))

# Create a ggplot map of Buenos Aires province
#buenos_aires_map <- ggplot(buenos_aires) +
#geom_sf() +
#labs(title = "Map of Buenos Aires Province, Argentina") +
#theme_minimal()

# Use coord_quickmap() to create the map with the appropriate aspect ratio
#buenos_aires_map + coord_quickmap()









#Funciones No utilizadas durante las clases----

#rowwise -> to group data into individual rows. dplyr functions will compute results for each row 

#grepl(pattern, data) -> TRUE means pattern was in data, FALSE wasn't in data

#months() -> given a date it returns the month

#fct_relevel() -> permite modificar el orden en que aparecen las categorias en el geom_bar

#wday() -> recibe un valor de tipo date retorna un numero entre 1 a 7 donde 1 es domingo, 2 es lunes, ...




