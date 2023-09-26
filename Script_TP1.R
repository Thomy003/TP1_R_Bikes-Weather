
#Librerias ----
library(readr)
library(tidyverse)


#Lectura de Dataframes ----
library(readr)
clima_aeroparque_BA_2022 <- read_csv("Data/Clima_Aeroparque_BA.csv")
View(Clima_Aeroparque_BA)

bike_trips_2022 <- read_csv("Data/trips_2022_reducido.csv")
View(bike_trips_2022)

#Filtramos el dataframe de bicis
bike_trips_2022_5_to_60_min <- bike_trips_2022 %>% 
  filter(between(duracion_recorrido, left = 60*5, right = 60*60))

# ----
summary_bike_trips_2022_5_to_60_min <- summary(bike_trips_2022_5_to_60_min) #ESTO ESTA BIEN PONERLO?


tabla <- bike_trips_2022_5_to_60_min %>% 
  mutate(duracion_recorrido = duracion_recorrido/60) %>% 
  ggplot(mapping = aes(x = duracion_recorrido)) +
  geom_histogram(colour = "#E6AC6E", fill = "beige", bins = 11) +
  labs(x = "Duracion", y = "Cantidad", title = "Duracion de los recorridos en minutos")

promedio_y_mediana_de_viajes <-  bike_trips_2022_5_to_60_min %>% 
  mutate(duracion_recorrido = duracion_recorrido/60) %>% 
  summarise("viaje promedio" = mean(duracion_recorrido), mediana = median(duracion_recorrido))

month_trip <- bike_trips_2022_5_to_60_min %>% 
  select(fecha_origen_recorrido) %>% 
  mutate(month = months(fecha_origen_recorrido))

trips_by_month <- month_trip %>% 
  ggplot(mapping = aes(x = fct_relevel(month, c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")))) +
  geom_bar(fill = "mintcream", colour = "lightskyblue") +
  labs(x = "Mes", y = "Cantidad")

origin_trip <- bike_trips_2022_5_to_60_min %>% 
  select(id_estacion_origen, nombre_estacion_origen) %>% 
  group_by(id_estacion_origen, nombre_estacion_origen) %>% 
  summarise(amount_by_station = n()) %>% 
  arrange(desc(amount_by_station))

destination_trip <- bike_trips_2022_5_to_60_min %>% 
  select(id_estacion_destino, nombre_estacion_destino) %>% 
  group_by(id_estacion_destino, nombre_estacion_destino) %>% 
  summarise(amount_by_station = n()) %>% 
  arrange(desc(amount_by_station)) 

common_trips <- bike_trips_2022_5_to_60_min %>% 
  select(id_estacion_origen, nombre_estacion_origen, id_estacion_destino, nombre_estacion_destino) %>% 
  group_by(id_estacion_origen, nombre_estacion_origen, id_estacion_destino, nombre_estacion_destino) %>% 
  summarise(amount = n()) %>% 
  arrange(desc(amount))

bike_type <- bike_trips_2022_5_to_60_min %>% 
  select(modelo_bicicleta) %>% 
  ggplot(mapping = aes(x = modelo_bicicleta, fill = modelo_bicicleta)) +
  geom_bar()

use_of_bikes_by_gender <- bike_trips_2022_5_to_60_min %>% 
  select(Género) %>% 
  filter(!is.na(Género)) %>% 
  ggplot(mapping = aes(x = Género, fill = Género)) + 
  geom_bar()

summary_weather <- summary(clima_aeroparque_BA_2022)

month_climate <- clima_aeroparque_BA_2022 %>% 
  mutate(month = months(date))

grafico_temperatura_por_mes <- month_climate %>% 
  select(tmin, tmax, tavg, month) %>% 
  group_by(month) %>% 
  summarise(tmin_by_month = mean(tmin), tmax_by_month = mean(tmax), tavg_by_month = mean(tavg)) %>% 
  ggplot(mapping = aes(x = fct_relevel(month, c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")), y = tmax_by_month)) +
  geom_point(colour = "red", size = 4) +
  geom_point(aes(x = fct_relevel(month, c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")), y = tmin_by_month), color = "blue", size = 4)

grafico_temperatura_por_dia <- month_climate %>% ggplot(mapping = aes(x = date, y = tmax)) +
  geom_line(colour = "red") +
  geom_line(aes(y = tmin), colour = "blue")

precipitaciones_por_mes <- month_climate %>% 
  group_by(month) %>% 
  filter(!is.na(prcp)) %>% 
  summarise(precipitation_by_month = mean(prcp)) %>% 
  ggplot(mapping = aes(x = fct_relevel(month, c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")), y = precipitation_by_month)) +
  geom_col(fill = "lightgoldenrod2", colour = "lightseagreen")
