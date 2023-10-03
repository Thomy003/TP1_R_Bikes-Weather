
#Librerias ----
library(readr)
library(tidyverse)



#Dataframes ----

#Lectura de dataframes. Para leer los archivos csv usamos el comando read_csv.

clima_aeroparque_BA_2022 <- read_csv("Data/Clima_Aeroparque_BA.csv")

bike_trips_2022 <- read_csv("Data/trips_2022_reducido.csv")

#Filtramos el dataframe de bicis
bike_trips_2022_5_to_60_min <- bike_trips_2022 %>% 
  filter(between(duracion_recorrido, left = 60*5, right = 60*60))

#Dtaframe de bicis filtrado con 3 variables mas: mes, dia de la semana y horario
month_day_hour_bike_trips_2022_5_to_60_min <- bike_trips_2022_5_to_60_min %>% 
  mutate(date = fecha,
         month = months(fecha_origen_recorrido), 
         hour = hour(fecha_origen_recorrido), 
         week_day = weekdays(fecha),
         es_feriado = (fecha %in% c("2022-01-01", "2022-02-28", "2022-03-01", "2022-03-24", "2022-04-02", "2022-04-14", "2022-04-15", "2022-04-16", "2022-04-17", "2022-04-22", "2022-04-23", "2022-04-24", "2022-05-01", "2022-05-02", "2022-05-18", "2022-05-25", "2022-06-17", "2022-06-20", "2022-07-09", "2022-07-30", "2022-08-15", "2022-09-26", "2022-09-27", "2022-10-05", "2022-10-07", "2022-10-10", "2022-11-20", "2022-11-21", "2022-12-08", "2022-12-09", "2022-12-25" )))
         


#Dataframe del clima con 1 variables mas: mes

clima_aeroparque_BA_2022_bymonth_and_raintype <- clima_aeroparque_BA_2022 %>% 
  mutate(month = months(date),
         rain = (prcp > 0),
         tipo_lluvia = case_when(prcp > 0 & prcp <= 3 ~ "debil",
                                 prcp > 3.0 & prcp <= 6.5 ~ "ligera",
                                 prcp > 6.5 & prcp <= 16.0 ~ "moderada",
                                 prcp > 16.0 & prcp <= 40.0 ~ "fuerte",
                                 prcp > 100.0 ~ "torrencial",
                                 .default = "no llovio")) 




#Dataframe bike_trips_2022_5_to_60_min ----

#esto es un resumen del datarframe de bicis con viajes entre 5 a 20 minutos
summary_bike_trips_2022_5_to_60_min <- summary(month_day_hour_bike_trips_2022_5_to_60_min)

#visualizacion de viajes en minutos
trips_duration_in_minutes <- bike_trips_2022_5_to_60_min %>% 
  mutate(duracion_recorrido = duracion_recorrido/60) %>% 
  ggplot(mapping = aes(x = duracion_recorrido)) +
  geom_histogram(colour = "#E6AC6E", fill = "beige", bins = 11) +
  labs(x = "Duracion", y = "Cantidad de Recorridos", title = "Duracion de los recorridos en minutos")

#dataframe con el viaje promedio y la mediana
promedio_y_mediana_de_viajes <-  bike_trips_2022_5_to_60_min %>% 
  mutate(duracion_recorrido = duracion_recorrido/60) %>% 
  summarise("viaje promedio" = mean(duracion_recorrido), mediana = median(duracion_recorrido))

#visualizacion de viajes por mes
trips_by_month <- month_day_hour_bike_trips_2022_5_to_60_min %>% 
  ggplot(mapping = aes(x = fct_relevel(month, c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")))) +
  geom_bar(fill = "mintcream", colour = "lightskyblue") +
  labs(x = "Mes", y = "Cantidad de Recorridos")

#visualizacion de viajes por dia
trips_by_day <- month_day_hour_bike_trips_2022_5_to_60_min %>% 
  ggplot(mapping = aes(x = fct_relevel(week_day, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))))+
  geom_bar(fill = "mintcream", colour = "lightskyblue") +
  labs(x = "Dia de la semana", y = "Cantidad de recorridos")

#visualizacion de viajes por hora
trips_by_hour <- month_day_hour_bike_trips_2022_5_to_60_min %>% 
  ggplot(mapping = aes(x = hour)) +
  geom_bar(fill = "mintcream", colour = "lightskyblue") +
  labs(x = "Horario", y = "Cantidad de recorridos") 

#visualizacion de viajes por mes y hora
trips_by_month_and_hour <- month_day_hour_bike_trips_2022_5_to_60_min %>% 
  group_by(month) %>% 
  ggplot(mapping = aes(x = hour)) +
  geom_bar(fill = "mintcream",color = "lightskyblue") +
  facet_wrap(~fct_relevel(month, c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))) +
  labs(x = "Horario", y = "Cantidad de recorridos")

#visualizacion de viajes por dia y hora
trips_by_day_and_hour <- month_day_hour_bike_trips_2022_5_to_60_min %>% 
  group_by(week_day) %>% 
  ggplot(mapping = aes(x = hour)) +
  geom_bar(fill = "mintcream",color = "lightskyblue") +
  facet_wrap(~fct_relevel(week_day, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) +
  labs(x = "Horario", y = "Cantidad de recorridos")

#dataframe con la cantidad de estaciones de origen 
origin_trip <- bike_trips_2022_5_to_60_min %>% 
  select(id_estacion_origen, nombre_estacion_origen) %>% 
  group_by(id_estacion_origen, nombre_estacion_origen) %>% 
  summarise(amount_by_station = n()) %>% 
  arrange(desc(amount_by_station))

#dataframe con la cantidad de estaciones de llegada
destination_trip <- bike_trips_2022_5_to_60_min %>% 
  select(id_estacion_destino, nombre_estacion_destino) %>% 
  group_by(id_estacion_destino, nombre_estacion_destino) %>% 
  summarise(amount_by_station = n()) %>% 
  arrange(desc(amount_by_station)) 

#viajes mas comunes
common_trips <- bike_trips_2022_5_to_60_min %>% 
  select(id_estacion_origen, nombre_estacion_origen, id_estacion_destino, nombre_estacion_destino) %>% 
  group_by(id_estacion_origen, nombre_estacion_origen, id_estacion_destino, nombre_estacion_destino) %>% 
  summarise(amount = n()) %>% 
  arrange(desc(amount))

#tipo de bici
bike_type <- bike_trips_2022_5_to_60_min %>% 
  select(modelo_bicicleta) %>% 
  ggplot(mapping = aes(x = modelo_bicicleta, fill = modelo_bicicleta)) +
  geom_bar() +
  labs(x = "Modelo de bicicleta", y = "Cantidad de recorridos")

#uso de bicis por genero
use_of_bikes_by_gender <- bike_trips_2022_5_to_60_min %>% 
  select(Género) %>% 
  filter(!is.na(Género)) %>% 
  ggplot(mapping = aes(x = Género, fill = Género)) + 
  geom_bar() +
  labs(x = "Género", y = "Cantidad de recorridos")


#uso de bicis dia comunes vs feriados
#tenemos 31 feriados y (365 - 31) = 334 no feriados 
uso_feriado_vs_no_feriado <- month_day_hour_bike_trips_2022_5_to_60_min %>% 
  group_by(es_feriado) %>% 
  summarise(cant_viajes = n())

viajes_promedio_feriados <- c(uso_feriado_vs_no_feriado[1,2] / 334, uso_feriado_vs_no_feriado[2,2] / 31)



#Dataframe clima_aeroparque_BA_2022 ----

#dataframe summary del clima
summary_weather <- summary(clima_aeroparque_BA_2022_bymonth_and_raintype)

#grafico de temperaturas por mes
grafico_temperatura_por_mes <- clima_aeroparque_BA_2022_bymonth_and_raintype %>% 
  select(tmin, tmax, tavg, month) %>% 
  group_by(month) %>% 
  summarise(tmin_by_month = mean(tmin), tmax_by_month = mean(tmax), tavg_by_month = mean(tavg)) %>% 
  ggplot(mapping = aes(x = fct_relevel(month, c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")), y = tmax_by_month)) +
  geom_point(colour = "red", size = 4) +
  geom_point(aes(x = fct_relevel(month, c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")), y = tmin_by_month), color = "blue", size = 4) +
  labs(x = "Mes", y = "Temperatura")

#grafico de temperaturas por dia
grafico_temperatura_por_dia <- clima_aeroparque_BA_2022_bymonth_and_raintype %>%
  pivot_longer(cols = 3:4, names_to = "tipo", values_to = "temp") %>%
  ggplot(mapping = aes(x = date, y = temp, color = tipo)) +
  geom_line() +
  scale_color_manual(labels = c("Tmax", "Tmin"), values = c("red", "blue")) +
  labs(x = "Dia del año", y = "Temperatura max y min", color = "Temperatura") +
  theme_classic()

#precipitaciones por mes
precipitaciones_por_mes <- clima_aeroparque_BA_2022_bymonth_and_raintype %>% 
  group_by(month) %>% 
  filter(!is.na(prcp)) %>% 
  summarise(precipitation_by_month = mean(prcp)) %>% 
  ggplot(mapping = aes(x = fct_relevel(month, c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")), y = precipitation_by_month)) +
  geom_col(fill = "lightgoldenrod2", colour = "lightseagreen") +
  labs(x = "Mes",y = "Precipitacion por mes")


#dias de lluvias
grafico_dias_de_lluvias <- clima_aeroparque_BA_2022_bymonth_and_raintype %>% 
  filter(!is.na(prcp)) %>% 
  group_by(rain) %>% 
  summarise(cant = n()) %>% 
  ggplot(mapping = aes(x = rain, y = cant)) +
  geom_col()


#FULL JOIN DE DATAFRAMES
bikes_and_weather <- full_join(month_day_hour_bike_trips_2022_5_to_60_min, clima_aeroparque_BA_2022_bymonth_and_raintype, by = "date")

#uso de bicis por temperatura_promedio
bikes_by_temp <- bikes_and_weather %>% 
  mutate(t_avg_round = round(tavg)) %>% 
  group_by(t_avg_round) %>% 
  summarise(viajes_por_tavg = n()) %>% 
  ggplot(mapping = aes(x = t_avg_round, y = viajes_por_tavg)) +
  geom_line()


#uso de bicis por precipitaciones
uso_de_bicis_segun_precipitaciones <- bikes_and_weather %>% 
  filter(!is.na(prcp)) %>% 
  group_by(rain) %>% 
  summarise(viajes_promedios = n())

promedio_de_viajes_dias_lluviosos_vs_no <- c(uso_de_bicis_segun_precipitaciones[1,2]/266, uso_de_bicis_segun_precipitaciones[2,2]/97)


#uso de bicis segun la potencialidad de lluvias
grafico_de_uso_De_bicis_segun_lluvia <- clima_aeroparque_BA_2022_bymonth_and_raintype %>% 
  filter(tipo_lluvia != "no llovio") %>% 
  ggplot(mapping = aes(x = fct_relevel(tipo_lluvia, c("debil", "ligera", "moderada", "fuerte", "torrencial")))) +
  geom_bar()

bikes_and_weather %>% select(fecha, tipo_lluvia) %>% distinct() %>% group_by(tipo_lluvia) %>% summarise(cant = n())


bikes_and_weather %>% group_by(tipo_lluvia) %>% summarise(cant = n())
