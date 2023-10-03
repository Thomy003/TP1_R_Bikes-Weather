
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

#uso de bicis por hora según género
use_of_bikes_by_gender_per_hour <- month_day_hour_bike_trips_2022_5_to_60_min %>%
  filter(!is.na(Género)) %>%
  group_by(Género) %>%
  mutate(cantidadTotalPorGénero = n()) %>%
  group_by(Género,hour,cantidadTotalPorGénero) %>%
  summarise(cantidad = n()) %>%
  mutate(
    cantidadPorcentual = case_when(
      Género == "FEMALE" ~ (cantidad/cantidadTotalPorGénero)*100,
      Género == "MALE" ~ (cantidad/cantidadTotalPorGénero)*100,
      Género == "OTHER" ~ (cantidad/cantidadTotalPorGénero)*100
    )
  ) %>%
  select(Género,cantidadPorcentual) %>%
  ggplot(mapping = aes(x = hour, y = cantidadPorcentual, colour = Género)) +
  geom_line() +
  facet_wrap(~Género)


#uso de bicis dia comunes vs feriados
#tenemos 31 feriados y (365 - 31) = 334 no feriados 
uso_feriado_vs_no_feriado <- month_day_hour_bike_trips_2022_5_to_60_min %>% 
  group_by(es_feriado) %>% 
  summarise(cant_viajes = n())

viajes_promedio_feriados <- c(uso_feriado_vs_no_feriado[1,2] / 334, uso_feriado_vs_no_feriado[2,2] / 31)



#uso de bicis en días desetacados del año (arbitrario) ----
#para este caso seleccionamos arbitrariamente 8 días que están descritos en el informe.
#(nota: cp indica la cantidad de personas)
feriaLibro <- c("2022-04-28","2022-04-29","2022-04-30","2022-05-01","2022-05-02","2022-05-03","2022-05-04","2022-05-05","2022-05-06","2022-05-07","2022-05-08","2022-05-09","2022-05-10","2022-05-11","2022-05-12","2022-05-13","2022-05-14","2022-05-15","2022-05-16") #cp = 1.245.000
comicCon <- c("2022-05-20","2022-05-21","2022-05-22") #cp = 80 mil
duaLipa <- c("2022-09-13","2022-09-14") #cp = 50 mil
marchaLGBTQI <- c("2022-10-05") #cp = 130 mil
coldplay <- c("2022-10-25","2022-10-26","2022-10-28","2022-10-29","2022-11-01","2022-11-01","2022-11-02","2022-11-04","2022-11-05","2022-11-07","2022-11-08") #cp = 600 mil
primaveraSound <- c("2022-11-12","2022-11-13") #cp = 100 mil
finalMundial <- c("2022-12-18") #cp = 4 millones
feriadoMundial <- c("2022-12-20") #cp = ??
diasDestacados <- c(feriaLibro, comicCon, duaLipa, marchaLGBTQI, coldplay, primaveraSound, finalMundial, feriadoMundial)
# Feria del libro
bicisFeriaDelLibro <- month_day_hour_bike_trips_2022_5_to_60_min %>%
  filter(fecha %in% feriaLibro) %>%
  group_by(hour) %>%
  summarise(cantidad = round(n()/length(feriaLibro)))
# Comic Con
bicisComicCon <- month_day_hour_bike_trips_2022_5_to_60_min %>%
  filter(fecha %in% comicCon) %>%
  group_by(hour) %>%
  summarise(cantidad = round(n()/length(comicCon)))
# Dua Lipa
bicisDuaLipa <- month_day_hour_bike_trips_2022_5_to_60_min %>%
  filter(fecha %in% duaLipa) %>%
  group_by(hour) %>%
  summarise(cantidad = round(n()/length(duaLipa)))
# MarchaLGBT
bicisMarchaLGBT <- month_day_hour_bike_trips_2022_5_to_60_min %>%
  filter(fecha %in% marchaLGBTQI) %>%
  group_by(hour) %>%
  summarise(cantidad = round(n()/length(marchaLGBTQI)))
# Coldplay
bicisColdplay <- month_day_hour_bike_trips_2022_5_to_60_min %>%
  filter(fecha %in% coldplay) %>%
  group_by(hour) %>%
  summarise(cantidad = round(n()/length(coldplay)))
#PrimaveraSound
bicisPrimSound <- month_day_hour_bike_trips_2022_5_to_60_min %>%
  filter(fecha %in% primaveraSound) %>%
  group_by(hour) %>%
  summarise(cantidad = round(n()/length(primaveraSound)))
# Mundial
bicisFinalMundial <- month_day_hour_bike_trips_2022_5_to_60_min %>%
  filter(fecha %in% finalMundial) %>%
  group_by(hour) %>%
  summarise(cantidad = round(n()/length(finalMundial)))
# Feriado Mundial
bicisFeriadoMundial <- month_day_hour_bike_trips_2022_5_to_60_min %>%
  filter(fecha == feriadoMundial) %>%
  group_by(hour) %>%
  summarise(cantidad = round(n()/length(feriadoMundial)))
# Este solamente es un dato orientativo para analizar los viajes en cada evento
viajesPromedioFeriaLibro <- sum(bicisFeriaDelLibro$cantidad) # 24 viajes
viajesPromedioComicCon <- sum(bicisComicCon$cantidad) # 17 viajes
viajesPromedioDuaLipa <- sum(bicisDuaLipa$cantidad) # 44 viajes
viajesPromedioLGBT <- sum(bicisMarchaLGBT$cantidad) # 48 viajes
viajesPromedioColdplay <- sum(bicisColdplay$cantidad) # 31 viajes
viajesPromedioPrimSound <- sum(bicisPrimSound$cantidad) # 9 viajes
viajesPromedioMundial <- sum(bicisFinalMundial$cantidad) # 31 viajes
viajesPromedioFeriadoMundial <- sum(bicisFeriadoMundial$cantidad) # 27 viajes
# De mayor a menor, los eventos con mayor viajes promedio son: Marcha LGBT, concierto Dua Lipa, concierto Coldplay y Final del Mundial, Feria del libro, Comic Con y Primavera Sound
# Generamos un data frame con los viajes realizados según cada evento
dfViajesPromedioEventos <- data.frame(
  evento = c("Feria del Libro","Comic Con","Dua Lipa","Marcha LGBT","Coldplay","Primavera Sound","Mundial","Feriado mundial"),
  viajesPorEvento = c(viajesPromedioFeriaLibro,viajesPromedioComicCon,viajesPromedioDuaLipa,viajesPromedioLGBT,viajesPromedioColdplay,viajesPromedioPrimSound,viajesPromedioMundial,viajesPromedioFeriadoMundial)
)
graficoEventos <- dfViajesPromedioEventos %>%
  ggplot(mapping = aes(x = evento, y = viajesPorEvento)) +
  geom_col()



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
