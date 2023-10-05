#CONSIDERACIONES ----

#1. Nuestras PC's trabajan con algunas variables en ingles y no en español, entonces si surgen problemas con el fct_level por ejemplo, es porque nosotros ordenamos las categorías en ingles y no español.

#2. Para tener una compresión mas rápida de los nombres de variables, utilizamos los siguientes prefijos "df_" para referirnos a un dataframe, "gr_" para referirnos a un gráfico y "v_" para referirnos a una variable.


#LIBRERIAS ----
library(readr)
library(tidyverse)


#VARIABLES ----

#Las fechas de los días feriados de Argentina 2022 los obtuvimos de la siguiente pagina: https://www.argentina.gob.ar/interior/feriados-nacionales-2022
v_feriados <- c("2022-01-01", "2022-02-28", "2022-03-01", "2022-03-24", "2022-04-02", "2022-04-14", "2022-04-15", "2022-04-16", "2022-04-17", "2022-04-22", "2022-04-23", "2022-04-24", "2022-05-01", "2022-05-02", "2022-05-18", "2022-05-25", "2022-06-17", "2022-06-20", "2022-07-09", "2022-07-30", "2022-08-15", "2022-09-26", "2022-09-27", "2022-10-05", "2022-10-07", "2022-10-10", "2022-11-20", "2022-11-21", "2022-12-08", "2022-12-09", "2022-12-25")

#(nota: cp indica la cantidad de personas)
v_feriaLibro <- c("2022-04-28","2022-04-29","2022-04-30","2022-05-01","2022-05-02","2022-05-03","2022-05-04","2022-05-05","2022-05-06","2022-05-07","2022-05-08","2022-05-09","2022-05-10","2022-05-11","2022-05-12","2022-05-13","2022-05-14","2022-05-15","2022-05-16") #cp = 1.245.000
v_comicCon <- c("2022-05-20","2022-05-21","2022-05-22") #cp = 80 mil
v_duaLipa <- c("2022-09-13","2022-09-14") #cp = 50 mil
v_marchaLGBTQI <- c("2022-10-05") #cp = 130 mil
v_coldplay <- c("2022-10-25","2022-10-26","2022-10-28","2022-10-29","2022-11-01","2022-11-02","2022-11-04","2022-11-05","2022-11-07","2022-11-08") #cp = 600 mil
v_primaveraSound <- c("2022-11-12","2022-11-13") #cp = 100 mil
v_finalMundial <- c("2022-12-18") #cp = 4 millones
v_feriadoMundial <- c("2022-12-20") #cp = ??

v_diasDestacados <- c(v_feriaLibro, v_comicCon, v_duaLipa, v_marchaLGBTQI, v_coldplay, v_primaveraSound, v_finalMundial, v_feriadoMundial)


# Feria del libro
v_bicisFeriaDelLibro <- as.numeric(df_bike_trips_2022_reformed %>%
                                     filter(date %in% v_feriaLibro) %>%
                                     summarise(cantidad = n()/length(v_feriaLibro)))
# Comic Con
v_bicisComicCon <- as.numeric(df_bike_trips_2022_reformed %>%
                                filter(date %in% v_comicCon) %>%
                                summarise(cantidad = round(n()/length(v_comicCon))))
# Dua Lipa
v_bicisDuaLipa <- as.numeric(df_bike_trips_2022_reformed %>%
                               filter(date %in% v_duaLipa) %>%
                               summarise(cantidad = round(n()/length(v_duaLipa))))
# MarchaLGBT
v_bicisMarchaLGBT <- as.numeric(df_bike_trips_2022_reformed %>%
                                  filter(date %in% v_marchaLGBTQI) %>%
                                  summarise(cantidad = round(n()/length(v_marchaLGBTQI))))
# Coldplay
v_bicisColdplay <- as.numeric(df_bike_trips_2022_reformed %>%
                                filter(date %in% v_coldplay) %>%
                                summarise(cantidad = round(n()/length(v_coldplay))))
#PrimaveraSound
v_bicisPrimSound <- as.numeric(df_bike_trips_2022_reformed %>%
                                 filter(date %in% v_primaveraSound) %>%
                                 summarise(cantidad = round(n()/length(v_primaveraSound))))
# Mundial
v_bicisFinalMundial <- as.numeric(df_bike_trips_2022_reformed %>%
                                    filter(date %in% v_finalMundial) %>%
                                    summarise(cantidad = round(n()/length(v_finalMundial))))
# Feriado Mundial
v_bicisFeriadoMundial <- as.numeric(df_bike_trips_2022_reformed %>%
                                      filter(date == v_feriadoMundial) %>%
                                      summarise(cantidad = round(n()/length(v_feriadoMundial))))

v_dia_promedio <- as.numeric(df_bike_trips_2022_reformed %>% 
                               filter(week_day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) %>% 
                               group_by(date) %>% 
                               summarise(cantidad = n()) %>% 
                               summarise(cant_prom = sum(cantidad) / length(date)))

v_dia_finde_promedio <- as.numeric(df_bike_trips_2022_reformed %>% 
                                     filter(week_day %in% c("Saturday", "Sunday")) %>% 
                                     group_by(date) %>% 
                                     summarise(cantidad = n()) %>% 
                                     summarise(cant_prom = sum(cantidad) / length(date)))

#DATAFRAMES
#Dataframes_originales ----

#Lectura de dataframes. Para leer los archivos csv usamos el comando read_csv.

df_clima_aeroparque_BA_2022 <- read_csv("Data/Clima_Aeroparque_BA.csv")

df_bike_trips_2022 <- read_csv("Data/trips_2022_reducido.csv")


#Dataframes_bicis_modificado ----
#Dataframe de bicis filtrado por duración del recorrido, con 4 variables mas ("month", "week_day", "hour" y "public_holiday") y la variable fecha renombrada a date.

df_bike_trips_2022_reformed <- df_bike_trips_2022 %>% 
  filter(between(duracion_recorrido, left = 60*5, right = 60*60)) %>% 
  rename(date = fecha) %>% 
  mutate(month = months(fecha_origen_recorrido), 
         hour = hour(fecha_origen_recorrido), 
         week_day = weekdays(date),
         public_holiday = (date %in% v_feriados))
         
#Dataframes_clima_modificado ----
#Dataframe del clima con 3 variables mas("month", "is_rain", "rain_strength")

df_clima_aeroparque_BA_2022_reformed <- df_clima_aeroparque_BA_2022 %>% 
  mutate(month = months(date),
         is_rain = (prcp > 0),
         rain_strength = case_when(prcp > 0 & prcp <= 3 ~ "debil",
                                 prcp > 3.0 & prcp <= 6.5 ~ "ligera",
                                 prcp > 6.5 & prcp <= 16.0 ~ "moderada",
                                 prcp > 16.0 & prcp <= 40.0 ~ "fuerte",
                                 prcp > 100.0 ~ "torrencial",
                                 .default = "no llovio")) 




#Dataframe_joined ----
#FULL JOIN DE DATAFRAMES
df_bikes_and_weather <- full_join(df_bike_trips_2022_reformed, df_clima_aeroparque_BA_2022_reformed, by = "date")

#Dataframes Descriptivos ----
#dataframe con la duracion de viaje promedio y mediana
df_promedio_y_mediana_de_viajes <-  df_bike_trips_2022_reformed %>% 
  mutate(duracion_recorrido = duracion_recorrido/60) %>% 
  summarise("promedio duracion de viaje" = mean(duracion_recorrido), "mediana de la duracion de viajes" = median(duracion_recorrido))


#GRAFICOS ----

#Dataframe df_bike_trips_2022_reformed
#visualizacion de viajes en minutos
gr_trips_duration_in_minutes_col <- df_bike_trips_2022_reformed %>% 
  mutate(duracion_recorrido = duracion_recorrido/60) %>% 
  ggplot(mapping = aes(x = duracion_recorrido)) +
  geom_histogram(colour = "#E6AC6E", fill = "beige", bins = 11) +
  labs(x = "Duracion", y = "Cantidad de Recorridos", title = "Duracion de los recorridos en minutos")

gr_trips_duration_in_minutes_box <- df_bike_trips_2022_reformed %>% 
  mutate(duracion_recorrido = duracion_recorrido/60) %>% 
  ggplot(mapping = aes(y = duracion_recorrido)) +
  geom_boxplot(color = "#E6AC6E", shape = 11) +
  labs(x = "recorridos", y = "Duracion", title = "Duracion de los recorridos en minutos") +
  scale_x_discrete(expand = c(0,0)) 

gr_trips_duration_in_minutes_by_gender <- df_bike_trips_2022_reformed %>% 
  filter(!is.na(Género)) %>% 
  mutate(duracion_recorrido = duracion_recorrido/60) %>% 
  ggplot(mapping = aes(y = duracion_recorrido, x = Género, color = Género)) +
  geom_boxplot() +
  labs(y = "Duracion", title = "Duracion de los recorridos en minutos segun genero")

#visualizacion de viajes por mes
gr_trips_by_month <- df_bike_trips_2022_reformed %>% 
  ggplot(mapping = aes(x = fct_relevel(month, c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")))) +
  geom_bar(fill = "mintcream", colour = "lightskyblue") +
  labs(x = "Mes", y = "Cantidad de Recorridos")

#visualizacion de viajes por dia
gr_trips_by_day <- df_bike_trips_2022_reformed %>% 
  ggplot(mapping = aes(x = fct_relevel(week_day, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))))+
  geom_bar(fill = "mintcream", colour = "lightskyblue") +
  labs(x = "Dia de la semana", y = "Cantidad de recorridos")

#visualizacion de viajes por hora
gr_trips_by_hour <- df_bike_trips_2022_reformed %>% 
  ggplot(mapping = aes(x = hour)) +
  geom_bar(fill = "mintcream", colour = "lightskyblue") +
  labs(x = "Horario", y = "Cantidad de recorridos") 

#visualizacion de viajes por mes y hora
gr_trips_by_month_and_hour <- df_bike_trips_2022_reformed %>% 
  group_by(month) %>% 
  ggplot(mapping = aes(x = hour)) +
  geom_bar(fill = "mintcream",color = "lightskyblue") +
  facet_wrap(~fct_relevel(month, c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))) +
  labs(x = "Horario", y = "Cantidad de recorridos")

#visualizacion de viajes por dia y hora
gr_trips_by_day_and_hour <- df_bike_trips_2022_reformed %>% 
  group_by(week_day) %>% 
  ggplot(mapping = aes(x = hour)) +
  geom_bar(fill = "mintcream",color = "lightskyblue") +
  facet_wrap(~fct_relevel(week_day, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) +
  labs(x = "Horario", y = "Cantidad de recorridos")

#dataframe con la cantidad de estaciones de origen 
df_origin_trip <- df_bike_trips_2022_reformed %>% 
  select(id_estacion_origen, nombre_estacion_origen) %>% 
  group_by(id_estacion_origen, nombre_estacion_origen) %>% 
  summarise(amount_by_station = n()) %>% 
  arrange(desc(amount_by_station))

#dataframe con la cantidad de estaciones de llegada
df_destination_trip <- df_bike_trips_2022_reformed %>% 
  select(id_estacion_destino, nombre_estacion_destino) %>% 
  group_by(id_estacion_destino, nombre_estacion_destino) %>% 
  summarise(amount_by_station = n()) %>% 
  arrange(desc(amount_by_station)) 

#viajes mas comunes
df_common_trips <- df_bike_trips_2022_reformed %>% 
  select(id_estacion_origen, nombre_estacion_origen, id_estacion_destino, nombre_estacion_destino) %>% 
  group_by(id_estacion_origen, nombre_estacion_origen, id_estacion_destino, nombre_estacion_destino) %>% 
  summarise(amount = n()) %>% 
  arrange(desc(amount))

#tipo de bici
gr_bike_type <- df_bike_trips_2022_reformed %>% 
  select(modelo_bicicleta) %>% 
  ggplot(mapping = aes(x = modelo_bicicleta, fill = modelo_bicicleta)) +
  geom_bar() +
  labs(x = "Modelo de bicicleta", y = "Cantidad de recorridos")

gr_bike_type_boxplot <- df_bike_trips_2022_reformed %>% 
  ggplot(mapping = aes(x = modelo_bicicleta, y = duracion_recorrido, fill = modelo_bicicleta)) +
  geom_boxplot() +
  labs(x = "Modelo de bicicleta", y = "Duracion de los recorridos en segundos")


#uso de bicis por genero
gr_use_of_bikes_by_gender <- df_bike_trips_2022_reformed %>% 
  select(Género) %>% 
  filter(!is.na(Género)) %>% 
  ggplot(mapping = aes(x = Género, fill = Género)) + 
  geom_bar() +
  labs(x = "Género", y = "Cantidad de recorridos")

#uso de bicis por hora según género
gr_use_of_bikes_by_gender_per_hour <- df_bike_trips_2022_reformed %>%
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
  geom_line(lwd = 1.49) + 
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  facet_wrap(~Género) +
  theme(legend.position = "bottom")


#uso de bicis dia comunes vs feriados
#tenemos 31 feriados y (365 - 31) = 334 no feriados 
df_uso_feriado_vs_no_feriado <- df_bike_trips_2022_reformed %>% 
  group_by(public_holiday) %>% 
  summarise(cant_viajes = n()) 

df_uso_feriado_vs_no_feriado_prom <- data.frame(es_feriado = c("feriado", "no feriado"), 
                                                viajes_promedio = c(as.numeric(df_uso_feriado_vs_no_feriado[2,2]) / 31, 
                                                                    as.numeric(df_uso_feriado_vs_no_feriado[1,2]) / 334))

                                            
gr_viajes_promedio_feriados <- df_uso_feriado_vs_no_feriado_prom %>% 
  ggplot(mapping = aes(x = es_feriado, y = viajes_promedio)) + 
  geom_col(fill = "white") +
  labs(x = "Dia Feriado", y = "Cantidad", title = "Cantidad de viajes diarios por feriados y no feriados")


#uso de bicis en días desetacados del año (arbitrario) ----
df_Viajes_Promedio_Eventos_No_Finde <- data.frame(evento = c("Feria del Libro","Dua Lipa","Marcha LGBT","Coldplay","Feriado mundial", "Dia Prom"),
                                                  viajesPorEvento = c(v_bicisFeriaDelLibro, v_bicisDuaLipa, v_bicisMarchaLGBT, v_bicisColdplay,v_bicisFeriadoMundial, v_dia_promedio))

gr_Eventos_Dia_De_Semana <- df_Viajes_Promedio_Eventos_No_Finde %>%
  ggplot(mapping = aes(x = evento, y = viajesPorEvento)) +
  geom_col()


df_Viajes_Promedio_Eventos_Finde <- data.frame(evento = c("Comic Con", "Primavera Sound", "Mundial", "Dia Promedio"),
                                                  viajesPorEvento = c(v_bicisComicCon, v_bicisPrimSound, v_bicisFinalMundial, v_dia_finde_promedio))

gr_Eventos_Fin_De_Semana <- df_Viajes_Promedio_Eventos_Finde %>%
  ggplot(mapping = aes(x = evento, y = viajesPorEvento)) +
  geom_col()

#Dataframe df_clima_aeroparque_BA_2022 ----

#dataframe summary del clima
summary(df_clima_aeroparque_BA_2022_reformed)

#grafico de temperaturas por mes
gr_temperatura_por_mes <- df_clima_aeroparque_BA_2022_reformed %>% 
  select(tmin, tmax, tavg, month) %>% 
  group_by(month) %>% 
  summarise(tmin_by_month = mean(tmin), tmax_by_month = mean(tmax), tavg_by_month = mean(tavg)) %>% 
  ggplot(mapping = aes(x = fct_relevel(month, c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")), y = tmax_by_month)) +
  geom_point(colour = "red", size = 4) +
  geom_point(aes(x = fct_relevel(month, c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")), y = tmin_by_month), color = "blue", size = 4) +
  labs(x = "Mes", y = "Temperatura")

#grafico de temperaturas por dia
gr_temperatura_por_dia <- df_clima_aeroparque_BA_2022_reformed %>%
  pivot_longer(cols = 3:4, names_to = "tipo", values_to = "temp") %>%
  ggplot(mapping = aes(x = date, y = temp, color = tipo)) +
  geom_line() +
  scale_color_manual(labels = c("Tmax", "Tmin"), values = c("red", "blue")) +
  labs(x = "Dia del año", y = "Temperatura max y min", color = "Temperatura") +
  theme_classic()

#precipitaciones por mes
gr_precipitaciones_por_mes <- df_clima_aeroparque_BA_2022_reformed %>% 
  group_by(month) %>% 
  filter(!is.na(prcp)) %>% 
  summarise(precipitation_by_month = mean(prcp)) %>% 
  ggplot(mapping = aes(x = fct_relevel(month, c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")), y = precipitation_by_month)) +
  geom_col(fill = "lightgoldenrod2", colour = "lightseagreen") +
  labs(x = "Mes",y = "Precipitacion por mes")


#uso de bicis por temperatura_promedio
gr_bikes_by_temp <- df_bikes_and_weather %>% 
  mutate(t_avg_round = round(tavg)) %>% 
  group_by(t_avg_round) %>% 
  summarise(viajes_por_tavg = n()) %>% 
  ggplot(mapping = aes(x = t_avg_round, y = viajes_por_tavg)) +
  geom_line()


#uso de bicis por precipitaciones
df_uso_de_bicis_segun_precipitaciones <- df_bikes_and_weather %>% 
  filter(!is.na(prcp)) %>% 
  group_by(is_rain) %>% 
  summarise(viajes_lluvia = n())

df_uso_de_bicis_segun_precipitaciones_prom <- data.frame(es_lluvia = c("llovio", "no llovio"), 
                                                viajes_promedio = c(as.numeric(df_uso_de_bicis_segun_precipitaciones[2,2]) / 97, 
                                                                    as.numeric(df_uso_de_bicis_segun_precipitaciones[1,2]) / 266))


gr_uso_de_bicis_segun_precipitaciones <- df_uso_de_bicis_segun_precipitaciones_prom %>% 
  ggplot(mapping = aes(x = es_lluvia, y = viajes_promedio, color = es_lluvia)) + 
  geom_col(fill = "white") +
  labs(x = "Llovio?", y = "Cantidad", title = "Cantidad de viajes diarios durante dias de lluvia y no lluvia")


#uso de bicis segun la potencialidad de lluvias
gr_potencia_lluvia_2022 <- df_clima_aeroparque_BA_2022_reformed %>% 
  filter(rain_strength != "no llovio") %>% 
  ggplot(mapping = aes(x = fct_relevel(rain_strength, c("debil", "ligera", "moderada", "fuerte", "torrencial")))) +
  geom_bar()

df_bikes_and_weather %>% select(date, rain_strength) %>% distinct() %>% group_by(rain_strength) %>% summarise(cant = n())


df_bikes_and_weather %>% group_by(rain_strength) %>% summarise(cant = n())


gr_duracion_recorrido_segun_lluvia <- df_bikes_and_weather %>% 
  filter(!is.na(prcp)) %>% 
  filter(!is.na(duracion_recorrido)) %>% 
  group_by(rain_strength) %>% 
  ggplot(mappin = aes(x = rain_strength, y = duracion_recorrido, color = rain_strength)) +
  geom_boxplot()
