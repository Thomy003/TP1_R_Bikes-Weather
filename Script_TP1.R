#CONSIDERACIONES ----

#1. Nuestras PC's trabajan con las variables en ingles y no en español, entonces si surgen problemas con el fct_level por ejemplo, es porque nosotros ordenamos las categorías en ingles y no español.

#2. Para tener una compresión mas rápida de los nombres de variables, utilizamos los siguientes prefijos "df_" para referirnos a un dataframe, "gr_" para referirnos a un gráfico y "v_" para referirnos a una variable.


#LIBRERIAS ----
library(readr)
library(tidyverse)




#DATAFRAMES
#Dataframes_originales ----

#Lectura de dataframes. Para leer los archivos csv usamos el comando read_csv.

df_clima_aeroparque_BA_2022 <- read_csv("Data/Clima_Aeroparque_BA.csv")

df_bike_trips_2022 <- read_csv("Data/trips_2022_reducido.csv")

#VARIABLES ----

v_months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

v_all_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

#Las fechas de los días feriados de Argentina 2022 los obtuvimos de la siguiente pagina: https://www.argentina.gob.ar/interior/feriados-nacionales-2022
v_feriados <- c("2022-01-01", "2022-02-28", "2022-03-01", "2022-03-24", "2022-04-02", "2022-04-14", "2022-04-15", "2022-04-16", "2022-04-17", "2022-04-22", "2022-04-23", "2022-04-24", "2022-05-01", "2022-05-02", "2022-05-18", "2022-05-25", "2022-06-17", "2022-06-20", "2022-07-09", "2022-07-30", "2022-08-15", "2022-09-26", "2022-09-27", "2022-10-05", "2022-10-07", "2022-10-10", "2022-11-20", "2022-11-21", "2022-12-08", "2022-12-09", "2022-12-25")

#(nota: cp indica la cantidad de personas)
v_feriaLibro <- c("2022-04-28","2022-04-29","2022-05-02","2022-05-03","2022-05-04","2022-05-05","2022-05-06","2022-05-09","2022-05-10","2022-05-11","2022-05-12","2022-05-13","2022-05-16") #cp = 1.245.000
v_feriaLibro_finde <- c("2022-04-30","2022-05-01","2022-05-07","2022-05-08", "2022-05-14","2022-05-15")
v_comicCon <- c("2022-05-20","2022-05-21","2022-05-22") #cp = 80 mil
v_duaLipa <- c("2022-09-13","2022-09-14") #cp = 50 mil
v_marchaLGBTQI <- c("2022-10-05") #cp = 130 mil
v_coldplay <- c("2022-10-25","2022-10-26","2022-10-28","2022-11-01","2022-11-02","2022-11-04","2022-11-07","2022-11-08") #cp = 600 mil
v_coldplay_finde <- c("2022-10-29", "2022-11-05")
v_primaveraSound <- c("2022-11-12","2022-11-13") #cp = 100 mil
v_finalMundial <- c("2022-12-18") #cp = 4 millones
v_feriado_del_Mundial <- c("2022-12-20") 


v_eventos_ord <- c("Marcha LGBT", "Dua Lipa", "Coldplay", "Feriado del mundial", "Feria del Libro", "Dia Promedio")

v_eventos_finde_ord <- c("Comic Con", "Mundial", "Primavera Sound", "Coldplay", "Feria del Libro", "Dia Promedio")

v_rain_strength <- c("debil", "ligera", "moderada", "fuerte", "torrencial")

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

#cantidad de usos de bicis en dias feriado vs no feriados
df_uso_feriado_vs_no_feriado <- df_bike_trips_2022_reformed %>% 
  group_by(public_holiday) %>% 
  summarise(cant_viajes = n()) 

#cantidad de viajes diarios en dias feriado vs no feriados (tenemos 31 feriados y (365 - 31) = 334 no feriados)\
df_uso_feriado_vs_no_feriado_prom <- data.frame(es_feriado = c("feriado", "no feriado"), 
                                                viajes_promedio = c(as.numeric(df_uso_feriado_vs_no_feriado[2,2]) / 31, 
                                                                    as.numeric(df_uso_feriado_vs_no_feriado[1,2]) / 334))


# Feria del libro
v_bicisFeriaDelLibro <- as.numeric(df_bike_trips_2022_reformed %>%
                                     filter(date %in% v_feriaLibro) %>%
                                     summarise(cantidad = n()/length(v_feriaLibro)))

v_bicisFeriaDelLibro_finde <- as.numeric(df_bike_trips_2022_reformed %>%
                                     filter(date %in% v_feriaLibro_finde) %>%
                                     summarise(cantidad = n()/length(v_feriaLibro_finde)))


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

v_bicisColdplay_finde <- as.numeric(df_bike_trips_2022_reformed %>%
                                   filter(date %in% v_coldplay_finde) %>%
                                   summarise(cantidad = round(n()/length(v_coldplay_finde))))

#PrimaveraSound
v_bicisPrimSound <- as.numeric(df_bike_trips_2022_reformed %>%
                                 filter(date %in% v_primaveraSound) %>%
                                 summarise(cantidad = round(n()/length(v_primaveraSound))))
# Mundial
v_bicisFinalMundial <- as.numeric(df_bike_trips_2022_reformed %>%
                                    filter(date %in% v_finalMundial) %>%
                                    summarise(cantidad = round(n()/length(v_finalMundial))))
# Feriado del Mundial
v_bicisFeriadoDelMundial <- as.numeric(df_bike_trips_2022_reformed %>%
                                      filter(date == v_feriado_del_Mundial) %>%
                                      summarise(cantidad = round(n()/length(v_feriado_del_Mundial))))

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

#viajes diarios durante los eventos que ocurrieron durante dias de la semana mayoritariamente
df_Viajes_Promedio_Eventos_No_Finde <- data.frame(evento = v_eventos_ord,
                                                  viajesPorEvento = c(v_bicisMarchaLGBT,
                                                                      v_bicisDuaLipa,
                                                                      v_bicisColdplay,
                                                                      v_bicisFeriadoDelMundial, 
                                                                      v_bicisFeriaDelLibro, 
                                                                      v_dia_promedio))

#viajes diarios durante los eventos que ocurrieron durante dias de fin de semana mayoritariamente
df_Viajes_Promedio_Eventos_Finde <- data.frame(evento = v_eventos_finde_ord,
                                               viajesPorEvento = c(v_bicisComicCon, 
                                                                   v_bicisFinalMundial,
                                                                   v_bicisPrimSound, 
                                                                   v_bicisColdplay_finde, 
                                                                   v_bicisFeriaDelLibro_finde, 
                                                                   v_dia_finde_promedio))


#cantidad de viajes durante dias de lluvia y no lluvia
df_uso_de_bicis_segun_precipitaciones <- df_bikes_and_weather %>% 
  filter(!is.na(prcp)) %>% 
  group_by(is_rain) %>% 
  summarise(viajes_lluvia = n())

#cantidad de viajes diarios durante dias de lluvia y no lluvia
df_uso_de_bicis_segun_precipitaciones_prom <- data.frame(es_lluvia = c("llovio", "no llovio"), 
                                                         viajes_promedio = c(as.numeric(df_uso_de_bicis_segun_precipitaciones[2,2]) / 97, 
                                                                             as.numeric(df_uso_de_bicis_segun_precipitaciones[1,2]) / 266))



#GRAFICOS ----

#visualizacion de duracion de los viajes en minutos version boxplot
gr_trips_duration_in_minutes_box <- df_bike_trips_2022_reformed %>% 
  mutate(duracion_recorrido = duracion_recorrido/60) %>% 
  ggplot(mapping = aes(y = duracion_recorrido)) +
  geom_boxplot(color = "#E6AC6E", shape = 11) +
  labs(x = "Recorridos", y = "Duracion (min)", title = "Duracion de los recorridos en minutos") +
  scale_x_discrete(expand = c(0,0)) 

#visualizacion de cantidad de viajes segun duracion en minutos
gr_trips_duration_in_minutes_col <- df_bike_trips_2022_reformed %>% 
  mutate(duracion_recorrido = duracion_recorrido/60) %>% 
  ggplot(mapping = aes(x = duracion_recorrido)) +
  geom_histogram(colour = "#E6AC6E", fill = "beige", bins = 11) +
  labs(x = "Duracion (min)", y = "Cantidad de Recorridos", title = "Duracion de los recorridos en minutos")

#visualizacion de viajes por mes
gr_trips_by_month <- df_bike_trips_2022_reformed %>% 
  ggplot(mapping = aes(x = fct_relevel(month, v_months))) +
  geom_bar(fill = "#B6C8D9", colour = "#6FA3AE") +
  labs(x = "Mes", y = "Cantidad", title = "Cantidad de recorridos durante cada mes del año")

#visualizacion de viajes por dia
gr_trips_by_day <- df_bike_trips_2022_reformed %>% 
  ggplot(mapping = aes(x = fct_relevel(week_day, v_all_days)))+
  geom_bar(fill = "#B6C8D9", colour = "#6FA3AE") +
  labs(x = "Dia de la semana", y = "Cantidad", title = "Cantidad de recorridos durante cada dia de la semana")

#visualizacion de viajes por hora
gr_trips_by_hour <- df_bike_trips_2022_reformed %>% 
  ggplot(mapping = aes(x = hour)) +
  geom_bar(fill = "#B6C8D9", colour = "#6FA3AE") +
  labs(x = "Horario", y = "Cantidad", title = "Cantidad de recorridos durante cada hora del dia") 

#visualizacion de viajes por mes y hora
gr_trips_by_month_and_hour <- df_bike_trips_2022_reformed %>% 
  group_by(month) %>% 
  ggplot(mapping = aes(x = hour)) +
  geom_bar(fill = "#CBA872",color = "#A05806") +
  facet_wrap(~fct_relevel(month, v_months)) +
  labs(x = "Horario", y = "Cantidad", title = "Cantidad de recorricos por mes segun horario")

gr_trips_by_season_and_hour <- df_bike_trips_2022_reformed %>% 
  mutate(season = case_when(date >= "2022-03-21" & date <= "2022-06-20" ~ "Autumn",
                            date >= "2022-06-21" & date <= "2022-09-20" ~ "Winter",
                            date >= "2022-09-21" & date <= "2022-12-20" ~ "Spring",
                            .default = "Summer")) %>% 
  ggplot(mapping = aes(x = hour)) +
  geom_bar(fill = "#CBA872",color = "#A05806") +
  facet_wrap(~fct_relevel(season, c("Summer", "Autumn", "Winter", "Spring"))) +
  labs(x = "Horario", y = "Cantidad", title = "Cantidad de recorricos por mes segun horario")

#visualizacion de viajes por dia y hora
gr_trips_by_day_and_hour <- df_bike_trips_2022_reformed %>% 
  group_by(week_day) %>% 
  ggplot(mapping = aes(x = hour)) +
  geom_bar(fill = "#CBA872",color = "#A05806") +
  facet_wrap(~fct_relevel(week_day, v_all_days), nrow = 2) +
  labs(x = "Horario", y = "Cantidad", title = "Cantidad de recorricos por dia segun horario")



#visualizacion de viajes promedio de dias feriados y no feriados
gr_viajes_promedio_feriados <- df_uso_feriado_vs_no_feriado_prom %>% 
  ggplot(mapping = aes(x = es_feriado, y = viajes_promedio)) + 
  geom_col(fill = c("#d4e09b", "#cbdfbd"), color = "black") +
  labs(x = "", y = "Viajes diarios", title = "Cantidad de viajes diarios 'Feriados Vs No Feriados'")



#visualizacion cantidad de viajes segun tipo de bici
gr_bike_type <- df_bike_trips_2022_reformed %>% 
  select(modelo_bicicleta) %>% 
  ggplot(mapping = aes(x = modelo_bicicleta)) +
  geom_bar(fill = c("#588157", "#52796f")) +
  labs(x = "Modelo de bicicleta", y = "Cantidad de recorridos")

#visualizacion cantidad de viajes segun tipo de bici
gr_bike_type_boxplot <- df_bike_trips_2022_reformed %>% 
  ggplot(mapping = aes(x = modelo_bicicleta, y = duracion_recorrido, fill = modelo_bicicleta)) +
  geom_boxplot() +
  labs(x = "Modelo de bicicleta", y = "Duracion (seg)", title = "Duracion de los viajes segun tipo de bici")


#visualizacion cantidad de usos de bicis segun genero
gr_use_of_bikes_by_gender <- df_bike_trips_2022_reformed %>% 
  select(Género) %>% 
  filter(!is.na(Género)) %>% 
  ggplot(mapping = aes(x = Género, fill = Género)) + 
  geom_bar() +
  labs(x = "Género", y = "Cantidad", title = "Cantidad de viajes en bici segun genero")

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
  theme(legend.position = "bottom") +
  labs(x = "Horario", y = "Porcentaje", title = "Cantidad porcentual sobre el uso de bicis segun genero y horario")

#visualizacion de duracion de los viajes en minutos version boxplot segun genero
gr_trips_duration_in_minutes_by_gender <- df_bike_trips_2022_reformed %>% 
  filter(!is.na(Género)) %>% 
  mutate(duracion_recorrido = duracion_recorrido/60) %>% 
  ggplot(mapping = aes(y = duracion_recorrido, x = Género, color = Género)) +
  geom_boxplot() +
  labs(y = "Duracion", title = "Duracion de los recorridos en minutos segun genero")


#visualizacion de usos de bicis promedio comparado con dias de eventos (dias de semana)
gr_Eventos_Dia_De_Semana <- df_Viajes_Promedio_Eventos_No_Finde %>%
  ggplot(mapping = aes(x = evento, y = viajesPorEvento, fill = evento)) +
  geom_col() +
  labs(x = "Eventos/Dia Promedio", y = "Viajes diarios", title = "Uso de bicis dias de la semana 'Eventos'", subtitle = "Cantidad de usos de bicis diarios durante dias normales y dias de evento que ocurrieron durante el lunes y viernes") +
  scale_x_discrete(limits = v_eventos_ord) +
  scale_fill_manual(values = c("Marcha LGBT" = "#ef476f", 
                               "Dua Lipa" = "#f78c6b", 
                               "Coldplay" = "#ffd166",
                               "Feriado del mundial" = "#06d6a0",
                               "Feria del Libro" =  "#118ab2",
                               "Dia Promedio" = "#073b4c"), 
                    breaks = v_eventos_ord)
  

#visualizacion de usos de bicis promedio comparado con dias de eventos (fin de semana)
gr_Eventos_Fin_De_Semana <- df_Viajes_Promedio_Eventos_Finde %>%
  ggplot(mapping = aes(x = evento, y = viajesPorEvento, fill = evento)) +
  geom_col() +
  labs(x = "Eventos/Dia Promedio", y = "Viajes diarios", title = "Uso de bicis diarios fin de semana 'Eventos'", subtitle = "Cantidad de usos de bicis diarios durante dias normales y dias de evento que ocurrieron durante el fin de semana") +
  scale_x_discrete(limits = v_eventos_finde_ord) +
    scale_fill_manual(values = c("Comic Con" = "#ef476f", 
                               "Mundial" = "#f78c6b", 
                               "Primavera Sound" = "#ffd166",
                               "Coldplay" = "#06d6a0",
                               "Feria del Libro" =  "#118ab2",
                               "Dia Promedio" = "#073b4c"), 
                    breaks = v_eventos_finde_ord)


#grafico de temperaturas por mes
gr_temperatura_por_mes <- df_clima_aeroparque_BA_2022_reformed %>% 
  select(tmin, tmax, tavg, month) %>% 
  group_by(month) %>% 
  summarise(tmin_by_month = mean(tmin), tmax_by_month = mean(tmax), tavg_by_month = mean(tavg)) %>%
  pivot_longer(cols = 2:4, names_to = "tipo", values_to = "temp") %>% 
  ggplot(mapping = aes(x = fct_relevel(month, v_months), y = temp, fill = fct_relevel(tipo, "tmax_by_month", "tavg_by_month"))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Clima promedio segun mes", x = "Mes", y = "Temperatura", fill = "Temp") +
  scale_fill_manual(values = c("tmax_by_month" = "#ee6055", "tavg_by_month" = "#60d394", "tmin_by_month" = "#255f85"), labels = c("Max Temp", "Avg Temp", "Min Temp")) +
  theme(legend.position = "bottom")

#grafico de temperaturas por dia
gr_temperatura_por_dia <- df_clima_aeroparque_BA_2022_reformed %>%
  pivot_longer(cols = 3:4, names_to = "tipo", values_to = "temp") %>%
  ggplot(mapping = aes(x = date, y = temp, color = tipo)) +
  geom_line() +
  scale_color_manual(labels = c("Tmax", "Tmin"), values = c("#ee6055", "#255f85")) +
  labs(x = "Dia del año", y = "Temperatura max y min", color = "Temperatura") +
  theme_classic()

#precipitaciones por mes
gr_precipitaciones_por_mes <- df_clima_aeroparque_BA_2022_reformed %>% 
  group_by(month) %>% 
  filter(!is.na(prcp)) %>% 
  summarise(precipitation_by_month = mean(prcp)) %>% 
  ggplot(mapping = aes(x = fct_relevel(month, v_months), y = precipitation_by_month)) +
  geom_col(fill = "#386fa4", colour = "#133c55") +
  labs(x = "Mes",y = "Precipitacion (mm)", title = "Precipitaciones por mes")


#uso de bicis por temperatura_promedio
gr_bikes_by_temp <- df_bikes_and_weather %>% 
  mutate(t_avg_round = round(tavg)) %>% 
  group_by(t_avg_round) %>% 
  summarise(viajes_por_tavg = n()) %>% 
  ggplot(mapping = aes(x = t_avg_round, y = viajes_por_tavg)) +
  geom_line(color = "#52b788", lwd = 1.5) +
  labs(x = "Temperatura Promedio", y = "Cantidad", title = "Cantidad de viajes segun temperatura")


#uso de bicis por precipitaciones
gr_uso_de_bicis_segun_precipitaciones <- df_uso_de_bicis_segun_precipitaciones_prom %>% 
  ggplot(mapping = aes(x = es_lluvia, y = viajes_promedio, fill = es_lluvia)) + 
  geom_col() +
  labs(x = "", y = "Cantidad", title = "Cantidad de viajes diarios durante dias de lluvia y no lluvia", fill = "LLovio?") +
  scale_fill_manual(values = c("llovio" = "#ee6055", "no llovio" = "#255f85"), labels = c("Si", "No")) 
  

#cantidad de dias de lluvia
gr_potencia_lluvia_2022 <- df_bikes_and_weather %>% 
  filter(rain_strength != "no llovio") %>% 
  ggplot(mapping = aes(x = fct_relevel(rain_strength, v_rain_strength), fill = rain_strength)) +
  geom_bar() + 
  labs(x = "", y = "cantidad de viajes", fill = "Tipo de lluvia")

gr_duracion_recorrido_segun_lluvia <- df_bikes_and_weather %>% 
  filter(!is.na(prcp)) %>% 
  filter(!is.na(duracion_recorrido)) %>% 
  group_by(rain_strength) %>% 
  ggplot(mappin = aes(x = fct_relevel(rain_strength, v_rain_strength), y = duracion_recorrido, color = rain_strength)) +
  geom_boxplot() +
  labs(x = "", y = "Duracion", title = "Duracion de los recorridos segun lluvia", color = "Tipo de lluvia")
