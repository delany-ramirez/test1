library(tidyverse)

partidos_fifa_copa_mundial_procesado <- readr::read_delim("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-10/partidos.txt",delim = "\t")

partidos <- partidos_fifa_copa_mundial_procesado

head(partidos)

partidos <- partidos %>% 
  mutate(partido_orden = as.numeric(gsub("[()]", "", partido_orden)))

partidos %>% 
  group_by(anio) %>% 
  summarize(num_partidos = n(), goles = sum(equipo_1_final,equipo_2_final), rate = goles/num_partidos) %>% 
  ggplot(aes(anio, rate)) +
  geom_col() + geom_smooth()
  
partidos %>% 
  filter(equipo_1_final != equipo_2_final) %>% 
  mutate(ganador = if_else(equipo_1_final > equipo_2_final, equipo_1, equipo_2)) %>% 
  count(ganador, sort = TRUE) %>% 
  ggplot(aes(fct_reorder(ganador, n), n, fill = ganador, label = n)) + 
  geom_col() +
  geom_text(hjust = -0.2) +
  coord_flip() +
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title = "Cantidad de partidos ganados en Mundiales por País",
       subtitle = "DatosDeMiércoles",
       caption = "Fuente: Open Public Domain Football Data",
       x = "País",
       y = "Cantidad de partidos ganados") 




partidos %>% 
  filter(equipo_1_final != equipo_2_final) %>% 
  mutate(ganador = if_else(equipo_1_final > equipo_2_final, equipo_1, equipo_2)) %>% 
  count(anio, ganador, sort = TRUE) %>% 
  group_by(anio) %>% 
  mutate(posicion = rank(-n, ties.method = "first")) %>% 
  ungroup() %>% 
  filter(posicion <= 1) %>% 
  ggplot(aes(x = as_factor(anio), y = n, col = ganador, label = ganador)) +
  geom_point() +
  geom_text(angle = 45, vjust = 1) +
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title = "País con mayor cantidad de partidos ganados por Copa Mundial",
       subtitle = "DatosDeMiércoles",
       caption = "Fuente: Open Public Domain Football Data",
       x = "Copa Mundial",
       y = "Cantidad de partidos ganados") 
