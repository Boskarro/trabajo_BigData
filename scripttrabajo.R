#CARGADO DE PAQUETES
library(tidyverse)
library(gganimate)
library(patchwork)

#CARGADO DE DATOS

datos <- "./datos/sharkie.csv"
datos2 <- "./datos/poblacion_mundo.csv"

sharks <-rio::import(datos)
humans <- rio::import(datos2)


colnames(humans) <- c("Year", "Asia", "Africa", "Americas", "Europe", "Oceania")
humans <- humans[-1, drop(TRUE)]
humans$Asia <- as.numeric(humans$Asia)
humans_bueno <- humans %>%
  mutate(Total = Africa + Americas + Europe + Oceania)

#me ventilo los NA

sharks_limpio <- sharks%>% drop_na()


#Voy a seleccionar las variables que me interesan para realizar el trabajo, generando el DF general que usaré:

DF_TIBURONE <- sharks_limpio %>%
  select(Year, Type, Country, Activity, Sex, Age,`Fatal (Y/N)`, Species)

#REALIZACION DE GRAFICOS

#GRAFICO CON NUMERO DE ATAQUES TOTAL POR AÑO

Ataques_por_año <- DF_TIBURONE %>%
  group_by(Year) %>%
  summarize(Num_Attacks = n()) %>%
  mutate(total = sum(Num_Attacks)) %>%
  mutate(Total_acumulado = cumsum(Num_Attacks))

G1 <- ggplot(Ataques_por_año,
       aes(
         Year, 
         Num_Attacks)) +
       geom_line(size = 0.90,
                 color = "#BD1D00") +
  labs(
    title = "Número de ataques de tiburón por año.",
    subtitle = "Serie 1555-2022",
    x = "Año",
    y = "Número de ataques"
  ) +
  ggthemes::theme_stata() + 
  transition_reveal(Year) +
  view_follow(fixed_y = TRUE)

animate(G1, nframes = 244, fps = 20)
  
G1

G2 <- ggplot(Ataques_por_año, 
             aes(
               Year,
               Total_acumulado)) +
  geom_line(size = 0.9,
            color = "#BD1D00" ) +
  labs(
    title = "Total de ataques de tiburón",
    subtitle = "Serie acumulada 1555-2022",
    x = "Año",
    y = "Número de ataques"
  ) + 
  ggthemes::theme_stata() +
  transition_reveal(Year) +
  view_follow(fixed_y = TRUE)

animate(G2, nframes = 244, fps = 20)

#GRAFICO DE ATAQUES POR PAÍS


#MAPAS

library("stringr")
library("rnaturalearth")
library("rnaturalearthdata")

DF_MAPA <- DF_TIBURONE %>%
  select(Year, Country) %>%
  filter(Year == 2022) %>%
  group_by(Country) %>%
  summarise(Num_Attacks = n()) %>%
  mutate(Country = case_when(
           Country == "USA" ~ "United States",
           TRUE ~ Country  # Mantener los demás países sin cambios
         )) %>%
  mutate(Country = str_to_title(Country))
  
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")


world <- world %>% filter(subregion != "Antarctica") %>% filter(admin != "Greenland")

ggplot() + geom_sf(data = world) + theme_void()

world <- world %>% select(name, iso_a3, geometry) 


df_world <- full_join(DF_MAPA, world, by = c("Country" = "name") ) 

p10 <- ggplot(df_world) +
  geom_sf(aes(fill = Num_Attacks, geometry = geometry), color = "white", size = 0.5) +
  scale_fill_viridis_c(option = "D", trans = "log", labels = scales::label_number(scale = 1)) +  # Sin decimales en la leyenda
  theme_void() +
  labs(title = "Número de Ataques de Tiburón por País en 2022",
       fill = "Número de ataques") +
  ggthemes::theme_stata()

p10


#GRAFICOS DE ESPECIES CON MAS ATAQUES REGISTRADOS

Especies <- c("White shark", "Grey reef shark", "Tiger shark", "Wobbegong shark", "Blacktip shark", "Sandtiger shark", "Reef shark", "Bull shark", "Blacktip reef shark", "Whitetip reef shark", "Lemon shark", "Porbeagle shark", "Nurse shark", "Bronze whaler shark", "Raggedtooth shark", "Mako shark", "Hammerhead shark", "Whaler shark", "Copper shark", "Galapagos shark", "Cookiecutter shark", "Broadnose sevengil shark", "Spinner shark", "Blue shark", "Carpet shark", "Basking shark", "Salmon shark", "Zambesi shark","shark")

Ataques_por_especie <- DF_TIBURONE %>%
  group_by(Species) %>%
  summarize(Num_Attacks = n()) %>%
  mutate(Total = sum(Num_Attacks),
         Especie = gsub("^\\+?[\\.+0-9]+'?[ ]?|[\\.+0-9]+[ ]?(m|cm)[ ]?to [\\.+0-9]+[ ]?(m|cm)[ ]?\\[([\\.+0-9]+)'? to ([\\.+0-9]+)'?\\][ ]?|[ ]?metre|[\\[\\]][ ]?|\\d+[ ]?[-–][ ]?\\[?\\d+[ ]?(kg|lb)\\]?[ ]?dead|,.*|[ ]?metre|[ ]?(m|cm)|\\[.*\\]|[;].*", "", Species))


Ataques_interes <- Ataques_por_especie %>%
  select(Especie, Num_Attacks, Total) %>%
  filter(Especie %in% Especies) %>%
  group_by(Especie) %>%
  summarise(Num_Attacks = sum(Num_Attacks)) %>%
  arrange(desc(Num_Attacks)) %>%
  mutate(Total = cumsum(Num_Attacks)) %>%
  mutate(Porcentaje = sprintf( "%.1f", Num_Attacks / sum(Num_Attacks) * 100)) %>%
  mutate(Total_Porcentaje = 1510/1510 *100)


barras1 <- ggplot(Ataques_interes, aes(x = reorder(Especie, -Num_Attacks), y = Num_Attacks, fill = Especie)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Cantidad de ataques de tiburón por especie",
       x = "Especies de tiburón",
       y = "Cantidad de ataques") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

barras1



#GRAFICOS PASTEL DE ACTIVIDADES MAS PROPENSAS A SUFRIR ATAQUES

Actividades <- c("Fishing", "Swimming", "Diving", "Fell overboard", "Kite surfing", "Paddling","Playing", "Rowing", "Scuba diving", "Sailing", "Sea Disaster", "Shark fishing", "Snorkeling", "Spearfishing", "Splashing", "Standing", "Suicide", "Surfing")

df_actividades <- DF_TIBURONE %>%
  group_by(Activity) %>%
  summarise("Num_Attacks" = n()) %>%
  filter (Activity%in%Actividades) %>%
  arrange(desc(Num_Attacks))
  
barras2 <- ggplot(df_actividades, aes(x = reorder(Activity, -Num_Attacks), y = Num_Attacks, fill = Activity)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Cantidad de ataques de tiburón por actividad",
       x = "Actividades",
       y = "Cantidad de ataques") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

barras2



#GRAFICOS QUE COMPAREN ATAQUES A HOMBRES Y MUJERES
