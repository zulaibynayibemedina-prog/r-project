#comienzo aquí cargando el dataset:
#install.packages("tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load('2020-07-07')
coffee_ratings <- tuesdata$coffee_ratings

#Cosas que me parece interesante comparar:
#El aroma con otros atributos como flavor, aftertaste, acidity
#Hay una variable balance, podemos tambien 
#compararla con atributos. ¿Qué explica el balance?
#Altitud vs calidad (atributos) ¿Como influye la altitud?
#Defects vs Balance o vs lo dulce que es
#Region y cupper_points, que entiendo que es la calificacion general.

#UN BUBLE MAP: ponemos los paises y las bubles son las calificaciones:
#Voy a verificar si hay datos NA en alguna de las dos columnas a usar:
head(coffee_ratings)
is.numeric(coffee_ratings$cupper_points) #True
class(coffee_ratings$country_of_origin) #Character
sum(is.na(coffee_ratings$cupper_points)) #0
sum(is.na(coffee_ratings$country_of_origin)) #me da 1
which(is.na(coffee_ratings$country_of_origin))
coffee_ratings[1198,]
#HICE ESTO SOLITAAAAA, SIN AYUDAS, solo a memoria.
#Link del bubble map: https://r-graph-gallery.com/bubble-map
install.packages("leaflet.js")
install.packages("plotly") #Con este paquete se hace interactivo
install.packages("leaflet")

#Primero tengo que hacer un mapa base
# Load the library
library(leaflet)
#Hay dos opciones de mapa base: 
# Background 1: NASA, este es mas a cartografia
#m <- leaflet() %>% 
  #addTiles() %>% 
  #setView( lng = 2.34, lat = 48.85, zoom = 5 ) %>% 
  #addProviderTiles("NASAGIBS.ViirsEarthAtNight2012")
#m

# Background 2: World Imagery, este mas a un mapa mundi
p <- leaflet() %>% 
  addTiles() %>% 
  setView( lng = 2.34, lat = 48.85, zoom = 3 ) %>% 
  addProviderTiles("Esri.WorldImagery")
p

install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("sf")

library(dplyr)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

#Necesito los centroides de los paises para las bubbles
world <- ne_countries(scale = "medium", returnclass = "sf")

#Extraer centroides
world_centroids <- cbind(
  world, 
  st_coordinates(st_centroid(world$geometry))
)

#Mantener solo nombre y coords: 
world_centroids <- world_centroids %>%
  select(admin, X, Y) %>%
  rename(country_of_origin = admin,
         long = X,
         lat = Y)

#Unir el dataset con coordenadas por país
data_map <- coffee_ratings %>%
  left_join(world_centroids, by = "country_of_origin")

#Crear bubble map
leaflet(data_map) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  setView(lng = 0, lat = 20, zoom = 2) %>%     # vista global
  addCircleMarkers(
    lng = ~long,
    lat = ~lat,
    radius = ~ (total_cup_points - min(total_cup_points)) / 2,  # tamaño proporcional
    color = "violet",
    fillOpacity = 0.6,
    popup = ~ paste0(
      "Country", country_of_origin, "<br>",
      "Total cup points", total_cup_points
    )
  )

#Hay un problema y es que el mapa es continuo, no se como cortarlo.
#Cosas por verificar: country_of_origin debe coincidir con los nombres estándar de países de rnaturalearth
#Falta añadir leyenda para el mapa.
#Existe una libreria
install.packages("countrycode")
library(countrycode)
coffee_ratings$country_clean <- countrycode(
  sourcevar = coffee_ratings$country_of_origin,
  origin = "country.name",
  destination = "country.name",
  warn = TRUE
)
#Aqui ya no se porque me dice:
#Avisos:
#1: Some values were not matched unambiguously: United States (Puerto Rico)
#2: Some strings were matched more than once, and therefore set to <NA> in the result: United States (Puerto Rico),Puerto Rico,United States


#SEGUNDA GRAFICA:
