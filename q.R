#comienzo aquí cargando el dataset:
#install.packages("tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load('2020-07-07')
coffee_ratings <- tuesdata$coffee_ratings

#Cosas que me parece interesante comparar:
#El aroma con otros atributos como flavor, aftertaste, acidity
#Hay una variable balance, podemos tambien 
#compararla con atributos. ¿Qué explica el balance?
#Altitud vs calidad (atributos) ¿Como influye la altitud?


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


#SEGUNDA GRAFICA:radar chart, spider chart.
#Tiene que ser con datos numéricos, en nuestro caso, puede ser con:
#aroma, flavor, acidity, lo que no entiendo es qué va en el centro
#Recomiendan máximo 3 variables
#Para el chart necesito minimos y maximos en el df
class(coffee_ratings)

library(fmsb)
library(RColorBrewer)
library(scales)

coffee_ratings <- as.data.frame(coffee_ratings) # convertir tibble a data.frame

# Seleccionar columnas
coffee_ratings2 <- coffee_ratings[, c("aroma","flavor","acidity")]

# filas max y min
fila_max <- apply(coffee_ratings2, 2, max)
fila_min <- apply(coffee_ratings2, 2, min)

# Data frame final para el radar chart
coffee_ratings_spider <- rbind(fila_max, fila_min, coffee_ratings2)

# Asignar nombres de fila correctos
rownames(coffee_ratings_spider) <- c("Max","Min","Cafe1","Cafe2","Cafe3")

# Colores
coul <- brewer.pal(3, "BuPu")
colors_border <- coul
colors_in <- alpha(coul,0.3)

# Radar chart 
radarchart(coffee_ratings_spider, axistype=0, maxmin=TRUE,
           pcol=colors_border, pfcol=colors_in, plwd=4, plty=1,
           cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8,
           vlcex=0.8)

# Leyenda 
legend(x=0.7, y=1, legend=rownames(coffee_ratings_spider[-c(1,2),]),
       bty="n", pch=20, col=colors_in, text.col="grey",
       cex=1.2, pt.cex=3)


#Tercera grafica: variety con la altitud? 
#Yo diria que una ridgeline plot
#En nuestro caso en el eje x va altitude_mean_meters
#En el eje y va variety que es categorica
#Debemos primero limpiar los datos para dejarlos sin NAs


install.packages("ggridges")
install.packages("ggplot2")
install.packages("MetBrewer")
install.packages("glue")
install.packages("dplyr")
library(ggridges)
library(dplyr)
library(ggplot2)
library(MetBrewer)
library(glue)


#Flitro las filas donde no hay NAs y creo otro df para hacer la grafica
coffee_plot3<-coffee_ratings[!is.na(coffee_ratings$variety) & !is.na(coffee_ratings$altitude_mean_meters),]
head(coffee_plot3)
#Miro primero maximos y minimos:
min(coffee_plot3$altitude_mean_meters)
max(coffee_plot3$altitude_mean_meters) #190164, entonces hago que el eje x vaya hasta 200000
#Calculo la altitud media segun cada variedad de cafe:
mean_altitudes <- tapply(
  coffee_plot3$altitude_mean_meters,
  coffee_plot3$variety,
  mean
)
#Ahora si organizo las variedades de cafe según la media de arriba:
ordenadas<-names(sort(mean_altitudes))
#Convierto en factor:
coffee_plot3$variety <- factor(coffee_plot3$variety, levels = ordenadas)

windows(width = 10, height = 7)
ggplot(coffee_plot3, aes(
  x = altitude_mean_meters,
  y = variety,
  fill = variety
)) +
  geom_density_ridges(alpha = 0.8) +
  scale_x_continuous(limits = c(0, 190180)) + #Pongo esto para el maximo en x 
  theme_minimal() +
  guides(fill = "none") +
  labs(
    title = "Altitud de cultivo por variedad de café",
    x = "Altitud media (m)",
    y = "Variedad"
  )
#Esta grafica se ve fatal pero no sé comoa arreglarla porque seguro algo esta mal.


##Boceto de la funcion del PUNTO 1: 
champions <- function(country, years) {
#Primero creo una lista con los paises y la urls   
  urls <- list(
    "Colombia"  = "URLColombia",
  )
#Verificar que el pais este en la lista: 
  if (!(country %in% names(urls))) {
    stop("El país elegido no está disponible.")
  }
  # Empezamos web scrapping con rvest.
  # Sacamos la tabla principal donde aparecen los campeones.
  # html_table() convierte la tabla HTML en un data frame.
  tabla <- rvest::html_table(
    rvest::read_html(urls[[country]])
  )[[1]]
  #teniendo en cuenta que las tablas pueden tener nombres de columna distintos
  #renombramos las columnas de Año y Campeón.
  names(tabla)[1:2] <- c("Year", "Champion")
  
  # Depende de los años que diga el usuario, se filtra:
  # Solo nos quedamos con las filas con el año que  esté en 'years'.
  tabla_filtrada <- subset(tabla, Year %in% years)
  
  #Tabla de frecuencia:
  # Frecuencia simple
  # y calculamos el porcentaje
  freq <- as.data.frame(table(tabla_filtrada$Champion))
  freq$Percent <- round(freq$Freq / sum(freq$Freq) * 100, 2)
  #Deberíamos obtener una tabla de frecuencias:
  return(freq)
}
