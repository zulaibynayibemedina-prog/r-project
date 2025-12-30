#comienzo aquí cargando el dataset:
#install.packages("tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load('2020-07-07')
coffee_ratings <- tuesdata$coffee_ratings

#Este código a continuación es la versión mejorada POR COPILOT del código
#Que yo hice antes que está debajo (despues de este)
#Cositas por ver: Yo no veo que los centroides cambien de tamaño taantp
#Intente poner con labels en cada centroide pero se veia horroroso
#Y ya pude recortar el mapa para que no sea continuo jej.
library(dplyr)
library(leaflet) #This library is used to create the base map
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(scales)


#Como el mapa usa nombre estandard e intenté usar una funcion,
#Habian errores entonces segun copilot hay que hacerlo manual.

#This first and previous step, consists on standarizing the country names
#because they must match the names from rnaturalearth
coffee_ratings$country_clean <- coffee_ratings$country_of_origin

coffee_ratings$country_clean[coffee_ratings$country_clean == "United States (Puerto Rico)"] <- "Puerto Rico"
coffee_ratings$country_clean[coffee_ratings$country_clean == "Tanzania, United Republic Of"] <- "Tanzania"
coffee_ratings$country_clean[coffee_ratings$country_clean == "Cote d?Ivoire"] <- "Ivory Coast"
coffee_ratings$country_clean[coffee_ratings$country_clean == "Hawai'i"] <- "United States of America"

#Centroids of the countries to place the bubbles:

world <- ne_countries(scale = "medium", returnclass = "sf")
#Extract centroids
centroids <- st_centroid(world$geometry)
coords <- st_coordinates(centroids)
#Maintain only name and coords
world_centroids <- data.frame(
  country_clean = world$admin,
  long = coords[, "X"],
  lat = coords[, "Y"]
)

#Join the data set with coordinates for each country:
data_map <- merge(
  coffee_ratings,
  world_centroids,
  by = "country_clean",
  all.x = TRUE
)

#Create the bubble map (interactive)

mapa <- leaflet(options = leafletOptions(worldCopyJump = FALSE))

mapa <- addProviderTiles(
  mapa,
  "Esri.WorldImagery",
  options = providerTileOptions(noWrap = TRUE)
)

mapa <- setView(mapa, lng = 0, lat = 20, zoom = 2) #Global view

mapa <- addCircleMarkers(
  mapa,
  lng = data_map$long,
  lat = data_map$lat,
  radius = scales::rescale(data_map$total_cup_points, to = c(5, 40)),
  color = "violet",
  fillOpacity = 0.6,
  #When the user clicks on the bubble, the following information is shown:
  popup = paste0(
    "Country: ", data_map$country_of_origin, "<br>",
    "Total cup points: ", data_map$total_cup_points
  )
)


mapa <- addLegend(
  mapa,
  position = "bottomright",
  colors = "violet",
  labels = "Bubble size = Coffee cup score",
  opacity = 0.6,
  title = "Interpretation"
)

mapa


#UN BUBLE MAP: ponemos los paises y las bubles son las calificaciones:
#Voy a verificar si hay datos NA en alguna de las dos columnas a usar:
head(coffee_ratings)
is.numeric(coffee_ratings$cupper_points) #True
class(coffee_ratings$country_of_origin) #Character
sum(is.na(coffee_ratings$cupper_points)) #0
sum(is.na(coffee_ratings$country_of_origin)) #me da 1
which(is.na(coffee_ratings$country_of_origin))
coffee_ratings[1198,]

#Link del bubble map: https://r-graph-gallery.com/bubble-map
#install.packages("leaflet.js")
#install.packages("plotly") #Con este paquete se hace interactivo
#install.packages("leaflet")

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

# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")
# install.packages("sf")

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
#install.packages("countrycode")
library(countrycode)
coffee_ratings$country_clean <- countrycode(
  sourcevar = coffee_ratings$country_of_origin,
  origin = "country.name",
  destination = "country.name",
  warn = TRUE
)
###############################################
library(fmsb)
library(RColorBrewer)
library(scales)

# Asegurar formato data.frame
coffee_ratings <- as.data.frame(coffee_ratings)

# Seleccionar columnas numéricas
coffee_ratings2 <- coffee_ratings[, c("aroma", "flavor", "acidity")]

# Seleccionar cafés extremos para cada atributo
cafe_aroma_high  <- coffee_ratings2[which.max(coffee_ratings2$aroma), ]
cafe_aroma_low   <- coffee_ratings2[which.min(coffee_ratings2$aroma), ]

cafe_flavor_high <- coffee_ratings2[which.max(coffee_ratings2$flavor), ]
cafe_flavor_low  <- coffee_ratings2[which.min(coffee_ratings2$flavor), ]

cafe_acid_high   <- coffee_ratings2[which.max(coffee_ratings2$acidity), ]
cafe_acid_low    <- coffee_ratings2[which.min(coffee_ratings2$acidity), ]

# Unir los 6 cafés
muestras <- rbind(
  cafe_aroma_high,
  cafe_aroma_low,
  cafe_flavor_high,
  cafe_flavor_low,
  cafe_acid_high,
  cafe_acid_low
)

rownames(muestras) <- c(
  "HighAroma", "LowAroma",
  "HighFlavor", "LowFlavor",
  "HighAcidity", "LowAcidity"
)

# Calcular fila max y min para la escala
fila_max <- apply(coffee_ratings2, 2, max)
fila_min <- apply(coffee_ratings2, 2, min)

# Data frame final para el radar chart
coffee_ratings_spider <- rbind(fila_max, fila_min, muestras)

# Colores (6 cafés)
coul <- brewer.pal(6, "Set2")
colors_border <- coul
colors_in <- alpha(coul, 0.35)

# Radar chart
radarchart(
  coffee_ratings_spider,
  axistype = 1,
  maxmin = TRUE,
  pcol = colors_border,
  pfcol = colors_in,
  plwd = 3,
  plty = 1,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  cglwd = 0.8,
  vlcex = 1
)

# Leyenda
legend(
  x = "topright",
  legend = rownames(coffee_ratings_spider[-c(1,2), ]),
  bty = "n",
  pch = 20,
  col = colors_in,
  text.col = "black",
  cex = 1,
  pt.cex = 2
)
#SEGUNDA GRAFICA:radar chart, spider chart.
#Tiene que ser con datos numéricos, en nuestro caso, puede ser con:
#aroma, flavor, acidity.
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
###################################################
#Opción 2 para la 2 grafica:
library(MASS)

# Seleccionar atributos sensoriales
coffee_ratings2 <- coffee_ratings[, c("aroma", "flavor", "acidity")]

# Elegir 20 cafés aleatorios para que sea legible
set.seed(123)
muestras <- coffee_ratings2[sample(1:nrow(coffee_ratings2), 20), ]

# Normalizar entre 0 y 1 (muy importante)
muestras_norm <- as.data.frame(
  apply(muestras, 2, function(x) (x - min(x)) / (max(x) - min(x)))
)

# Crear colores bonitos
colores <- rainbow(nrow(muestras_norm), alpha = 0.6)

# Parallel coordinates plot con MASS
parcoord(
  muestras_norm,
  col = colores,
  lwd = 2,
  var.label = TRUE,
  main = "Parallel Coordinates Plot (MASS::parcoord)"
)

# Añadir leyenda
legend(
  "topright",
  legend = paste0("Cafe", 1:nrow(muestras_norm)),
  col = colores,
  lwd = 2,
  bty = "n"
)

#Tercera grafica: variety con la altitud? 
#Yo diria que una ridgeline plot
#En nuestro caso en el eje x va altitude_mean_meters
#En el eje y va variety que es categorica
#Debemos primero limpiar los datos para dejarlos sin NAs


# install.packages("ggridges")
# install.packages("ggplot2")
# install.packages("MetBrewer")
# install.packages("glue")
# install.packages("dplyr")
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

############Te pongo aqui el nuevo codigo que si funciona##############
####Lo que tu me dices: 
summary(coffee_plot3$altitude_mean_meters)
# install.packages("ggridges")
# install.packages("ggplot2")
# install.packages("MetBrewer")
# install.packages("glue")
# install.packages("dplyr")
library(ggridges)
library(dplyr)
library(ggplot2)
library(MetBrewer)
library(glue)

# Flitro las filas donde no hay NAs y creo otro df para hacer la grafica
coffee_plot3 <- coffee_ratings[!is.na(coffee_ratings$variety) & 
                                 !is.na(coffee_ratings$altitude_mean_meters), ]

head(coffee_plot3)

# Miro primero maximos y minimos:
summary(coffee_plot3$altitude_mean_meters)

# AQUI AGREGO LO NECESARIO: poner NA a valores irreales (>3000 m)
coffee_plot3$altitude_mean_meters[
  coffee_plot3$altitude_mean_meters > 3000
] <- NA

# Filtrar nuevamente para quitar esos NA
coffee_plot3 <- coffee_plot3[!is.na(coffee_plot3$altitude_mean_meters), ]

# Calculo la altitud media segun cada variedad de cafe:
mean_altitudes <- tapply(
  coffee_plot3$altitude_mean_meters,
  coffee_plot3$variety,
  mean
)

# Ahora si organizo las variedades de cafe según la media de arriba:
ordenadas <- names(sort(mean_altitudes))

# Convierto en factor:
coffee_plot3$variety <- factor(coffee_plot3$variety, levels = ordenadas)

windows(width = 10, height = 7)
ggplot(coffee_plot3, aes(
  x = altitude_mean_meters,
  y = variety,
  fill = variety
)) +
  geom_density_ridges(alpha = 0.8) +
  scale_x_continuous(limits = c(0, 3000)) + 
  scale_fill_manual(values=met.brewer("Java",length(unique(coffee_plot3$variety))))
  #Si no quieres java, hay "Hiroshige", "Cassatt2"
  theme_minimal() +
  guides(fill = "none") +
  labs(
    title = "Altitud de cultivo por variedad de café",
    x = "Altitud media (m)",
    y = "Variedad"
  )

