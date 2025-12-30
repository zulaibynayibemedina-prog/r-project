tuesdata <- tidytuesdayR::tt_load('2020-07-07')
coffee_ratings <- tuesdata$coffee_ratings

library(tidyverse)
library(fmsb)

set.seed(1234)
df_spyder <- coffee_ratings |> 
  drop_na(aroma:sweetness) |> 
  slice_sample(n = 3) |> 
  mutate(label = paste0(species, " - ", country_of_origin))


filas <- c("max", "min", df_spyder$label)
  
  
df_spyder <- df_spyder |> 
  dplyr::select(aroma:sweetness) 

df_spyder <- rbind(rep(10,9) , rep(0,9) , df_spyder) |> as.data.frame()


rownames(df_spyder) <- filas

colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.1), rgb(0.8,0.2,0.5,0.1) , rgb(0.7,0.5,0.1,0.1) )

radarchart(df_spyder, axistype = 1,
           pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,10,2), cglwd=0.8,
           #custom labels
           vlcex=0.8 )

legend(x=0.9, y=1, legend = rownames(df_spyder[-c(1,2),]), bty = "n", pch=20 , col=colors_border , text.col = "grey30", cex=1.1, pt.cex=3)
