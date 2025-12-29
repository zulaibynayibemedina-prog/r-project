
# Champions function ------------------------------------------------------




##Boceto de la funcion del PUNTO 1: 
champions <- function(country, years) {
  #Primero creo una lista con los paises y la urls   
  urls <- list(
    'Argentina' = 'https://www.afa.com.ar/es/pages/campeones-de-primera-division',
    'Bolivia' = 'https://es.wikipedia.org/wiki/Primera_Divisi%C3%B3n_de_Bolivia',
    'Brasil' = 'https://es.wikipedia.org/wiki/Campeonato_Brasile%C3%B1o_de_Serie_A',
    'Chile' = 'https://es.wikipedia.org/wiki/Historial_de_la_Primera_Divisi%C3%B3n_de_Chile',
    'Colombia' = 'https://es.wikipedia.org/wiki/Categor%C3%ADa_Primera_A',
    'Ecuador' = 'https://es.wikipedia.org/wiki/Serie_A_(Ecuador)',
    'Paraguay' = 'https://es.wikipedia.org/wiki/Primera_Divisi%C3%B3n_de_Paraguay',
    'Perú' = 'https://es.wikipedia.org/wiki/Anexo:Campeones_de_la_Primera_Divisi%C3%B3n_del_Per%C3%BA',
    'Uruguay' = 'https://es.wikipedia.org/wiki/Primera_Divisi%C3%B3n_de_Uruguay',
    'Venezuela' = 'https://es.wikipedia.org/wiki/Anexo:Historial_de_la_Primera_Divisi%C3%B3n_de_Venezuela'
  )
  #Verificar que el pais este en la lista: 
  if (!(country %in% names(urls))) {
    stop("El país elegido no está disponible.")
  }
  # Empezamos web scrapping con rvest.
  # Sacamos la tabla principal donde aparecen los campeones.
  # html_table() convierte la tabla HTML en un data frame.
  raw1 <- rvest::html_table(
    rvest::read_html(urls[[country]]))
  #teniendo en cuenta que las tablas pueden tener nombres de columna distintos
  #renombramos las columnas de Año y Campeón.
  
  if (country == 'Argentina') {
    tab_cham <- raw1[[2]] |> 
      mutate(Temporada = str_remove_all(Temporada, "\\([^)]+\\)"),
             Temporada = str_replace_all(Temporada, "\\/\\d\\d\\d\\d", ""),
             Temporada = str_replace_all(Temporada, "\\/\\d\\d", ""),
             Temporada = str_squish(Temporada),
             Temporada = as.numeric(str_sub(Temporada, start = -4)),
             Campeón = str_squish(Campeón),
             Campeón_2 = case_when(Campeón == "Estudiantes" ~ "Estudiantes de La Plata",
                                   Campeón == "Newells Old Boys" ~ "Newell´s Old Boys",
                                   Campeón == "Vélez Sársfield" ~ "Vélez Sarsfield",
                                   T ~ Campeón)) |> 
      select("Year" = Temporada, "Champion" = Campeón_2)
  }
  if (country == 'Bolivia') {
    tab_cham <- raw1[[6]] |> 
      filter(str_starts(Temp., "\\d")) |> 
      mutate(Torneo = str_remove_all(Torneo, "\\[\\d\\]|\\[\\d\\d\\]|\\[\\d[:alpha:]\\]"),
             Torneo = str_remove_all(Torneo, "\\-[:alpha:]"),
             Torneo = str_remove_all(Torneo, "[:alpha:]$"),
             Torneo = as.numeric(Torneo),
             Campeón = str_remove_all(Campeón, "\\(\\d\\d\\)|\\(\\d\\)"),
             Campeón = str_squish(Campeón)) |> 
      select("Year" = Torneo, "Champion" = Campeón)
  }
  
  if (country == "Brasil") {
    tab_cham <- raw1[[5]] |> 
      select(-8) |> 
      filter(str_starts(Temporada, "\\d")) |> 
      mutate(Temporada = as.numeric(Temporada),
             Campeón = str_remove_all(Campeón, "\\(\\d\\d\\)|\\(\\d\\)"),
             Campeón = str_squish(Campeón)) |> 
      select("Year" = Temporada, "Champion" = Campeón)
  }
  
  if (country == "Chile") {
    tab_cham <- raw1[[1]] |> 
      filter(str_starts(Año, pattern = "\\d")) |> 
      mutate(Año = str_remove(Año, "\\([:alnum:]\\)"),
             Año = str_remove(Año, "\\([:alnum:][:alnum:]\\)"),
             Año = as.numeric(str_squish(Año)),
             `Campeón[1]​` = str_remove(`Campeón[1]​`, "\\([:alnum:]\\)"),
             `Campeón[1]​` = str_squish(str_remove(`Campeón[1]​`, "\\([:alnum:][:alnum:]\\)"))) |> 
      select("Year" = Año, "Champion" = `Campeón[1]​`)
  }
  
  if (country == "Colombia"){
    
    tab_cham <- raw1[[3]] |> 
      select(-10) |> 
      rename("Campeón" = 3) |> 
      filter(str_starts(Año, "\\d")) |> 
      mutate(Año = as.numeric(str_sub(Año, start = 1, end = 4)),
             Campeón = str_extract(Campeón, "^[^(]+")) |> 
      select("Year" = Año, "Champion" = Campeón)
  }
  
  if (country == "Ecuador"){
    tab_cham <- raw1[[5]] |> 
      mutate(Temporada = as.numeric(str_remove(Temporada, "-[:alpha:]")),
             Campeón = str_remove_all(Campeón, "\\(\\d\\d\\)|\\(\\d\\)"),
             Campeón = str_squish(Campeón),
             Campeón = if_else(Temporada %in% c(1958,1959), NA, Campeón)) |> 
      select("Year" = Temporada, "Champion" = Campeón)
  }
  
  if (country == "Paraguay"){
    tab_cham <- raw1[[5]] |> 
      select(-Edición) |> 
      filter(str_starts(Temp., "\\d")) |> 
      mutate(Año = as.numeric(Año),
             Campeón = str_extract(Campeón, "^[^(]+"),
             Campeón = str_squish(Campeón)) |> 
      select("Year" = Año, "Champion" = Campeón)
  }
  
  if (country == "Perú"){
    tab_cham <- raw1[[1]] |> 
      filter(str_starts(Temporada, "\\d")) |> 
      mutate(Temporada = str_squish(Temporada),
             Temporada = str_sub(Temporada,1,4),
             Temporada = as.numeric(Temporada)) |> 
      select("Year" = Temporada, "Champion" = Campeón)
  }
  
  if (country == "Uruguay"){
    tab_cham <- raw1[[3]] |> 
      select(-7) |> 
      filter(str_starts(N.º, "\\d")) |> 
      mutate(Temp. = str_sub(Temp., 1,4),
             Campeón = str_squish(str_extract(Campeón, "^[^(]+"))) |> 
      select("Year" = Temp., "Champion" = Campeón)
  }
  
  if (country == "Venezuela"){
    tab_ven <- suppressMessages(full_join(raw1[[2]], raw1[[4]]) |> 
      full_join(raw1[[5]]))
    
    tab_cham <- tab_ven |> 
      mutate(Campeón = case_when(!is.na(`Campeón[1]​`) ~ `Campeón[1]​`,
                                 !is.na(`Campeón[2]​`) ~ `Campeón[2]​`,
                                 !is.na(`Campeón[3]​`) ~ `Campeón[3]​`)) |> 
      filter(str_starts(Año, "\\d")) |> 
      mutate(Año = as.numeric(str_sub(Año, 1,4)),
             Campeón = str_remove_all(Campeón, "\\(\\d\\d\\)|\\(\\d\\)"),
             Campeón = str_squish(Campeón)) |> 
      select("Year" = Año, "Champion" = Campeón)
  }
  
  # print(tab_cham)
  
  # Depende de los años que diga el usuario, se filtra:
  # Solo nos quedamos con las filas con el año que  esté en 'years'.
  tabla_filtrada <- subset(tab_cham, Year %in% years)
  
  #Tabla de frecuencia:
  # Frecuencia simple
  # y calculamos el porcentaje
  freq <- as.data.frame(table(tabla_filtrada$Champion))
  freq <- dplyr::arrange(freq, dplyr::desc(Freq))
  freq$Percent <- round(freq$Freq / sum(freq$Freq) * 100, 2)
  #Deberíamos obtener una tabla de frecuencias:
  # return(freq)
  # print(freq)
  
  
  teams <- format(freq$Var1, width = 20, justify = "left")
  freqs <- format(freq$Freq, width = 5, justify = "right")
  percs <- format(sprintf("%.2f", freq$Percent), width = 7, justify = "right")
  
  cat("Champions (male football) in", country,  "in the", sum(freq$Freq), "selected years\n")
  
  cat(format("", width = 20), " ", 
      format("Freq", width = 5, justify = "right"), " ", 
      format("%", width = 7, justify = "right"), "\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")
  
  for (i in 1:nrow(freq)) {
    cat(teams[i], " ", freqs[i], " ", percs[i], "\n")
  }
}



champions("Colombia", 1996:2025)
