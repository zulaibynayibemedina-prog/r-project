library(rvest)
library(tidyverse)


pag_col <- read_html("https://fbref.com/en/comps/41/history/Primera-A-Seasons")

camp_col <- pag_col |> 
  html_elements("table") |> 
  html_table() |> 
  as.data.frame()

camp_proc <- camp_col |> 
  separate_wider_delim(Champion, delim = ",", names = c("ch1", "ch2"), too_few ="align_start") |> 
  pivot_longer(cols = c(ch1, ch2), names_to = "champ_type", values_to = "Champion") |> 
  mutate(Champion = str_remove_all(Champion, "\\([:alpha:]\\)"),
         Champion = str_squish(Champion))

table(camp_proc$Champion)

pag_chile <- read_html("https://fbref.com/en/comps/35/history/Chilean-Primera-Division-Seasons")

camp_chi <- pag_chile |> 
  html_elements("table") |> 
  html_table() |> 
  as.data.frame()

chi_proc <- camp_chi |> 
  mutate(Season = str_sub(Season, start = 1, end = 4)) |> 
  separate_wider_delim(Champion, delim = ",", names = c("ch1", "ch2"), too_few = "align_start") |> 
  pivot_longer(cols = c(ch1, ch2), names_to = "champ_type", values_to = "Champion") |> 
  drop_na() |> 
  mutate(Champion = str_remove_all(Champion, "\\([:alpha:]\\)"),
         Champion = str_remove_all(Champion, "-[:space:]\\d\\d"),
         Champion = str_squish(Champion))

table(chi_proc$Champion)

pag_arg <- read_html("https://fbref.com/en/comps/21/history/Liga-Profesional-Argentina-Seasons")

camp_arg <- pag_arg |> 
  html_elements("table") |> 
  html_table() |> 
  as.data.frame()

arg_proc <- camp_arg |> 
  mutate(Season = str_sub(Season, start = 1, end = 4),
         Champion = str_remove_all(Champion, "-[:space:]\\d\\d"),
         Champion = str_squish(Champion),
         Champion = if_else(str_detect(Champion, "\\w"), Champion, NA)) |> 
  drop_na()


# Wikipedia ---------------------------------------------------------------

url1 <- "https://es.wikipedia.org/wiki/Historial_de_la_Primera_Divisi%C3%B3n_de_Chile"

tab_ch <- as.data.frame(html_table(read_html(url1))[1])

tab_ch2 <- tab_ch |> 
  filter(str_starts(Año, pattern = "\\d")) |> 
  mutate(Año = str_remove(Año, "\\([:alnum:]\\)"),
         Año = str_remove(Año, "\\([:alnum:][:alnum:]\\)"),
         Año = as.numeric(str_squish(Año)),
         Campeón.1.. = str_remove(Campeón.1.., "\\([:alnum:]\\)"),
         Campeón.1.. = str_squish(str_remove(Campeón.1.., "\\([:alnum:][:alnum:]\\)")))


urls <- read_csv2("lista_urls.csv")


table(tab_ch2$Campeón.1..)


freq_s <- table(tab_ch2$Campeón.1..)
names(freq_s)
as.numeric(freq_s)

tab_años <- subset(tab_ch2, Año %in% 1990:1999)
freq_base <- sort(table(tab_años$Campeón.1..),decreasing = T)
fr_n <- as.numeric(freq_base)
fr_por <- round(fr_n / sum(fr_n) *100,2)
cat("Frequencies\n",
    "Club  - Freq -  % -\n")
for (i in 1:length(freq_base)) {
  if (i == 1) {
    cat(format("", width = 20), " ", 
        format("Freq", width = 5), " ", 
        format("%", width = 7), "\n")
    cat(paste(rep("-", 35), collapse = ""), "\n")
  }
      cat(format(names(freq_base)[i], width = 20, justify = "left"), 
          format(fr_n[i], width = 5, justify = "right"),
          format(fr_por[i], width = 7), "\n")
  
}


# Limpieza wiki -----------------------------------------------------------

##### Argentina #####


tab_arg <- html_table(read_html("https://www.afa.com.ar/es/pages/campeones-de-primera-division"))[[2]]

tab_arg2 <- tab_arg |> 
  mutate(Temporada = str_remove_all(Temporada, "\\([^)]+\\)"),
         Temporada = str_replace_all(Temporada, "\\/\\d\\d\\d\\d", ""),
         Temporada = str_replace_all(Temporada, "\\/\\d\\d", ""),
         Temporada = str_squish(Temporada),
         Temporada = as.numeric(str_sub(Temporada, start = -4)),
         Campeón = str_squish(Campeón),
         Campeón_2 = case_when(Campeón == "Estudiantes" ~ "Estudiantes de La Plata",
                             Campeón == "Newells Old Boys" ~ "Newell´s Old Boys",
                             # Campeón == "River  Plate" ~ "River Plate",
                             Campeón == "Vélez Sársfield" ~ "Vélez Sarsfield",
                             T ~ Campeón)) |> 
  select("Year" = Temporada, "Champion" = Campeón_2)

tab_bol <- html_table(read_html("https://es.wikipedia.org/wiki/Primera_Divisi%C3%B3n_de_Bolivia"))[[6]]

tab_bol2 <- tab_bol |> 
  filter(str_starts(Temp., "\\d")) |> 
  mutate(Torneo = str_remove_all(Torneo, "\\[\\d\\]|\\[\\d\\d\\]|\\[\\d[:alpha:]\\]"),
         Torneo = str_remove_all(Torneo, "\\-[:alpha:]"),
         Torneo = str_remove_all(Torneo, "[:alpha:]$"),
         Torneo = as.numeric(Torneo),
         Campeón = str_remove_all(Campeón, "\\(\\d\\d\\)|\\(\\d\\)"),
         Campeón = str_squish(Campeón)) |> 
  select("Year" = Torneo, "Champion" = Campeón)

tab_bra <- html_table(read_html("https://es.wikipedia.org/wiki/Campeonato_Brasile%C3%B1o_de_Serie_A"))[[5]]

tab_bra2 <- tab_bra |> 
  select(-8) |> 
  filter(str_starts(Temporada, "\\d")) |> 
  mutate(Temporada = as.numeric(Temporada),
         Campeón = str_remove_all(Campeón, "\\(\\d\\d\\)|\\(\\d\\)"),
         Campeón = str_squish(Campeón)) |> 
  select("Year" = Temporada, "Champion" = Campeón)

tab_chi <- html_table(read_html("https://es.wikipedia.org/wiki/Historial_de_la_Primera_Divisi%C3%B3n_de_Chile"))[[1]]

tab_chi2 <- tab_chi |> 
  filter(str_starts(Año, pattern = "\\d")) |> 
  mutate(Año = str_remove(Año, "\\([:alnum:]\\)"),
         Año = str_remove(Año, "\\([:alnum:][:alnum:]\\)"),
         Año = as.numeric(str_squish(Año)),
         `Campeón[1]​` = str_remove(`Campeón[1]​`, "\\([:alnum:]\\)"),
         `Campeón[1]​` = str_squish(str_remove(`Campeón[1]​`, "\\([:alnum:][:alnum:]\\)"))) |> 
  select("Year" = Año, "Champion" = `Campeón[1]​`)

tab_col <- html_table(read_html("https://es.wikipedia.org/wiki/Categor%C3%ADa_Primera_A"))[[3]]

tab_col2 <- tab_col |> 
  select(-10) |> 
  rename("Campeón" = 3) |> 
  filter(str_starts(Año, "\\d")) |> 
  mutate(Año = as.numeric(str_sub(Año, start = 1, end = 4)),
         Campeón = str_extract(Campeón, "^[^(]+")) |> 
  select("Year" = Año, "Champion" = Campeón)

tab_ecu <- html_table(read_html("https://es.wikipedia.org/wiki/Serie_A_(Ecuador)"))[[5]]

tab_ecu2 <- tab_ecu |> 
  mutate(Temporada = as.numeric(str_remove(Temporada, "-[:alpha:]")),
         Campeón = str_remove_all(Campeón, "\\(\\d\\d\\)|\\(\\d\\)"),
         Campeón = str_squish(Campeón),
         Campeón = if_else(Temporada %in% c(1958,1959), NA, Campeón)) |> 
  select("Year" = Temporada, "Champion" = Campeón)

tab_par <- html_table(read_html("https://es.wikipedia.org/wiki/Primera_Divisi%C3%B3n_de_Paraguay"))[[5]]

tab_par2 <- tab_par |> 
  select(-Edición) |> 
  filter(str_starts(Temp., "\\d")) |> 
  mutate(Año = as.numeric(Año),
         Campeón = str_extract(Campeón, "^[^(]+"),
         Campeón = str_squish(Campeón)) |> 
  select("Year" = Año, "Champion" = Campeón)

tab_per <- html_table(read_html("https://es.wikipedia.org/wiki/Anexo:Campeones_de_la_Primera_Divisi%C3%B3n_del_Per%C3%BA"))[[1]]

tab_per2 <- tab_per |> 
  filter(str_starts(Temporada, "\\d")) |> 
  mutate(Temporada = str_squish(Temporada),
         Temporada = str_sub(Temporada,1,4),
         Temporada = as.numeric(Temporada)) |> 
  select("Year" = Temporada, "Champion" = Campeón)

tab_uru <- html_table(read_html("https://es.wikipedia.org/wiki/Primera_Divisi%C3%B3n_de_Uruguay"))[[3]]

tab_uru2 <- tab_uru |> 
  select(-7) |> 
  filter(str_starts(N.º, "\\d")) |> 
  mutate(Temp. = str_sub(Temp., 1,4),
         Campeón = str_squish(str_extract(Campeón, "^[^(]+"))) |> 
  select("Year" = Temp., "Champion" = Campeón)

tab_ven <- html_table(read_html("https://es.wikipedia.org/wiki/Anexo:Historial_de_la_Primera_Divisi%C3%B3n_de_Venezuela"))

tab_ven2 <- full_join(tab_ven[[2]], tab_ven[[4]]) |> 
  full_join(tab_ven[[5]])

tab_ven3 <- tab_ven2 |> 
  mutate(Campeón = case_when(!is.na(`Campeón[1]​`) ~ `Campeón[1]​`,
                             !is.na(`Campeón[2]​`) ~ `Campeón[2]​`,
                             !is.na(`Campeón[3]​`) ~ `Campeón[3]​`)) |> 
  filter(str_starts(Año, "\\d")) |> 
  mutate(Año = as.numeric(str_sub(Año, 1,4)),
         Campeón = str_remove_all(Campeón, "\\(\\d\\d\\)|\\(\\d\\)"),
         Campeón = str_squish(Campeón)) |> 
  select("Year" = Año, "Champion" = Campeón)


df1 <- as.data.frame(table(tab_arg2$Champion))



arrange(df1, desc(Freq))
