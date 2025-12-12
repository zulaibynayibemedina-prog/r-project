library(rvest)

pag_col <- read_html("https://fbref.com/en/comps/41/history/Primera-A-Seasons")

camp_col <- pag_col |> 
  html_elements("table") |> 
  html_table() |> 
  as.data.frame()
