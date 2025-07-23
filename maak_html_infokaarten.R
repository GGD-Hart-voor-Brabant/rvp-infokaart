# maak html infokaarten

library(dplyr)
setwd(rstudioapi::getActiveDocumentContext()$path %>% dirname() )

ggd = "GGD West-Brabant"

# statline data ophalen ---------------------------------------------------
gemeenten_in_regio <- cbs_get_data(
  '85385NED',
  select = c("Code_1",
             "Naam_2",
             "Code_16",
             "Naam_17")) %>% 
  mutate(
    Naam_2 = str_trim(Naam_2, side = "both"),
    Naam_17 = str_trim(Naam_17, side = "both")
    ) %>%
  filter(Naam_17 %in% c(ggd)) %>% 
  pull(Naam_2)

print(gemeenten_in_regio)

#testen met 1e gemeente

# HTML uitdraai per gemeente -----------------------------------------------------------
for(gemeentenaam in gemeenten_in_regio){
  
  print(paste("HTML maken voor", gemeentenaam,"..."))
   
  quarto::quarto_render(
    input = "infokaart.qmd",
    output_format = "html",
    output_file = glue::glue("infokaart_{gemeentenaam}.html"),
    execute_params = list(

      gemeentenaam = gemeentenaam,
      excel_bestand = "data/Vaccgraad_GGD West-Brabant_2021tm2025_incllft.xlsx",
      ggd = ggd,
      ggd_afkorting = "WB"
      )
  )
  
  #Bestand kopieren naar submap. Staat opgeruimder
  file.copy(from = glue::glue("infokaart_{gemeentenaam}.html"),
            to = glue::glue("infokaarten/infokaart_{gemeentenaam}.html"),
            overwrite = TRUE)
  
  file.remove(glue::glue("infokaart_{gemeentenaam}.html"))

}
