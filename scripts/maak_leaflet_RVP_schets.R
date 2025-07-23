#Knip en plakwerk van https://github.com/ggdatascience/rvp_vaccinatiegraad_rivm
#tbv kaartje voor RVP schets;  zuigelingen basisimmuum; bmr Pneu


#Lees Vaccinatiegraad RIVM
#NB We kregen de data aangeleverd met in sheet 2023 geen in CELL A1 
#(itt de sheets van andere jaren waar TOTAAL NEDERLAND  staat) Deze inconsistentie 
#zorgt ervoor dat de rij die initieel voor kolomkoppen wordt gebruikt eentje opschuift
#t.a.v de andere sheets. Dat maakt het onmogelijk om alles tegelijk in te lezen. 
#Opgelost door zelf iets in A1 van sheet 2023 te zetten.
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#workbook laden


maak_leaflet_rvp_schets <- function(
    bestand,
    cohorten = "Zuigelingen",
    vaccinatietoestand = c("Basisimmuun","Volledig_afgesloten"),
    vaccinsoort = c("BMR","Pneu"),
    vacctoestand_ipv_leeftijd = TRUE,
    percentages_in_kaart = FALSE,
    jaar_in_kaartlaag = FALSE,
    aantal_jaren_meenemen = NULL,
    pc4_shp = "../shapefiles/PC4.shp"
    
){
  

  
  wb <- openxlsx::loadWorkbook(bestand)
  
  toegevoegde_postcodes <- c("5048")
  
  #checken welke sheets er zijn
  sheets <- wb %>% openxlsx::sheets()
  
  #Aanname: Alle met een 4 cijfers is een jaar
  jaren <- sheets[str_detect(sheets,"[:digit:]{4}")] %>% sort()
  
  #als aantal_jaren meenemen is ingesteld: 
  #vector jaren inkorten en laatste x jaren overhouden
  if(!is.null(aantal_jaren_meenemen)){

    jaren <- tail(jaren, aantal_jaren_meenemen)

  }
  
  #Voor iedere sheet met een jaartal: data per pc4 & gemeente ophalen
  vaccinatiegraad_rivm_ggd <- lapply(jaren, function(x){
    
    df <- openxlsx::read.xlsx(bestand,sheet = x) %>%
      #kolommen X47,X48 en X49 bestaan alleen in 2023.
      #in 2024 zijn kolommen 44 t/m 46 anders dan voorheen; ook verwijderen
      select(-any_of(paste0("X",44:52)))
    
    
    #Vector met bruikbare kolomnamen maken.
    #Kolominformatie ophalen uit rijen 1:6 (3,8 in excel)
    kolominfo <- df[1:6,-1] %>%
      #transponeren zodat we tidyr::fill() kunnen gebruiken
      t() %>%
      as.data.frame() %>% 
      #van boven naar beneden aanvullen waar NA (dus eigenlijk vlnr)
      fill(everything()) %>%
      #kolomnaam opbouwen uit alle rij-info excl geboortejaar
      mutate(kolomnamen = paste(`1`,`2`,`3`,`4`,`6`) %>%
               str_trim() %>%
               str_replace_all(" ","_")) %>%
      pull(kolomnamen) 
    
    #kolomnamen globaal toewijzen voor hergebruik in vaccinatiegraad_rivm_ggd
    kolomnamen <- c("id",kolominfo)
    #namen kolommen toewijzen (gedefinieerd in cijfers_nl)
    names(df) <- kolomnamen
    
    #Tabel bij begint en eind afknippen .
    start_tabel = which(df$id == "Rijlabels")
    eind_tabel = which(df$id == "Eindtotaal")
    
    df <- df %>%
      select(id,
             #Alleen ophalen wat boven is opgegeven
             matches(cohorten) &
               matches(vaccinsoort) &
               matches(vaccinatietoestand)) %>%
      #tabel filteren op rijnummers waar gemeente & pc4 data in zit 
      filter(row_number() > start_tabel & row_number() < eind_tabel) %>%
      #Gemeentenaam uit rij met postcodes halen voor apparte kolom
      mutate(gemeentenaam = case_when(
        #Als er een letter in de variabele zit is het een gemeente ipv postcode
        str_detect(id, "[:alpha:]") ~ id,
        TRUE ~ NA)) %>%
      #gemeentenaam aanvullen van boven naar beneden
      tidyr::fill(gemeentenaam, .direction = "down") 
    
    df %>%
      #Jaar; aanpassen v. rapportage jaar naar werkelijk jaar
      mutate(jaar = as.numeric(x) -1) %>%
      #alle aantallen naar numeric & percentage afronden
      mutate(across(contains("_"), as.numeric),
             across(contains("%"),~ round(.x * 100, 1))) %>%
      #Na's vervangen met 0 zodat NA als <5 clienten geteld kan worden
      mutate_if(is.numeric, replace_na,0) 
  }) %>% 
    #dataframes koppelen
    do.call(rbind,.)
  
  #Op verzoek:  Kaart zonder exacte percentages maken.maar met categorieën
  if(!percentages_in_kaart) {
    
    vaccinatiegraad_rivm_ggd <- vaccinatiegraad_rivm_ggd %>%
      mutate(across(contains("%"), ~
                      
                      case_when(
                        .x < 75 ~  "< 75%",
                        .x < 85 ~  ">= 75%",
                        .x < 90 ~  ">= 85%",
                        .x < 95 ~  ">= 90%",
                        .x >= 95 ~ ">= 95%",
                        TRUE ~ "?"
                      )))
  }
  
  #Sheetnamen in RIVM excel zijn rapportagejaren. gaat over voorgaande jaren. dus: -1
  jaren <- as.numeric(jaren) - 1
  
  
  #Postcodedata uit df halen
  vaccinatiegraad_pc4 <- vaccinatiegraad_rivm_ggd %>% 
    filter(id %>% str_detect("[:digit:]"))
  
  #PC4 is niet alle jaren gelijk; missing pc4 als lege "grijze" polygonen weergeven in die jaren. 
  #PC4 aanvullen in jaren waar er pc4 ontbreken
  
  #koppeltabel maken van gemeentenamen en pc4
  gemeentenamen_pc4 <- vaccinatiegraad_pc4 %>%
    group_by(id, gemeentenaam) %>%
    summarise() %>%
    ungroup()
  
  
  #dataframe met alle mogelijke combinaties pc4/jaar maken
  pc4_jaar_df <- expand.grid(c(gemeentenamen_pc4$id,
                               #Eventueel handmatig toegevoegde postcodes meenemen
                               toegevoegde_postcodes),jaren) %>%
    #gemeentenaam koppelen
    left_join(gemeentenamen_pc4, by = c("Var1" = "id")) %>%
    #PC4 5048 nog een gemeentenaam geven
    mutate(gemeentenaam = ifelse(is.na(gemeentenaam),
                                 "Tilburg",gemeentenaam))
  
  #namen gelijk trekken met vaccinatiegraad_pc4
  names(pc4_jaar_df) <- c("id","jaar", "gemeentenaam_2")
  
  #Lege pc4 aan vaccinatiegraaddata koppelen
  vaccinatiegraad_pc4 <- vaccinatiegraad_pc4 %>%
    full_join(pc4_jaar_df, by = c("id","jaar")) %>%
    mutate(gemeentenaam = ifelse(is.na(gemeentenaam),
                                 gemeentenaam_2,
                                 gemeentenaam)) %>%
    select(-gemeentenaam_2) %>%
    mutate_if(is.numeric, replace_na,0)
  
  
  #PC4 shapefile inlezen en vaccinatiedata data koppelen  
  pc4_sf <- st_read(pc4_shp,
                    quiet = T) %>%
    left_join(vaccinatiegraad_pc4, by = c("PC4" = "id")) %>%
    mutate(id = PC4,
           niveau = "PC4") %>%
    #Alle PC4 waar jaar missing is, zijn niet in HvB
    filter(!is.na(jaar),
           gemeentenaam == params$gemeentenaam
    ) %>%
    st_transform(4326)
  
  
  
  #Dataframe met labels voor kaartlagen maken
  categorieen <- names(vaccinatiegraad_rivm_ggd %>% select(-c(id,gemeentenaam,jaar))) %>%
    str_remove("_Aantal.*|_%") %>%
    unique()
  
  niveaus <- c("PC4","gemeente")
  
  #Alle combinaties van categorie & nivea, + een jaarvariabele
  kaartlagen <- expand.grid(categorieen, max(jaren), niveaus) %>%
    data.frame()
  
  names(kaartlagen) <- c("categorie","jaar","niveau") 
  
  kaartlagen <- kaartlagen %>% 
    mutate(
      categorie = as.character(categorie), #van factor naar character
      cohort = str_extract(categorie,cohorten),
      vaccinsoort = str_extract(categorie,glue("(?<={cohorten}_)[[:alpha:]|\\(|\\)]*")),
      #o.b.v. de variabele vacctoestand_ipv_leeftijd
      #Wordt een leeftijd of de vaccinatietoestand uit de categorie gehaald
      leeftijd_of_vaccinatietoestand =  case_when(
        vacctoestand_ipv_leeftijd ~ str_extract(categorie, paste0(vaccinatietoestand,collapse = "|")) %>%
          str_replace_all("_"," "),
        TRUE ~ str_extract(categorie,"[:digit:]+") %>% paste("jaar")),
      #vaccinatietoestand ook altijd los meenemen
      vaccinatietoestand = str_extract(categorie, paste0(vaccinatietoestand,collapse = "|")) %>% 
        str_replace_all("_"," "),
      naam_noemer = glue("{cohort} ({leeftijd_of_vaccinatietoestand})"),
      cohort = str_remove(cohort,"_"),    #_ in 'adolescente_meisjes' verwijderen
      groepnaam = glue("{vaccinsoort} {cohort} ({leeftijd_of_vaccinatietoestand}) per {niveau} {max(jaren)}"),
      data = case_when(niveau == "PC4" ~ "pc4_sf",
                       niveau == "gemeente" ~ "gemeente_sf"))
  
  if(!jaar_in_kaartlaag) {
    kaartlagen$groepnaam <- kaartlagen$groepnaam %>% str_remove(" [:digit:]{4}$")
  }
  
  
  
  #Functie om polygoonlaag te maken voor leaflet kaart RIVM vaccinatiegraden op PC4 en gemeenteniveau
  maak_leaflet_polygon_layer <- function(kaart, data, huidig_jaar, categorie,groepnaam, 
                                         naam_noemer, percentages_in_kaart = T, 
                                         kleuren_graad = c("green" = 95,
                                                           "#FB6A4A" = 90,
                                                           "#EF3B2C" = 85,
                                                           "#A50F15" = 75,
                                                           "#67000D" = 0)
  ){
    
    # print(glue("{groepnaam} verwerken"))
    
    #referenties naar relevante variabelen maken
    clienten = glue("{categorie}_Aantal_clienten")
    percentage = glue("{categorie}_%")
    
    #kleuren vacc. graad: waarden & kleurcodes uit elkaar halen
    afkapwaarden <- kleuren_graad %>% unname()
    kleuren <- kleuren_graad %>% names()
    
    
    if(percentages_in_kaart){
      #Kleuren aan dataframe toewijzen o.b.v vaccinatiegraad
      data = data %>%
        mutate(
          
          kleuren = case_when(
            #Minder dan 5 clienten = niet laten zien
            !!sym(clienten) < minimum_verbergen ~ "grey",
            #95% of hoger = WHO norm
            !!sym(percentage) >= afkapwaarden[1] ~ kleuren[1],
            #Alles daaronder is verschillende graden van slecht
            !!sym(percentage) >= afkapwaarden[2] ~ kleuren[2],
            !!sym(percentage) >= afkapwaarden[3] ~  kleuren[3],
            !!sym(percentage) >= afkapwaarden[4] ~  kleuren[4],
            TRUE ~  kleuren[5])
        )
      
    } else{
      
      #Kleuren anders toewijzen wanneer er voor !percentages_in_kaart is gekozen
      data = data %>%
        mutate(kleuren = case_when(
          !!sym(clienten) < minimum_verbergen ~ "grey",
          !!sym(percentage) == ">= 95%" ~  "#004529",
          !!sym(percentage) == ">= 90%" ~  "#ADDD8E",
          !!sym(percentage) == ">= 85%" ~  "#FB6A4A",
          !!sym(percentage) == ">= 75%" ~  "#A50F15",
          TRUE ~ "#67000D"
          
        ))
    }
    
    #Voor de polygoon willen we alleen het laatste jaar laten zien
    sf <- data %>% filter(jaar == huidig_jaar)
    
    #Popup Labels maken per polygoon
    labels <- lapply(sf$id, function(x){
      
      titel_popup <- if(is.null(alt_popup_titel)){
        glue("<p><strong>Vaccinatiegraad {groepnaam}</strong></p>") 
      }else{
        alt_popup_titel
      }
      
      #Subset maken voor specifieke gemeente/pc4
      temp_df = sf %>% filter(id == x)
      
      #Naam gebied toewijzen
      naam = case_when(
        #Label gebied voor "gemeente"
        temp_df$niveau == "gemeente" ~ glue("<p><strong>{temp_df$id}</strong></p>"),
        #Label gebied voor PC4
        temp_df$niveau == "PC4" ~ glue("<p><strong> PC4: {temp_df$id} ({temp_df$gemeentenaam}) </strong></p>")
      )
      
      #Als < 5 clienten niks weergeven & functie afbreken
      if(temp_df[[clienten]] < minimum_verbergen){
        
        return(glue("<p><strong>Vaccinatiegraad {groepnaam}</strong></p>
                {naam}
                <p>Aantal {naam_noemer} te laag om vaccinatiegraad te weergeven (< {minimum_verbergen})</p>"))
        
      }
      
      #lege disclaimervar maken, vullen als clienten < minimum_waarschuwing
      disclaimer = ""
      if(temp_df[[clienten]] < minimum_waarschuwing){
        
        disclaimer = glue("<p style='color:red;' > LET OP: Minder dan {minimum_waarschuwing} {naam_noemer} </p>")
      }
      
      
      if(!percentages_in_kaart){
        #% categorieen tonen
        melding = glue("<strong><span style='color:{temp_df$kleuren}'>{temp_df[[percentage]]} </span></strong> (uit totaal {temp_df[[clienten]]} {naam_noemer})")
      } else {
        #wel percentages tonen
        
        #Bij percentages boven de 95% willen we niet het exacte percentage laten zien maar ">=95%"
        if(temp_df[[percentage]] >= 95 | temp_df[[percentage]] == ">= 95%" ){
          melding = glue("<strong><span style='color:{temp_df$kleuren}'> >95% </span></strong> uit totaal {temp_df[[clienten]]} {naam_noemer}")
        } else{
          melding = glue("<strong><span style='color:{temp_df$kleuren}'>{temp_df[[percentage]]}% </span></strong> (uit totaal {temp_df[[clienten]]} {naam_noemer})")
        }
      }
      
      
      
      #Data uit meerjaren_df halen voor trendlijntje & kleurtjes
      trend_df <- data %>% filter(id == x)
      
      
      
      tabel_header = NULL
      
      #labels van jaren aanpassen als er alternatieve jaarnamen zijn toegewezen
      if(!is.null(names(jaren))){
        
        #Het woordgedeelte vd alteratieve namen wordt een header voor de tabel
        tabel_header <- c(length(jaren),1)
        
        names(tabel_header) <-  c(names(jaren) %>% str_remove("[:digit:]{4}") %>% str_trim() %>% unique(), " ")
        
        
        #De kolomkoppen worden het 'jaar' gedeelte vd alternatieve namen
        nieuwe_jaarlabels <- str_extract(names(jaren),"[:digit:]{4}")
        trend_df$jaar <- nieuwe_jaarlabels
      }
      
      
      #Trendwaarden als vector opslaan; alles op of hoger dan 95 naar 95.
      if(percentages_in_kaart){
        trendwaarden <- ifelse(trend_df[[percentage]] >= 95, 95,trend_df[[percentage]])
        
      } else{
        
        #als we categorieen willen laten zien kunnen we niet een trendlijn
        #maken op de exacte cijfers. Die zijn er immers niet.
        #Daarom 1,2,3. zodat we  kunnen laten zien wat hoger / lager is
        trendwaarden <- case_when(
          
          trend_df[[percentage]] == "< 75%" ~ 1,
          trend_df[[percentage]] == ">= 75%" ~ 2,
          trend_df[[percentage]] == ">= 85%" ~ 3,
          trend_df[[percentage]] == ">= 90%" ~ 4,
          trend_df[[percentage]] == ">= 95%" ~ 5
          
        )
        
      }
      

      trend <- trend_df %>%
        select(jaar,!!sym(percentage), kleuren) %>%
        mutate(percentage = case_when(
          !!sym(percentage) >= 95 ~ ">= 95",
          TRUE ~ as.character(!!sym(percentage))
        ),
        #Kleurcodering cellen:
        percentage = cell_spec(percentage,
                               background = kleuren,
                               color = "white")) %>%
        data.frame() %>%
        select(jaar,percentage) %>%
        #Breed maken
        pivot_wider(names_from = jaar, values_from = percentage) %>%
        #lege trendlijnvariabele maken
        mutate(trendlijn = "") %>%
        kableExtra::kable(escape = F,
                          format = "html") %>%
        kable_minimal() %>%
        #Plaatje van de trendlijn toevoegen in kolom na jaarkolommen
        column_spec(length(jaren) + 1, image = spec_plot(list(trendwaarden), same_lim = T)) %>% 
        add_header_above(tabel_header) %>% 
        row_spec(1, extra_css = "white-space: nowrap;") #forceren dat ">= 95" geen linebreak heeft
      
      #Geen trend-tabel toevoegen als er slechts 1 jaar in de data zit
      if(length(jaren) < 2){
        trend = ""
      }
      
      #Werkelij de popup samenstellen
      return(
        glue(
          "
      {titel_popup}
      {naam}
      {disclaimer}
      {melding}
      {trend}
      "
          
        )
        
      )
      
      
    })
    
    
    #Kaartlaag toevoegen
    kaart %>%
      addPolygons(data        = sf,
                  color       = "black", 
                  fillColor   = sf$kleuren,
                  opacity     = 0.5,
                  weight      = 1,
                  fillOpacity = 0.5,
                  group       = groepnaam,
                  popup       = labels,
                  labelOptions = labelOptions(
                    style = list(direction = "auto")),
                  highlightOptions = highlightOptions(color = "black", weight = 3,
                                                      bringToFront = TRUE))
    
    
    
  }
  
  
  
  centroide = st_centroid(st_union(pc4_sf))
  
  #Basiskaart aanmaken
  kaart <- leaflet() %>%
    #addTiles() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    setView(lng = centroide[[1]][1], lat = centroide[[1]][2], zoom = 11)
  
  
  #We willen gemeentepolygonen bovenaan hebben. dataframe kaartlagen sorteren
  kaartlagen <- kaartlagen %>% arrange(desc(niveau))
  
  #Kleuren en waarden voor de kaartjes. Alles >= de numerieke waarde krijgt die kleur
  #Moet een named numeric vector van 5 elementen zijn.
  kleuren_vaccinatiegraad = c("#004529" = 95,
                              "#ADDD8E" = 90,
                              "#FB6A4A" = 85,
                              "#A50F15" = 75,
                              #hieronder op 0 laten staan. alles < het voorgaande
                              "#67000D" = 0)
  
  
  minimum_verbergen = 15 #minder clienten = niet laten zien
  minimum_waarschuwing = 50 #minder clienten = waarschuwing geven
  
  alt_popup_titel = "<p><strong>Vaccinatiegraad zuigelingen per geboortejaar</strong></p>"
  
  kaartlagen <- kaartlagen %>%
    filter(niveau == "PC4")
  
  #Loop over alle kaartlagen
  for (i in 1:nrow(kaartlagen)) {
    
    #Pak info van de i'de rij in het kaartlagen dataframe
    temp_kaartlaag <- kaartlagen[i,]
    
    #voeg laag toe aan basiskaart
    kaart <- kaart %>%
      maak_leaflet_polygon_layer(data = get(temp_kaartlaag$data),
                                 #jaar is altijd het laatste jaar
                                 huidig_jaar = temp_kaartlaag$jaar,
                                 categorie = temp_kaartlaag$categorie,
                                 groepnaam = temp_kaartlaag$groepnaam,
                                 naam_noemer = temp_kaartlaag$naam_noemer,
                                 #functie werkt alleen met 5 elementen
                                 #in kleuren_graad.
                                 #Wordt genegeerd bij !percentages_in_kaart
                                 kleuren_graad = kleuren_vaccinatiegraad,
                                 percentages_in_kaart = percentages_in_kaart
      )
    
  }
  
  #Groepcontrols toevoegen aan kaart
  kaart <- kaart %>%
    addLayersControl(
      baseGroups = kaartlagen$groepnaam,
      options = layersControlOptions(collapsed = FALSE))
  
  #Legenda maken voor kaart
  
  #Labels voor legenda ophalen
  labels_legenda = kleuren_vaccinatiegraad %>% unname() %>% paste0(">= ",.,"%")
  
  #Laatste label = lager dan 4e label
  labels_legenda[5] <- labels_legenda[4] %>% str_replace(">=","<")
  
  #Missing label toevoegen
  cohorten_te_weinig <- kaartlagen$naam_noemer %>% unique() %>% paste(collapse = " of<br>")
  
  labels_legenda[6] <- glue("< {minimum_verbergen} {cohorten_te_weinig}")
  
  
  
  #Legenda toevoegen aan kaart
  kaart <- kaart %>% addLegend(colors = kleuren_vaccinatiegraad %>%
                                 names() %>%
                                 c("grey"),
                               labels = labels_legenda)
  
  
  return(kaart)
  
  
}
