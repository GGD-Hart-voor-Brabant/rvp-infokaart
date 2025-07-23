#Functie die excel-bestanden RVP data van rivm omzet naar formaat dat gebruikt kan worden in R 
lees_cijfers_nl <- function(jaar, bestand, zonder_leeftijd,
                            verwijder_kolommen = paste0("X",44:52), #Vanaf HPV doet niet mee in grotere defaultdataset
                            behoud_kolommen = NULL
                            
                            ){
  
  df <- openxlsx::read.xlsx(bestand, sheet = jaar) %>%
    #sommige kolommen bestaan alleen in recente jaren. optioneel verwijderen tbv rbind meerdere jaren
    select(-any_of(verwijder_kolommen))
  
  if(!is.null(behoud_kolommen)){
    
    df <- df %>% 
      select("Totaal.Nederland", behoud_kolommen)
    
  }
  
  
  #Tabellen zijn onhandig geformatteerd voor verwerking in R. 
  #Informatie die over de inhoud kolommen gaat staat op meerdere rijen en niet boven elke kolom
  
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
  
  #namen aan df toewijzen
  names(df) <- kolomnamen
  
  
  df_long <- df %>%
    select(#Alleen de variabelen ophalen die gekozen zijn 
      matches(cohorten) & 
        matches(vaccinsoort) &
        matches(vaccinatietoestand)) %>%
    #Rij 7 = de rij waar de cijfers NL staan (rij 9 in Excel)
    filter(row_number() %in% c(7)) %>% 
    #Alles lang maken
    pivot_longer(everything())
  
  if(zonder_leeftijd){
    df_long <- df_long %>%
      mutate(
        #Type cijfer ophalen (aantal gevaccineerd,aantal clienten en %)
        type_cijfer = str_extract(name,"(?<=nvt_).*$"), 
        #Inhoudelijke naam ophalen
        name = str_remove(name,"_nvt.*")) %>%
      #Weer breed maken; kolom per type cijfer
      pivot_wider(values_from = value,
                  names_from = type_cijfer) %>%
      #Jaar; aanpassen v. rapportage jaar naar werkelijk jaar
      mutate(jaar = as.numeric(jaar) - 1)
  } else{
    
    df_long <- df_long %>% 
      mutate(
        type_cijfer = str_extract(name, "(?<=[:digit:]{1,2}_jaar_).*$"),
        name = str_remove(name, "_[:digit:]{1,2}_jaar.*")
      ) %>% 
      #Weer breed maken; kolom per type cijfer
      pivot_wider(values_from = value,
                  names_from = type_cijfer) %>%
      #Jaar; aanpassen v. rapportage jaar naar werkelijk jaar
      mutate(jaar = as.numeric(jaar) - 1)
    
  }
  
  return(df_long)
  
} 



lees_cijfers_gemeente_pc4 <- function(jaar, bestand, zonder_leeftijd,
                                      verwijder_kolommen = paste0("X",44:52), #Vanaf HPV doet niet mee in grotere defaultdataset
                                      behoud_kolommen = NULL){
  
  
  df <- openxlsx::read.xlsx(bestand, sheet = jaar) %>%
    #sommige kolommen bestaan alleen in recente jaren. optioneel verwijderen tbv rbind meerdere jaren
    select(-any_of(verwijder_kolommen))
  
  if(!is.null(behoud_kolommen)){
    
    df <- df %>% 
      select("Totaal.Nederland", behoud_kolommen)
    
  }
  
  
  #Tabellen zijn onhandig geformatteerd voor verwerking in R. 
  #Informatie die over de inhoud kolommen gaat staat op meerdere rijen en niet boven elke kolom
  
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
  
  #namen aan df toewijzen
  names(df) <- kolomnamen
  
  #Tabel bij begint en eind afknippen .
  start_tabel = which(df$id == "Rijlabels")
  eind_tabel = which(df$id == "Eindtotaal")
  
  df <- df %>%
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
    mutate(jaar = as.numeric(jaar) -1) %>%
    #alle aantallen naar numeric & percentage afronden
    mutate(across(contains("_"), as.numeric),
           across(contains("%"),~ round(.x * 100, 1))) %>%
    #Na's vervangen met 0 zodat NA als <5 clienten geteld kan worden
    mutate_if(is.numeric, replace_na,0) 
  
} 
  
  
  



#Functie die statline data & excel-bestanden rivm leest & combineert tbv infokaart RVP
lees_data_rivm <- function(excel_bestand, 
                           ggd_naam = "GGD Hart voor Brabant",
                           zonder_leeftijd = FALSE){
  
  # statline data ophalen ---------------------------------------------------
  gemeenten_per_GGD <- cbs_get_data('85385NED',
                                    select = c("Code_1",
                                               "Naam_2",
                                               "Code_16",
                                               "Naam_17")) %>% #2025: CBS heeft kolommen toegevoegd code14 en naam15 schuiven twee op
    mutate(Code_1 = str_trim(Code_1, side = "both"),
           Naam_17 = str_trim(Naam_17, side = "both")) %>%
    filter(Naam_17 %in% c(ggd_naam))
  
  
  
  statline_vacc_data <- cbs_get_data("50117NED", 
                                     catalog = "RIVM") %>% 
    cbs_add_label_columns() %>% 
    mutate(jaar = str_extract(Perioden, "[:digit:]{4}") %>% as.numeric() - 1, #-1 omdat statline rapportagejaar ipv meetjaar hanteert
           RegioS = str_trim(RegioS)) %>% 
    filter(RegioS %in% c("NL01", gemeenten_per_GGD$Code_1)) %>% 
    select(jaar, RegioS_label, Vaccinaties_label, Populatie_1, GevaccineerdenMetLeeftijdsgrens_2, VaccinatiegraadMetLeeftijdsgrens_3)
  
  names(statline_vacc_data) <- c("jaar", "regio", "vaccinatie", "populatie","gevaccineerden_lft", "vaccinatiegraad_lft")
  
  
  
  # excel data rivm ophalen -------------------------------------------------
  #RIVM gegevens 2022 2024 zonder leeftijdsgrens
  bestand = excel_bestand
  wb <- openxlsx::loadWorkbook(bestand)
  #checken welke sheets er zijn
  sheets <- wb %>% openxlsx::sheets()
  
  #Aanname: Alle met een 4 cijfers is een jaar
  jaren <- sheets[str_detect(sheets,"[:digit:]{4}")] %>% sort()
  
  cohorten = c("Zuigelingen","Kleuters","Schoolkinderen","Adolescente_meisjes", "Adolescenten","Zwangeren")
  vaccinsoort = c("D\\(K\\)TP","Hib","HepB","Pneu","BMR","MenC/ACWY","HPV","Alles","MATK")
  vaccinatietoestand = c("Primaire_serie","Basisimmuun","Volledig_afgesloten","Volledige_deelname","Totaal*")
  #DATA NL ophalen
  #Voor iedere sheet met een jaartal: Data NL ophalen. 
  cijfers_nl <- lapply(jaren, function(x){
    
    # print(glue::glue("cijfers nl: jaar {x}"))
    #Hulpfunctie aanroepen die landelijke cijfers uit excel per jaar naar dataframe omzet
    #default worden kolommen 46 t/m 52 verwijderd 
    lees_cijfers_nl(jaar = x, bestand = excel_bestand,  zonder_leeftijd = zonder_leeftijd)
    
  }) %>% 
    #dfs per jaar samenvoegen
    do.call(rbind,.) %>%
    mutate(`%` = as.numeric(`%`)*100 %>% round(1)) %>% 
    #select(-`NA`) %>%
    filter(!is.na(Aantal_clienten)) %>% 
    mutate(cohort = str_extract(name, paste0(cohorten, collapse = "|")),
           vaccinatie = str_extract(name, paste0(vaccinsoort, collapse = "|")),
           vaccinatietoestand = str_extract(name, paste0(vaccinatietoestand, collapse = "|")))
  
  
  #Voor iedere sheet met een jaartal: data per pc4 & gemeente ophalen
  vaccinatiegraad_rivm_ggd <- lapply(jaren, function(x){
    # print(glue::glue("cijfers per gm/pc4: jaar {x}"))
    
    lees_cijfers_gemeente_pc4(jaar = x, bestand = excel_bestand,  zonder_leeftijd = zonder_leeftijd)

  }) %>% 
    #dataframes koppelen
    do.call(rbind,.) %>% 
    pivot_longer(contains("_"))
  
  #Namen anders construeren afh. van of het zlft of inccleeftijd is
  if(zonder_leeftijd){
    
    vaccinatiegraad_rivm_ggd <- vaccinatiegraad_rivm_ggd %>% 
      mutate(
        #Type cijfer ophalen (aantal gevaccineerd,aantal clienten en %)
        type_cijfer = str_extract(name,"(?<=nvt_).*$"), 
        #Inhoudelijke naam ophalen
        name = str_remove(name,"_nvt.*")) %>%
      #Weer breed maken; kolom per type cijfer
      pivot_wider(values_from = value,
                  names_from = type_cijfer) %>% 
      mutate(cohort = str_extract(name, paste0(cohorten, collapse = "|")),
             vaccinatie = str_extract(name, paste0(vaccinsoort, collapse = "|")),
             vaccinatietoestand = str_extract(name, paste0(vaccinatietoestand, collapse = "|")))
    
  } else {
    vaccinatiegraad_rivm_ggd <- vaccinatiegraad_rivm_ggd %>%
      mutate(
        type_cijfer = str_extract(name, "(?<=[:digit:]{1,2}_jaar_).*$"),
        name = str_remove(name, "_[:digit:]{1,2}_jaar.*")
      ) %>% 
      #Weer breed maken; kolom per type cijfer
      pivot_wider(values_from = value,
                  names_from = type_cijfer) %>% 
      mutate(cohort = str_extract(name, paste0(cohorten, collapse = "|")),
             vaccinatie = str_extract(name, paste0(vaccinsoort, collapse = "|")),
             vaccinatietoestand = str_extract(name, paste0(vaccinatietoestand, collapse = "|")))
  }
  
  
  
  
  
  
  
  # statline & excel koppelen per gemeente en NL ----------------------------
  
  #TODO bespreken: We gebruiken de data zonder lftgrens vanaf 2020, daarvoor gebruiken we de statline data met lftgrens
  
  #RIVM excel; gemeente + nl + regio koppelen
  vaccinatiegraad_2021_2024_gemeente <- vaccinatiegraad_rivm_ggd %>% 
    filter(!str_detect(id,"[:digit:]")) %>% 
    select(-c(id, name)) %>% 
    rename("regio" = gemeentenaam)
  
  #regionaam o.b.v. ggd_naam - "GGD "
  regionaam = ggd_naam %>% str_remove("GGD ")
  
  #regiocijfers berekenen o.b.v. som gemeentecijfers
  vaccinatiegraad_2021_2024_hvb <- vaccinatiegraad_2021_2024_gemeente %>%
    group_by(jaar, cohort, vaccinatie, vaccinatietoestand) %>% 
    summarise(Aantal_clienten = sum(Aantal_clienten),
              Aantal_clienten_gevaccineerd = sum(Aantal_clienten_gevaccineerd)) %>% 
    ungroup() %>% 
    mutate(`%` = Aantal_clienten_gevaccineerd/Aantal_clienten*100,
           regio = ggd_naam) %>% 
    select(regio, jaar, Aantal_clienten, Aantal_clienten_gevaccineerd, `%`, cohort, vaccinatie, vaccinatietoestand)
  
  
  vaccinatiegraad_2021_2024 <- vaccinatiegraad_2021_2024_gemeente %>% 
    rbind(vaccinatiegraad_2021_2024_hvb) %>% #regiodata koppelen
    rbind( #nl data koppelen
      cijfers_nl %>% 
        mutate(regio = "Nederland" ) %>%  
        select(regio, jaar, Aantal_clienten, Aantal_clienten_gevaccineerd, `%`, cohort, vaccinatie, vaccinatietoestand)
    )
  
  #Statline data; regiodata maken en df gelijktrekken met exceldata
  
  vaccinatiegraad_tm_2019_nl_gemeente <- statline_vacc_data %>% 
    filter(jaar < 2020) %>% 
    mutate(
      cohort = case_when(
        str_detect(vaccinatie,"2 jaar") ~ "Zuigelingen",
        str_detect(vaccinatie,"14 jaar") ~ "Adolescenten",
        str_detect(vaccinatie,"5 jaar") ~ "Kleuters",
        str_detect(vaccinatie,"10 jaar") ~ "Schoolkinderen",
        TRUE ~ vaccinatie), #later verder invullen als uberhaupt nodig is
      vaccinatietoestand = case_when(
        str_detect(vaccinatie, "basisimmuum") ~ "Basisimmuun",
        TRUE ~  vaccinatie #later verder invullen als uberhaupt nodig is
      ),
      
      vaccinatie = case_when(
        str_detect(vaccinatie, "DKTP") ~ "D(K)TP",
        str_detect(vaccinatie, "Hib") ~ "Hib",
        str_detect(vaccinatie, "Hepatitis B") ~ "HepB",
        str_detect(vaccinatie, "Pneumokokken") ~ "Pneu",
        str_detect(vaccinatie, "BMR") ~ "BMR",
        str_detect(vaccinatie, "MenC/ACWY") ~ "MenC/ACWY",
        str_detect(vaccinatie, "Volledige deelname RVP") ~ "Alles",
        str_detect(vaccinatie, "HPV") ~ "HPV",
        TRUE ~ vaccinatie #later verder invullen als uberhaupt nodig is
      )
    )
  
  #regionale dataset maken
  vaccinatiegraad_tm_2019_hvb <- vaccinatiegraad_tm_2019_nl_gemeente %>%
    filter(regio != "Nederland",
           !is.na(populatie)) %>% 
    group_by(jaar, cohort, vaccinatie, vaccinatietoestand) %>% 
    summarise(populatie = sum(populatie),
              gevaccineerden_lft = sum(gevaccineerden_lft)) %>% 
    ungroup() %>% 
    mutate(vaccinatiegraad_lft = gevaccineerden_lft / populatie * 100,
           regio = ggd_naam) %>% 
    select(jaar, regio, vaccinatie, populatie, gevaccineerden_lft,
           vaccinatiegraad_lft, cohort, vaccinatietoestand)
  
  vaccinatiegraad_tm_2019 <- rbind(
    vaccinatiegraad_tm_2019_nl_gemeente,
    vaccinatiegraad_tm_2019_hvb ) %>% 
    rename('%' = 'vaccinatiegraad_lft')
  
  
  vaccinatiegraad_totaal <- rbind(
    vaccinatiegraad_2021_2024 %>% select(regio, jaar, `%`,cohort, vaccinatie, vaccinatietoestand),
    vaccinatiegraad_tm_2019 %>%  select(regio, jaar, `%`,cohort, vaccinatie, vaccinatietoestand)
  )
  
  vaccinatiegraad_totaal <- vaccinatiegraad_totaal %>% 
    rename("percentage" = "%")
  
  return(vaccinatiegraad_totaal)
#  write.csv(vaccinatiegraad_totaal,"vaccinatiegraad_2014_2024.csv", row.names = F)
  
}




