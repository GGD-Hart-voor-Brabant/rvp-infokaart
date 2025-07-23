
# cirkel met getal --------------------------------------------------------


# Glue package dient geinstalleerd te zijn voor deze functie

bol_met_cijfer_rvp <- function(getal, kleur = "#b41257", kleur_outline = "#FFFFFF", kleur_text = "#FFFFFF"){


  # Voeg de ingevoerde informatie op de juiste plekken in de svg code met behulp van glue
  svg_code <- glue::glue('<svg class="cirkel" role="img" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" xml:space="preserve" style="shape-rendering:geometricPrecision; text-rendering:geometricPrecision; image-rendering:optimizeQuality;"
                viewBox="0 0 50 50">
                <title>{getal}</title>
                
                <g id="circle">
                    <circle style="fill:{kleur};" cx="25" cy="25" r="20" stroke = "{kleur_outline}">
                    </circle>
                    <text x=25 y="25" text-anchor="middle" fill="{kleur_text}" stroke="{kleur_text}" stroke-width="1px" dy=".3em" font-size="1em">{getal}</text>
                </g>
                
                </svg>')
  
  
  return(svg_code)
}


# staafdiagram ------------------------------------------------------------

maak_staafdiagram <- function(data, waarde = "percentage", gemeentenaam, periode = "jaar", categorie_kleur = "regio", ymin = 0,
                              kleuren_categorie = c(kleur_gemeente, kleur_regio, kleur_nl), hoek_tekst = 90, grootte_tekst = 5,
                              ggd_naam = "Hart voor Brabant", ggd_afkorting = "HvB"
                              
                              ){
  
  hjust_plot <- ifelse(hoek_tekst == 90, 1.5,.5)
  vjust_plot <- ifelse(hoek_tekst == 90, .5, 1.5)
  data <- data %>%
    mutate(
      regio = case_when(
        regio == "Nederland" ~ "NL",
        regio == ggd_naam ~ ggd_afkorting,
        TRUE ~ regio)
      )

  
  #Als de categorie per regio is; vaste volgorde waarbij gemeente eerst is
  if(gemeentenaam %in% data[[categorie_kleur]]){
    
    namen_categorie <- c(gemeentenaam, ggd_afkorting, "NL")
  
  }else{
    
    namen_categorie <-  data[[categorie_kleur]] %>% unique()
    
  }
  
  # Make sure kleuren_categorie matches the length of namen_categorie
  kleuren_categorie <- kleuren_categorie[1:length(namen_categorie)]
  names(kleuren_categorie) <- namen_categorie
  
  # Categorien op volgorde zetten
  data <- data %>%
    mutate(!!sym(categorie_kleur) := factor(!!sym(categorie_kleur), levels = namen_categorie))
  
  #vector textkleuren maken; bij 4 categorieën is de laatste zwarte tekst.
  kleuren_text <- rep("#FFFFFF",3)
  if(length(kleuren_categorie) > 3){
    kleuren_text <- c(kleuren_text,"#000000") 
  }
  
  
  ggplot(data,
         fill = "transparent"
         
         ) +
    geom_bar(mapping = aes(x = factor(!!sym(periode)),
                           y = !!sym(waarde), 
                           fill = factor(!!sym(categorie_kleur))),
             stat = "identity", position = position_dodge2()
             )+

    geom_hline(yintercept = ymin) +
    coord_cartesian(ylim = c(ymin,100), expand = FALSE) +
    theme_minimal() +
    ylab("") + 
    xlab("") +
    scale_fill_manual("", values = kleuren_categorie) +
    
    
    ggnewscale::new_scale_color() + 
    geom_text(mapping = aes(x = factor(!!sym(periode)),
                            y = !!sym(waarde),
                            group = factor(!!sym(categorie_kleur)),
                            label = sprintf("%.1f",!!sym(waarde)),
                            color = factor(!!sym(categorie_kleur))
    ),
    size = grootte_tekst,
    show.legend = FALSE,
    position = position_dodge(width = 0.9),
    angle = hoek_tekst,
    hjust = hjust_plot,
    vjust = vjust_plot
    )+
    

    
    scale_color_manual("",values = kleuren_text) + 
    
    theme(legend.position = "top",
          legend.text = element_text(size = 14),
          axis.text.y = element_blank(),
          axis.text.x = element_text(face = "bold",
                                     size = 14),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank()
    )
  
  
}
