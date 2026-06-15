diag_aktiva_foretag_storleksklass_lan <- function(
    region_vekt = "20",			# Val av region. Finns: "00", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25", "SE0", "SE00", "SE1", "SE11", "SE110", "SE12", "SE121", "SE122", "SE123", "SE124", "SE125", "SE2", "SE21", "SE211", "SE212", "SE213", "SE214", "SE22", "SE221", "SE224", "SE23", "SE231", "SE232", "SE3", "SE31", "SE311", "SE312", "SE313", "SE32", "SE321", "SE322", "SE33", "SE331", "SE332"
    cont_klartext = "Antal aktiva företag",			 # Max 1 åt gången. #  #  Finns: "Antal aktiva företag", "Antal nyetablerade företag", "Antal nedlagda företag", "Antal anställda", "Antal anställda i nyetablerade företag", "Antal anställda i nedlagda företag"
    tid_koder = "9999",			 # "*" = alla år. Finns från 2023. Max 1 åt gången. 9999 ger senaste år
    gruppera_namn = NA,              # för att skapa egna geografiska indelningar av samtliga regioner som skickas med i uttaget
    diagram_capt = "Källa: Tillväxtanalys. Bearbetning: Samhällsanalys, Region Dalarna.\nEtt företag anses ha varit aktivt om den hade en positiv nettoomsättning, hade anställda eller genomförde investeringar under referensåret.",
    diagram_capt_facet = "Källa: Tillväxtanalys. Bearbetning: Samhällsanalys, Region Dalarna.",
    visa_dataetiketter = FALSE,
    diag_vanlig = TRUE, # Om man vill ha ett enskilt diagram där variabeln väljs enligt cont_klartext ovan
    diag_facet = FALSE, # Om man vill ha ett facet-diagram med "Antal aktiva företag", "Antal nyetablerade företag" och "Antal nedlagda företag"
    diag_fargvekt = NA,
    ta_med_logga = TRUE,
    logga_sokvag = NA,
    output_mapp = NA,
    skriv_diagramfil = FALSE,
    returnera_data_rmarkdown = FALSE,
    demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
) {
  
  # ==============================================================================================================================
  #
  # Skriver ut diagram med diverse olika variabler från Tillväxtanalys
  #  
  # Skapat av Jon Frank 2026-06-15
  # ==============================================================================================================================
  
  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
  if (demo){
    demo_url <- 
      c("https://region-dalarna.github.io/utskrivna_diagram/foradlingsvarde_bransch_20_ar2022.png")
    walk(demo_url, ~browseURL(.x))
    if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
    stop_tyst()
  }
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         glue,
         readxl,
         janitor,
         here)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_aktiva_foretag_mm_tid_lan_storleksklass_tva.R")
  
  "G:/skript/jon/Slask/hamta__tid_lan_bransch_variabel__tva.R"
  # om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger
  if (all(is.na(diag_fargvekt))) {
    if (exists("diagramfarger", mode = "function")) {
      diag_fargvekt <- diagramfarger("rus_sex")
    } else {
      diag_fargvekt <- hue_pal()(9)
    }
  }
  
  if (all(is.na(output_mapp))) {
    if (exists("utskriftsmapp", mode = "function")) {
      output_mapp <- utskriftsmapp()
    } else {
      stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
    }
  }
  
  # Funktion som hanterar storleksklasserna
  
  
  clean_storleksklass <- function(x) {
    x %>%
      str_replace("^0(\\d)", "\\1") %>%           # 01 -> 1, 00 -> 0
      str_replace("([–-])0(\\d)", "\\1\\2") %>%   # 1–09 / 1-09 -> 1–9 / 1-9
      str_replace("\\+$", " eller fler")          # 10+ -> 10 eller fler
  }
  
  
  gg_list <- list()
  
  if(diag_vanlig == TRUE){
    
    # Hämrat data från Tillväxtanalys och tar bort onödig text från branscher
    foretag_df <- hamta_aktiva_foretag_mm_tid_lan_storleksklass_variabel_tva(region_vekt = region_vekt,
                                                                             variabel_klartext = cont_klartext,
                                                                             tid_koder = tid_koder,
                                                                             storleksklass_klartext = "*") %>%
      mutate(storleksklass = clean_storleksklass(storleksklass),
             region = skapa_kortnamn_lan(region)) %>% 
      rename(varde = last_col())
    
    region_txt <- unique(foretag_df$region)
    ar_txt <- unique(foretag_df$år)
    
    variabel_txt <- unique(foretag_df$variabel)
    
    
    diagramtitel <- glue("{variabel_txt} i {region_txt} år {ar_txt}")
    
    safe_name <- make_clean_names(variabel_txt)
    
    if(returnera_data_rmarkdown == TRUE){
      assign(paste0(safe_name,"storleksklass_df"), foretag_df, envir = .GlobalEnv)
    }
    
    diagramfil <- glue("{safe_name}_storleksklass_{region_txt}_ar{ar_txt}.png")
    
    gg_obj <- SkapaStapelDiagram(
      skickad_df = foretag_df,
      skickad_x_var = "storleksklass",
      skickad_y_var = "varde",
      diagram_capt = diagram_capt,
      diagram_titel = diagramtitel,
      x_axis_sort_value = TRUE,
      #diagram_liggande = TRUE,
      stodlinjer_avrunda_fem = TRUE,
      filnamn_diagram = diagramfil,
      dataetiketter = visa_dataetiketter,
      manual_y_axis_title = cont_klartext,
      x_axis_lutning = 0,
      manual_color = diag_fargvekt,
      output_mapp = output_mapp,
      lagg_pa_logga = ta_med_logga,
      logga_path = logga_sokvag,
      skriv_till_diagramfil = skriv_diagramfil
    )
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
    
  }
  
  if(diag_facet == TRUE){
    # Hämrat data från Tillväxtanalys och tar bort onödig text från branscher
    foretag_facet_df <- hamta_aktiva_foretag_mm_tid_lan_storleksklass_variabel_tva(region_vekt = region_vekt,
                                                                                   variabel_klartext = c("Antal aktiva företag", "Antal nyetablerade företag", "Antal nedlagda företag"),
                                                                                   tid_koder = tid_koder,
                                                                                   storleksklass_klartext = "*") %>% 
      mutate(storleksklass = clean_storleksklass(storleksklass),
             region = skapa_kortnamn_lan(region)) %>% 
      rename(varde = last_col()) 
    
    
    region_txt <- unique(foretag_df$region)
    ar_txt <- unique(foretag_df$år)
    
    diagramtitel <- ""
    diagramfil <- glue("antal_foretag_mm_storleksklass_facet_{region_txt}_ar_{ar_txt}.png")
    #safe_name <- make_clean_names(variabel_txt)
    
    if(returnera_data_rmarkdown == TRUE){
      assign("antal_foretag_mm_storleksklass_facet_df", foretag_facet_df, envir = .GlobalEnv)
    }
    
    gg_obj <- SkapaStapelDiagram(
      skickad_df = foretag_facet_df %>% mutate(variabel =  variabel %>% str_replace("^Antal\\s+", "") |> str_to_sentence(locale = "sv")),
      skickad_x_var = "storleksklass",
      skickad_y_var = "varde",
      diagram_capt = diagram_capt_facet,
      diagram_titel = diagramtitel,
      x_axis_sort_value = TRUE,
      diagram_facet = TRUE,
      facet_kolumner = 1,
      facet_grp = "variabel",
      facet_scale = "fixed",
      diagram_liggande = TRUE,
      stodlinjer_avrunda_fem = TRUE,
      filnamn_diagram = diagramfil,
      dataetiketter = visa_dataetiketter,
      manual_y_axis_title =  paste0("Antal i ",region_txt," år ",ar_txt),
      x_axis_lutning = 0,
      manual_color = diag_fargvekt[1],
      output_mapp = output_mapp,
      lagg_pa_logga = ta_med_logga,
      logga_path = logga_sokvag,
      skriv_till_diagramfil = skriv_diagramfil
    )
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
  }
  
  return(gg_list)
  
} # slut diag-funktion

