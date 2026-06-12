diag_utlandskt_agande_bransch_land <- function(
    region_vekt = "20",			# Val av region. Finns: "00", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25", "SE0", "SE00", "SE1", "SE11", "SE110", "SE12", "SE121", "SE122", "SE123", "SE124", "SE125", "SE2", "SE21", "SE211", "SE212", "SE213", "SE214", "SE22", "SE221", "SE224", "SE23", "SE231", "SE232", "SE3", "SE31", "SE311", "SE312", "SE313", "SE32", "SE321", "SE322", "SE33", "SE331", "SE332"
    cont_klartext = "Antal anställda",			 # Max 1 åt gången. #  Finns: "Antal arbetsställen", "Antal anställda"
    tid_koder = "9999",			 # "*" = alla år. Finns från 2022. Max 1 åt gången
    gruppera_namn = NA,              # för att skapa egna geografiska indelningar av samtliga regioner som skickas med i uttaget
    diagram_capt = "Källa: Tillväxtanalys. Bearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Ett arbetsställe definieras som utländskt om mer än hälften av aktiernas röstvärde innehas av utländska ägare.",
    visa_dataetiketter = FALSE,
    diag_fargvekt = NA,
    diag_bransch = TRUE, # Uppdelning på bransch
    diag_land = TRUE, # Uppdelning på ägarland
    ta_med_logga = TRUE,
    logga_sokvag = NA,
    output_mapp = NA,
    skriv_diagramfil = FALSE,
    returnera_data_rmarkdown = FALSE,
    demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
) {
  
  # ==============================================================================================================================
  #
  # Skriver ut diagram med vald variabel från företagens ekonomi per bransch. Finns data från 2022
  #  
  # 
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
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_utlandskt_agande_tid_agarland_lan_bransch_tva.R")
  
  
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
  
  gg_list <- list()
  
  # Hämrat data från företagens ekonomi
  agande_df <- hamta_utlandskt_agande_tid_agarland_lan_bransch_tva(tid_koder = tid_koder,
                                                                   agarland_vekt = "*",
                                                                   region_vekt = region_vekt,
                                                                   variabel_klartext = cont_klartext) %>% 
    rename(varde = last_col()) %>% 
      mutate(region = skapa_kortnamn_lan(region))
  
  region_txt <- unique(agande_df$region)
  ar_txt <- unique(agande_df$år)
  
  if(diag_bransch == TRUE){
    
    branschnyckel <- readxl::read_xlsx("g:/skript/nycklar/Bransch_FEK.xlsx") %>% 
      select(Avdelning, Grupp_kod, Branschgrupp) %>% 
      distinct()
    
    bransch_df <- agande_df %>% 
      filter(!str_detect(bransch, "ospecificerad")) %>%
        mutate(branschbokstav = str_sub(bransch, 1, 1)) %>% 
            left_join(branschnyckel, by = c("branschbokstav" = "Avdelning")) %>% 
              group_by(år, region, Branschgrupp, variabel) %>% 
                summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop") %>% 
                  filter(!(is.na(Branschgrupp)))
    
    variabel_txt <- unique(bransch_df$variabel)
    
    
    diagramtitel <- glue("{variabel_txt} i utlandsägda företag i {region_txt} år {ar_txt}")
    
    safe_name <- make_clean_names(variabel_txt)
    
    if(returnera_data_rmarkdown == TRUE){
      assign(paste0(safe_name,"_utlandskt_agande_bransch_df"), bransch_df, envir = .GlobalEnv)
    }
  
    diagramfil <- glue("{safe_name}_bransch_{region_txt}_ar{ar_txt}.png")
    
    gg_obj <- SkapaStapelDiagram(
      skickad_df = bransch_df,
      skickad_x_var = "Branschgrupp",
      skickad_y_var = "varde",
      diagram_capt = diagram_capt,
      diagram_titel = diagramtitel,
      x_axis_sort_value = TRUE,
      stodlinjer_avrunda_fem = TRUE,
      filnamn_diagram = diagramfil,
      dataetiketter = visa_dataetiketter,
      manual_y_axis_title = cont_klartext,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      x_axis_lutning = 45,
      manual_color = diag_fargvekt,
      output_mapp = output_mapp,
      lagg_pa_logga = ta_med_logga,
      logga_path = logga_sokvag,
      skriv_till_diagramfil = skriv_diagramfil
    )
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
    
  }
  
  if(diag_bransch == TRUE){
    land_nyckel <- readxl::read_xlsx(here("indata","landkoder.xlsx")) %>%
      distinct()
    
    agarland_df <- agande_df %>% 
      left_join(land_nyckel, by = c("ägarland" = "Landkod")) %>% 
            group_by(år, region, Land, variabel) %>% 
              summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop") %>% 
                group_by(år) %>%
                  slice_max(order_by = varde, n = 15, with_ties = FALSE) %>%
                    ungroup()
    
    safe_name <- make_clean_names(variabel_txt)
    
    if(returnera_data_rmarkdown == TRUE){
      assign(paste0(safe_name,"_utlandskt_agande_land_df"), agarland_df, envir = .GlobalEnv)
    }
    
    diagramtitel <- glue("{variabel_txt} i utlandsägda företag i {region_txt} per ägarland år {ar_txt}")
    diagramfil <- glue("{safe_name}_agarland_{region_txt}_ar{ar_txt}.png")
    diagram_capt = paste0(diagram_capt, "\nI diagrammet syns de 15 största ägarländerna.")
    
    gg_obj <- SkapaStapelDiagram(
      skickad_df = agarland_df,
      skickad_x_var = "Land",
      skickad_y_var = "varde",
      diagram_capt = diagram_capt,
      diagram_titel = diagramtitel,
      x_axis_sort_value = TRUE,
      stodlinjer_avrunda_fem = TRUE,
      filnamn_diagram = diagramfil,
      dataetiketter = visa_dataetiketter,
      manual_y_axis_title = cont_klartext,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      x_axis_lutning = 45,
      manual_color = diag_fargvekt,
      output_mapp = output_mapp,
      lagg_pa_logga = ta_med_logga,
      logga_path = logga_sokvag,
      skriv_till_diagramfil = skriv_diagramfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
    
  }

  return(gg_list)
  
} # slut diag-funktion

