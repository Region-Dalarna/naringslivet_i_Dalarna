diag_fek_bransch_lan_scb <- function(
    region_vekt = "20",			# Val av region. Finns: "00", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25", "SE0", "SE00", "SE1", "SE11", "SE110", "SE12", "SE121", "SE122", "SE123", "SE124", "SE125", "SE2", "SE21", "SE211", "SE212", "SE213", "SE214", "SE22", "SE221", "SE224", "SE23", "SE231", "SE232", "SE3", "SE31", "SE311", "SE312", "SE313", "SE32", "SE321", "SE322", "SE33", "SE331", "SE332"
    sni2007_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "A-SexklK-O samtliga näringsgrenar (exkl. K+O+T+U)", "A-01-03 jordbruk, skogsbruk och fiske", "B-05-09 utvinning av mineral", "C-10-33 tillverkning", "D-35 försörjning av el, gas, värme och kyla", "E-36-39 vattenförsörjning; avloppsrening, avfallshantering och sanering", "F-41-43 byggverksamhet", "G-45-47 handel; reparation av motorfordon och motorcyklar", "H-49-53 transport och magasinering", "I-55-56 hotell- och restaurangverksamhet", "J-58-63 informations- och kommunikationsverksamhet", "L-68 fastighetsverksamhet", "M-69-75 verksamhet inom juridik, ekonomi, vetenskap och teknik", "N-77-82 uthyrning, fastighetsservice, resetjänster och andra stödtjänster", "P-85 utbildning", "Q-86-88 vård och omsorg; sociala tjänster", "R-90-93 kultur, nöje och fritid", "S-94-96 annan serviceverksamhet"
    cont_klartext = "Förädlingsvärde, mnkr",			 # Max 1 åt gången. Finns: "Antal arbetsställen (lokala verksamheter)", "Antal anställda", "Nettoomsättning exkl. merchantingkostnader, mnkr", "Produktionsvärde, mnkr", "Förädlingsvärde, mnkr", "Totala intäkter, mnkr", "Totala kostnader, mnkr"
    tid_koder = "*",			 # "*" = alla år. Finns från 2022
    gruppera_namn = NA,              # för att skapa egna geografiska indelningar av samtliga regioner som skickas med i uttaget
    diagram_capt = "Källa: Företagens ekonomi i SCB:s öppna statistikdatabas. Bearbetning: Samhällsanalys, Region Dalarna",
    visa_dataetiketter = FALSE,
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
  # Skriver ut diagram med vald variabel från företagens ekonomi per bransch. Finns data från 2022
  #  
  # Utveckling av ett skript som Peter skapat: "diag_fek_foradlingsvarde_bransch_lan_scb.R" /Jon
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
         janitor)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8")
  #source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_fek_lve_region_sni2007_tid_NSEBasfaktaLVEngs07_scb.R")
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_pxweb2.R")
  
  
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
  foradl_df <- pxweb2_hamta_data(
    tabell = "TAB6329",
    query = list(
      Region = region_vekt,
      SNI2007 = sni2007_klartext,
      ContentsCode = cont_klartext,
      Tid = tid_koder
    )) %>% 
    rename(regionkod = region_kod,
           variabel = `tabellinnehåll`,
           varde = value) 

  branschnyckel <- readxl::read_xlsx("g:/skript/nycklar/Bransch_FEK.xlsx") %>% 
    select(Avdelning, Grupp_kod, Branschgrupp) %>% 
    distinct()
  
  chart_df <- foradl_df %>% 
    mutate(branschbokstav = str_sub(`näringsgren SNI 2007`, 1, 1)) %>% 
    filter(!str_detect(`näringsgren SNI 2007`, "samtliga näringsgrenar")) %>% 
    left_join(branschnyckel, by = c("branschbokstav" = "Avdelning")) %>% 
    group_by(år, regionkod, region, Grupp_kod, Branschgrupp, variabel) %>% 
    summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop")
  
  # om man vill gruppera ihop flera kommuner eller län till en större geografisk indelning
  # så anges den med namn i gruppera_namn. Lämnas den tom görs ingenting nedan
  if (!is.na(gruppera_namn)) {
    chart_df <- chart_df %>% 
      group_by(across(-c(regionkod, region, varde))) %>% 
      summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop") %>% 
      mutate(regionkod = "gg",
             region = gruppera_namn) %>% 
      relocate(region, .before = 1) %>% 
      relocate(regionkod, .before = region)
    
    region_vekt <- "gg"
  }
  
  
  
  vald_region_txt <- chart_df %>% 
    distinct(region) %>% 
    dplyr::pull() %>%
    list_komma_och() %>% 
    skapa_kortnamn_lan()
  
  ar_txt <- chart_df %>% 
    distinct(år) %>% 
    dplyr::pull() %>%
    list_komma_och()
  
  # Skapar ett variabelnamn för att lägga till i diagrammets rubrik
  variabel_txt <- str_remove(unique(foradl_df$variabel), "( exkl\\..*|,.*)")
  # Skapar ett "safe" variabelnamn som kan användas i filnamnet, där alla specialtecken och mellanslag tas bort eller ersätts
  safe_name <- make_clean_names(variabel_txt)
  
  if(returnera_data_rmarkdown == TRUE){
    assign(paste0(safe_name,"_df"), chart_df, envir = .GlobalEnv)
  }
  
  if(length(unique(chart_df$år)) < 2){
    diagramtitel <- glue("{variabel_txt} i {vald_region_txt} per bransch år {ar_txt}")
  } else {
    diagramtitel <- glue("{variabel_txt} i {vald_region_txt} per bransch")
  }
  
  if(length(unique(chart_df$år)) < 2){
    diagramfil <- glue("{safe_name}_bransch_{region_vekt %>% paste0(collapse = '_')}_ar_{first(unique(chart_df$år))}_{last(unique(chart_df$år))}.png")
  } else {
    diagramfil <- glue("{safe_name}_bransch_{region_vekt %>% paste0(collapse = '_')}_ar{ar_txt}.png")
  }
  
  
  
  gg_obj <- SkapaStapelDiagram(
    skickad_df = chart_df,
    skickad_x_var = "Branschgrupp",
    skickad_y_var = "varde",
    skickad_x_grupp = if (length(unique(chart_df$år)) > 1) "år" else NULL,
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
  
  return(gg_list)
  
} # slut diag-funktion

