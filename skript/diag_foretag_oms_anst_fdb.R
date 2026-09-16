diag_fdb_omsattning_mm <- function(region_vekt = "20", # Enbart län för tillfället
                                   diagram_capt = "Källa: Företagsdatabasen (FDB), SCB. Bearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Företag som har säte i länet, är registrerade för F-skatt, är verksamma och har en omsättning.",
                                   output_mapp = "G:/Samhällsanalys/API/Fran_R/Utskrift/",
                                   returnera_data_rmarkdown == TRUE,
                                   visa_dataetiketter = TRUE,
                                   skriv_diagramfil = FALSE) {
  
  #############################################################################################
  ### 
  ### Två diagram för antal företag uppdelat på omsättning respektive antal anställda 
  ###
  ###
  #############################################################################################

  
    # Bara paket, ingen source() mot funktioner-/hamta_data-reporna och inget
    # p_load(tidyverse). Anropas med fullt namespace (dplyr::filter() osv.) i
    # stället för library().
    if (!requireNamespace("rddiagram", quietly = TRUE)) {
      remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
    }
    if (!requireNamespace("rdverktyg", quietly = TRUE)) {
      remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
    }
    if (!requireNamespace("rdpostgres", quietly = TRUE)) {
      remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdpostgres")
    }
    if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
    if (!requireNamespace("glue", quietly = TRUE)) install.packages("glue")
    
    vald_region <- skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region)
    
    gg_list <- list()
    
    # Hämtar data från SCB (företagsdatabasen) via vår databas
    foretag_df <- rdpostgres::oppnadata_hamta("scb","foretag")|> 
      dplyr::filter(substr(`säteskommun, kod`,1,2) == region_vekt,
                   `fskattstatus, kod`== 1,
                   `företagsstatus, kod` == 1 ) 
  
    
    # Olika omsättningsgrupper
    group_omsattning <- function(x) {
      case_when(
        x %in% c("< 1 tkr", "1 - 499 tkr") ~ "< 500 tkr",
        x %in% c("500 - 999 tkr", "1 000 - 4 999 tkr") ~ "500 - 4 999 tkr",
        x %in% c("5 000 - 9 999 tkr", "10 000 - 19 999 tkr") ~ "5 000 - 19 999 tkr",
        x %in% c("20 000 - 49 999 tkr", "50 000 - 99 999 tkr") ~ "20 000 - 99 999 tkr",
        x %in% c("100 000 - 499 999 tkr", "500 000 - 999 999 tkr") ~ "100 000 - 999 999 tkr",
        x %in% c("1 000 000 - 4 999 999 tkr", "5 000 000 - 9 999 999 tkr", " > 9 999 999 tkr") ~ "≥ 1 000 000 tkr",
        TRUE ~ NA_character_
      )
    }
    
    # Så att ordningen blir rätt i diagrammet
    group_order <- c("< 500 tkr", "500 - 4 999 tkr", "5 000 - 19 999 tkr", 
                     "20 000 - 99 999 tkr","100 000 - 999 999 tkr", "≥ 1 000 000 tkr")
    
    # 
    foretag_oms_ranking_df <- foretag_df |>
      filter(`storleksklass, oms` != "") |>
        mutate(omsattning_grupp = group_omsattning(`storleksklass, oms`)) |> 
          mutate(omsattning_grupp = factor(omsattning_grupp, levels = group_order, ordered = TRUE)) |> 
            count(omsattning_grupp)
    
    if(returnera_data_rmarkdown == TRUE){
      assign(paste0(safe_name,"foretag_oms_ranking_df"), foretag_oms_ranking_df, envir = .GlobalEnv)
    }
  
      
      diagramtitel <- glue("Antal företag i {vald_region} uppdelat på omsättning")
      diagramfil <- glue("antal_foretag_omsattning_SCB_{vald_region}.png")
  
      gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = foretag_oms_ranking_df,
                               skickad_x_var = "omsattning_grupp",
                               skickad_y_var = "n",
                               diagram_titel = diagramtitel,
                               diagram_capt = diagram_capt,
                               stodlinjer_avrunda_fem = TRUE,
                               filnamn_diagram = diagramfil,
                               dataetiketter = visa_dataetiketter,
                               manual_y_axis_title = "Antal företag",
                               manual_x_axis_title = "Omsättning",
                               manual_x_axis_text_vjust = 1,
                               manual_x_axis_text_hjust = 1,
                               manual_color = rddiagram::diagramfarger("rus_sex"),
                               output_mapp = output_mapp,
                               skriv_till_diagramfil = skriv_diagramfil)
      
      gg_list <- c(gg_list, list(gg_obj))
      names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
   
  
      group_anstallda <- function(x) {
        case_when(
          x == "0 anställda" ~ "0",
          x %in% c("1-4 anställda", "5-9 anställda") ~ "1-9",
          x %in% c("10-19 anställda", "20-49 anställda") ~ "10-49",
          x %in% c("50-99 anställda", "100-199 anställda") ~ "50-249",
          x %in% c("200-499 anställda") ~ "250-499",
          x %in% c("500-999 anställda", "1000-1499 anställda", "1500-1999 anställda", 
                   "2000-2999 anställda", "4000-4999 anställda", "5000-9999 anställda", 
                   "10000- anställda") ~ "500-",
          TRUE ~ NA_character_
        )
      }
  
      anstallda_order <- c("0", "1-9", "10-49", "50-249", "250-499", "500-")
  
  foretag_anst_ranking_df <- foretag_df |>
    filter(`storleksklass, oms` != "") |>
      mutate(anstallda_grupp = group_anstallda(storleksklass)) |> 
        mutate(anstallda_grupp = factor(anstallda_grupp, levels = anstallda_order, ordered = TRUE)) |> 
          count(anstallda_grupp)
  
  if(returnera_data_rmarkdown == TRUE){
    assign(paste0(safe_name,"foretag_anst_ranking_df"), foretag_anst_ranking_df, envir = .GlobalEnv)
  }
    
  diagramtitel <- glue("Antal företag i {vald_region} per företagsstorlek")
  diagramfil <- glue("antal_foretag_anstallda_SCB_{vald_region}.png")
  
  gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = foretag_anst_ranking_df,
                                          skickad_x_var = "anstallda_grupp",
                                          skickad_y_var = "n",
                                          diagram_titel = diagramtitel,
                                          diagram_capt = diagram_capt,
                                          stodlinjer_avrunda_fem = TRUE,
                                          filnamn_diagram = diagramfil,
                                          dataetiketter = visa_dataetiketter,
                                          manual_y_axis_title = "Antal företag",
                                          manual_x_axis_title = "Antal anställda",
                                          x_axis_lutning = 0,
                                          manual_color = rddiagram::diagramfarger("rus_sex"),
                                          output_mapp = output_mapp,
                                          skriv_till_diagramfil = skriv_diagramfil)
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
  
  return(gg_list)
  
} # slut diag-funktion
