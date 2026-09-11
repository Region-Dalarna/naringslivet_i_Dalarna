# Största arbetsgivare i Dalarna - både privat och offentlig sektor. Underliggande data behöver uppdateras manuellt från www.foretagsklimat.se
# För tillfället finns dock inte största arbetsgivare på deras hemsida
# Senast uppdaterad (data): 20260604 

diag_storsta_arbetsgivare <- function(region_vekt = hamtakommuner("20",tamedriket = FALSE),
                                      returnera_data = TRUE # Skall data returneras till R-studios global environment
                                      ){
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(openxlsx,
                 here,
                 tidyverse,
                 gt,
                 webshot2)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_filer.R", encoding = "utf-8", echo = FALSE)
  options(dplyr.summarise.inform = FALSE)
 
  # ========================================== Läser in data ============================================
  valda_kommuner_txt <- region_vekt %>%    # gör om kommunkoderna till kommunnamn
    hamtaregion_kod_namn() %>% 
    select(region) %>% 
    dplyr::pull()
  
  input_mapp <- "G:/Samhällsanalys/Statistik/Företagsstatistik/Största arbetsgivare/www_foretagsklimat_se/"
  
  # Största privata arbetsgivare
  files <- list.files(input_mapp, pattern = "*Största privata arbetsgivare", full.names = TRUE)
  
  file_info <- file.info(files)
  latest_file <- rownames(file_info)[which.max(file_info$mtime)]
  
  arbgiv_privat <- readxl::read_xlsx(latest_file, skip = 3) %>% 
    rename(`Arbetsgivare (privat)` = `Delserie 1`,                                   # döp om två kolumner till vettigare namn
           Kategori = `Delserie 2`) %>%
    filter(Kategori == "Antal anställda",                                 # ta bara med variablen Antal anställda
           Kommun %in% valda_kommuner_txt) %>%                            # filtrera på  kommuner som vi valt
    mutate(`Arbetsgivare (privat)` = str_to_title(`Arbetsgivare (privat)`)) %>%
    mutate(
      `Arbetsgivare (privat)` = str_remove(
        `Arbetsgivare (privat)`,
        regex("\\s(AB|AKTIEBOLAG).*", ignore_case = TRUE)
      )
    ) %>% 
    pivot_longer(-c(Kommun, `Arbetsgivare (privat)`, Kategori), names_to = "år", values_to = "antal anställda") %>% 
    filter(år == max(år),
           !(is.na(`antal anställda`))) %>% 
    group_by(Kommun, år) %>%
    slice_max(order_by = `antal anställda`, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  # Största offentliga arbetgivare
  files <- list.files(input_mapp, pattern = "*Största arbetsgivare", full.names = TRUE)
  
  file_info <- file.info(files)
  latest_file <- rownames(file_info)[which.max(file_info$mtime)]
  
  arbgiv_offentlig <- readxl::read_xlsx(latest_file, skip = 3) %>% 
    rename(`Arbetsgivare (offentlig)` = `Delserie 1`,                                   # döp om två kolumner till vettigare namn
           Kategori = `Delserie 2`) %>%
    filter(Kategori == "Antal anställda",                                 # ta bara med variablen Antal anställda
           Kommun %in% valda_kommuner_txt,
           grepl("kommun|region", `Arbetsgivare (offentlig)`, ignore.case = TRUE)) %>%                            # filtrera på  kommuner som vi valt
    mutate(`Arbetsgivare (offentlig)` = str_to_title(`Arbetsgivare (offentlig)`)) %>%
    pivot_longer(-c(Kommun, `Arbetsgivare (offentlig)`, Kategori), names_to = "år", values_to = "antal anställda") %>% 
    filter(år == max(år),
           !(is.na(`antal anställda`))) %>% 
    group_by(Kommun, år) %>%
    slice_max(order_by = `antal anställda`, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  # Slår ihop 
  arbgiv <- arbgiv_privat %>% 
    left_join(arbgiv_offentlig, by = c("Kommun", "år")) %>% 
    select(år,Kommun, `Arbetsgivare (privat)`, `Antal anställda (privat)` = `antal anställda.x`, `Arbetsgivare (offentlig)`, `Antal anställda (offentlig)` = `antal anställda.y`)
  
  if(returnera_data == TRUE){
    assign("storsta_arbetsgivare_df", arbgiv, envir = .GlobalEnv)
  }
  
  # 
  # if(returnera_data == TRUE){
  #   assign("bransch_kommun", bransch_kommun_df, envir = .GlobalEnv)
  # }
  
  gg_list <- lst()

  tabell <-  arbgiv %>%
    select(Kommun,
           `Arbetsgivare (privat)`,
           `Antal anställda (privat)`,
           `Arbetsgivare (offentlig)`,
           `Antal anställda (offentlig)`) %>%
    gt() %>%
    tab_header(
      title = paste0("Största arbetsgivare i Dalarna ", unique(arbgiv$år))
    ) %>%
    cols_label(
      `Antal anställda (privat)` = "Antal anställda",
      `Antal anställda (offentlig)` = "Antal anställda"
    ) %>%
    cols_align(align = "center", columns = c(3, 5)) %>%
    cols_width(
      Kommun ~ px(120),                     # makes first column wider
      `Arbetsgivare (privat)` ~ px(100),     # pushes column 2 further right
      `Antal anställda (privat)` ~ px(100), # sets fixed width for column 3
      `Arbetsgivare (offentlig)` ~ px(100),   # pushes column 4 further right
      `Antal anställda (offentlig)` ~ px(100) # sets fixed width for column 5
    ) %>%
    tab_options(
      heading.padding = 20,
      heading.background.color = diagramfarger("rus_sex")[5],
      table.border.bottom.color = "transparent",
      table.font.size = px(10L)
    ) %>%
    tab_style(
      locations = cells_column_labels(columns = everything()),
      style = list(
        cell_borders(sides = c("top","bottom"), weight = px(3)),
        cell_text(weight = "bold")
      )
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = Kommun == "Dalarnas län"
      )
    ) %>% 
    tab_source_note(
      source_note = "Källa: www.foretagsklimat.se, bearbetning: Samhällsanalys, Region Dalarna"
    )
  gg_list <- c(gg_list, list(tabell))

  names(gg_list) <- "storsta_arbetsgivare"  
  return(gg_list)
  
}
