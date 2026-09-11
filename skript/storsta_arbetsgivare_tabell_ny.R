# Största arbetsgivare i Dalarna - både privat och offentlig sektor. Underliggande data behöver uppdateras manuellt från www.foretagsklimat.se
# För tillfället finns dock inte största arbetsgivare på deras hemsida
# Senast uppdaterad (data): 20260604 

diag_storsta_arbetsgivare <- function(region_vekt = "20",
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
  
  # ========================================== Info =====================================================
  # Största arbetsgivare. Data från företagsklimat (Svenskt näringsliv). Finns inte längre på hemsida,
  # utan fått Excelfiler per mail från www.foretagsklimat.se
  
  # ========================================== Läser in data ============================================
  input_mapp <- "G:/Samhällsanalys/Statistik/Företagsstatistik/Största arbetsgivare/www_foretagsklimat_se/mail_fran_foretagsklimat/"
  
  # Största privata arbetsgivare
  files <- list.files(input_mapp, pattern = "*samtliga", full.names = TRUE)
  
  file_info <- file.info(files)
  latest_file <- rownames(file_info)[which.max(file_info$mtime)]
  
  år_extracted <- str_extract(latest_file, "\\d{4}")
  
  flikar <- c("Län", "Kommun")
  
  data_list <- list()
  
  for (sheet in flikar) {
    df <- readxl::read_xlsx(latest_file, sheet = sheet, skip = 3) |> 
      select(1:3,5) |> 
        rename(antal_anstallda = RedigeratAntalAnställda)
    
    names(df)[1] <- "regionkod"
    names(df)[2] <- "region_namn"
    
    # Delar upp i offentlig och privat verksamhet
    df <- df |>
      mutate(
        sektor = if_else(
          str_detect(FöretagsNamn, regex("region|kommun|församling", ignore_case = TRUE)),
          "Offentlig",
          "Privat"
        ),
        år = år_extracted
      )
    # Fixar stora bokstäver för kommun och region
    df <- df |>
      mutate(
        FöretagsNamn = case_when(
          str_detect(FöretagsNamn, "^REGION ") ~ str_to_title(FöretagsNamn),
          str_detect(FöretagsNamn, " KOMMUN$") ~ str_to_title(FöretagsNamn),
          TRUE ~ FöretagsNamn
        )
      )
    
    data_list[[sheet]] <- df
  }
  
  arbgiv_alla <- bind_rows(data_list, .id = "niva")
  rm(df,data_list)
  
  # Step 1: get the single biggest employer per kommun, per sector
  arbgiv_max <- arbgiv_alla |>
    group_by(region_namn, sektor) |>
    slice_max(antal_anstallda, n = 1, with_ties = FALSE) |>
    ungroup()
  
  # Step 2: split into private and public, renaming to match your gt() code
  privat <- arbgiv_max |>
    filter(sektor == "Privat") |>
    select(regionkod,
      Kommun = region_namn,
      `Arbetsgivare (privat)` = FöretagsNamn,
      `Antal anställda (privat)` = antal_anstallda,
      år
    )
  
  offentlig <- arbgiv_max |>
    filter(sektor == "Offentlig") |>
    select(regionkod,
      Kommun = region_namn,
      `Arbetsgivare (offentlig)` = FöretagsNamn,
      `Antal anställda (offentlig)` = antal_anstallda
    )
  
  # Step 3: join into the final table-ready df
  arbgiv <- left_join(privat, offentlig, by = c("regionkod","Kommun")) |> 
    relocate(år,.before = Kommun) |> 
     filter(substr(regionkod,1,2)=="20")
  
  vald_region = arbgiv |> filter(nchar(regionkod)==2) |> dplyr::pull(Kommun)
 
  if(returnera_data == TRUE){
    assign("storsta_arbetsgivare_df", arbgiv, envir = .GlobalEnv)
  }

  gg_list <- lst()
  
  tabell <-  arbgiv %>%
    select(Kommun,
           `Arbetsgivare (privat)`,
           `Antal anställda (privat)`,
           `Arbetsgivare (offentlig)`,
           `Antal anställda (offentlig)`) %>%
    gt() %>%
    tab_header(
      title = paste0("Största arbetsgivare i ",vald_region," " ,unique(arbgiv$år))
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
        rows = Kommun == vald_region
      )
    ) %>% 
    tab_source_note(
      source_note = md(paste0(
        "Källa: [www.foretagsklimat.se](https://www.foretagsklimat.se), bearbetning: Samhällsanalys, Region Dalarna<br>",
        "Notera att flera arbetsgivare kan ha lika många antal anställda, men bara ett syns i tabellen."
      ))
    )
  gg_list <- c(gg_list, list(tabell))
  
  names(gg_list) <- "storsta_arbetsgivare"  
  return(gg_list)
  
}
