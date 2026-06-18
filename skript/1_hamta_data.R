# Skript som hämtar data och skapar figurer/variabler som används för att skapa markdown-rapporten. ctrl+A följt av ctrl+enter för att köra skriptet

#Det finns två alternativ för skriptet:

# 1: Kör skriptet utan att uppdatera data - sätt variabeln uppdatera_data till FALSE. Då läses den senast sparade versionen av R-studio global environment in.
# Detta är ett bra alternativ om man enbart vill ändra text eller liknande, men inte uppdatera data.

# 2: Uppdatera data - sätt variabeln uppdatera_data till TRUE. Då uppdateras data, alla figurer skapas på nytt och en ny enviroment sparas.
# Tar längre tid (ett par minuter) och medför en risk att text inte längre är aktuell då figurer har uppdaterats med nya data.

hoppa_over_felhantering = FALSE

uppdatera_data = TRUE
spara_figurer = FALSE

if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,
       here,
       glue,
       scales)

mapp_environment_fil = "g:/skript/projekt/environments/" # OBS: Får ej ändras
repo_namn = "naringslivet_i_Dalarna" # OBS: Får ej ändras

source("G:/skript/jon/Funktioner/func_markdown.R")

# Funktion som automatiskt väljer variabel med högst respektive lägst värde



# library(dplyr)
# library(scales)



# get_extremes_by_year <- function(data, value_var, region_var, year_var,
#                                  specific_region = NA_character_,
#                                  accuracy = 0.1,
#                                  decimal_mark = ",") {
#   
#   formatter <- label_number(
#     accuracy = accuracy,
#     decimal.mark = decimal_mark
#   )
#   
#   year_name <- rlang::as_label(enquo(year_var))
#   
#   highest <- data %>%
#     group_by({{ year_var }}) %>%
#     slice_max({{ value_var }}, n = 1, with_ties = FALSE) %>%
#     ungroup() %>%
#     transmute(
#       {{ year_var }},
#       highest_grupp = {{ region_var }},
#       highest_value = {{ value_var }}
#     )
#   
#   lowest <- data %>%
#     group_by({{ year_var }}) %>%
#     slice_min({{ value_var }}, n = 1, with_ties = FALSE) %>%
#     ungroup() %>%
#     transmute(
#       {{ year_var }},
#       lowest_grupp = {{ region_var }},
#       lowest_value = {{ value_var }}
#     )
#   
#   result <- highest %>%
#     left_join(lowest, by = year_name) %>%
#     mutate(
#       highest_value_num = highest_value,
#       lowest_value_num  = lowest_value,
#       highest_value = formatter(highest_value),
#       lowest_value  = formatter(lowest_value)
#     )
#   
#   if (!is.na(specific_region)) {
#     
#     specific <- data %>%
#       filter({{ region_var }} == specific_region) %>%
#       group_by({{ year_var }}) %>%
#       slice_max({{ value_var }}, n = 1, with_ties = FALSE) %>%
#       ungroup() %>%
#       transmute(
#         {{ year_var }},
#         specific_grupp = {{ region_var }},
#         specific_value = {{ value_var }}
#       )
#     
#     result <- result %>%
#       left_join(specific, by = year_name) %>%
#       mutate(
#         specific_value_num = specific_value,
#         specific_value = formatter(specific_value)
#       )
#   }
#   
#   result
# }


if(uppdatera_data == TRUE){
  
  output_mapp_figur = here("figurer","/")
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  
  cat("Hämtning av data påbörjad\n")
  start_time <- Sys.time()
  
  # Förvärvsarbetande från 1990 till senaste år. Både antal och förändring (från första till sista)
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_forvarvsarbetande_90_senastear_SCB.R")
  gg_forv_90 <- funktion_upprepa_forsok_om_fel( function() {
    diagram_forvarvsarbetande_90(output_mapp_figur = output_mapp_figur,
                                 spara_figur = spara_figurer,
                                 diag_antal = TRUE,
                                 diag_forandring = TRUE,
                                 returnera_figur = TRUE,
                                 returnera_data = TRUE,
                                 vald_farg = diagramfarger("rus_sex"))
  }, hoppa_over = hoppa_over_felhantering)
  
  storsta_bransch_varde_senaste_ar <- get_extremes_by_year(forvarvsarbetande_90_senastear, antal, Näringsgren, år,accuracy = 1) %>% 
    slice_max(år, n = 1)
  
  storsta_bransch_varde_forsta_ar <- get_extremes_by_year(forvarvsarbetande_90_senastear, antal, Näringsgren, år,accuracy = 1) %>% 
    slice_min(år, n = 1)
  
  bransch_storsta_forandring_varde <- get_extremes_by_year(forvarvsarbetande_90_forandring, skillnad, Näringsgren, år,accuracy = 1) %>% 
    slice_max(år, n = 1)
  
  
  # Antal aktiva, nystartade och nedlagda företag uppdelat på bransch (Tillväxtanalys)
  source(here("skript","diag_aktiva_foretag_mm_tva.R"), encoding="UTF-8")
  gg_aktiva_foretag <- funktion_upprepa_forsok_om_fel( function() {diag_aktiva_foretag_bransch_lan(cont_klartext = "Antal aktiva företag",
                                                                                                   skriv_diagramfil = spara_figurer,
                                                                                                   output_mapp = output_mapp_figur,
                                                                                                   diag_facet = TRUE,
                                                                                                   returnera_data_rmarkdown = TRUE)
  }, hoppa_over = hoppa_over_felhantering)
  
  antal_aktiva_foretag_bransch_varde <- get_extremes_by_year(antal_aktiva_foretag_df, varde, bransch, år,accuracy = 1) %>% 
    slice_max(år, n = 1)
  
  gg_nyetablerade_foretag <- funktion_upprepa_forsok_om_fel( function() {diag_aktiva_foretag_bransch_lan(cont_klartext = "Antal nyetablerade företag",
                                                                                                    skriv_diagramfil = spara_figurer,
                                                                                                    diagram_capt = "Källa: Tillväxtanalys. Bearbetning: Samhällsanalys, Region Dalarna.\nMed nyetablerade företag menas de företag som är helt nybildade eller har återupptagits efter att ha varit vilande i minst två år.",
                                                                                                    output_mapp = output_mapp_figur,
                                                                                                    returnera_data_rmarkdown = TRUE)
  }, hoppa_over = hoppa_over_felhantering)
  
  antal_nyetablerade_foretag_bransch_varde <- get_extremes_by_year(antal_nyetablerade_foretag_df, varde, bransch, år,accuracy = 1) %>% 
    slice_max(år, n = 1)

  gg_nedlagda_foretag <- funktion_upprepa_forsok_om_fel( function() {diag_aktiva_foretag_bransch_lan(cont_klartext = "Antal nedlagda företag",
                                                                                                         skriv_diagramfil = spara_figurer,
                                                                                                         diagram_capt = "Källa: Tillväxtanalys. Bearbetning: Samhällsanalys, Region Dalarna.",
                                                                                                         output_mapp = output_mapp_figur,
                                                                                                         returnera_data_rmarkdown = TRUE)
  }, hoppa_over = hoppa_over_felhantering)
  
  antal_nedlagda_foretag_bransch_varde <- get_extremes_by_year(antal_nedlagda_foretag_df, varde, bransch, år,accuracy = 1) %>%
    slice_max(år, n = 1)

  # Motsvarande fast uppdelat på storleksklass istället för bransch
  source(here("skript","diag_aktiva_foretag_mm_storleksklass_tva.R"), encoding="UTF-8")
  gg_aktiva_foretag_storleksklass <- funktion_upprepa_forsok_om_fel( function() {diag_aktiva_foretag_storleksklass_lan(cont_klartext = "Antal aktiva företag",
                                                                                                   skriv_diagramfil = spara_figurer,
                                                                                                   output_mapp = output_mapp_figur,
                                                                                                   diag_facet = FALSE,
                                                                                                   #visa_dataetiketter = TRUE,
                                                                                                   returnera_data_rmarkdown = TRUE)
  }, hoppa_over = hoppa_over_felhantering)
  
  antal_aktiva_foretag_storleksklass_varde <- get_extremes_by_year(antal_aktiva_foretagstorleksklass_df, varde, storleksklass, år,accuracy = 1) %>% 
    slice_max(år, n = 1)
  
  # Förädlingsvärde
  source(here("skript","diag_fek_bransch_fran_2022_korrekt.R"), encoding="UTF-8")
  gg_foradlingsvarde_bransch <- funktion_upprepa_forsok_om_fel( function() {diag_fek_bransch_lan_scb(skriv_diagramfil = spara_figurer,
                                                                                                     output_mapp = output_mapp_figur,
                                                                                                     returnera_data_rmarkdown = TRUE)
  }, hoppa_over = hoppa_over_felhantering)
  
  foradlingsvarde_varde <- get_extremes_by_year(foradlingsvarde_df, varde, Branschgrupp, år,accuracy = 1) %>% 
    slice_max(år, n = 1)
  
  # Nettoomsättning
  source(here("skript","diag_fek_bransch_fran_2022_korrekt.R"), encoding="UTF-8")
  gg_nettoomsattning_bransch <- funktion_upprepa_forsok_om_fel( function() {diag_fek_bransch_lan_scb(cont_klartext = "Nettoomsättning exkl. merchantingkostnader, mnkr",
                                                                                                     skriv_diagramfil = spara_figurer,
                                                                                                     output_mapp = output_mapp_figur,
                                                                                                     returnera_data_rmarkdown = TRUE)
  }, hoppa_over = hoppa_over_felhantering)
  
  nettoomsattning_varde <- get_extremes_by_year(nettoomsattning_df, varde, Branschgrupp, år,accuracy = 1) %>% 
    slice_max(år, n = 1)
  
  source(here("skript","diag_fek_bransch_fran_2022_korrekt.R"), encoding="UTF-8")
  gg_antal_anstallda_bransch <- funktion_upprepa_forsok_om_fel( function() {diag_fek_bransch_lan_scb(cont_klartext = "Antal anställda",
                                                                                                     skriv_diagramfil = spara_figurer,
                                                                                                     output_mapp = output_mapp_figur,
                                                                                                     returnera_data_rmarkdown = TRUE)
  }, hoppa_over = hoppa_over_felhantering)
  
  antal_anstallda_varde <- get_extremes_by_year(antal_anstallda_df, varde, Branschgrupp, år,accuracy = 1) %>% 
    slice_max(år, n = 1)
  
  # Produktivitet (BRP/Sysselsatt) - 2 diagram
  source("https://raw.githubusercontent.com/Region-Dalarna/uppfoljning_dalastrategin/refs/heads/main/Skript/diagram_BRP.R")
  gg_brp_per_sysselsatt <- funktion_upprepa_forsok_om_fel( function() {diagram_brp_sysselsatt(region_vekt = "20",
                                                                                              output_mapp = output_mapp_figur,
                                                                                              returnera_data = TRUE,
                                                                                              ggobjektfilnamn_utan_tid = TRUE,
                                                                                              spara_figur = spara_figurer)
  }, hoppa_over = hoppa_over_felhantering)
  
  # Tillväxt
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_brp_per_inv_scb.R")
  gg_brp_invanare <- funktion_upprepa_forsok_om_fel( function() { diag_brp_per_inv_scb(region_vekt = "20",
                                                                                       output_mapp = output_mapp_figur,
                                                                                       returnera_data_rmarkdown  = TRUE,
                                                                                       skriv_diagramfil = spara_figurer)
  }, hoppa_over = hoppa_over_felhantering)
  
  brp_per_invanare_varde <- get_extremes_by_year(data = brp_lan_df, value_var =  `BRP per invånare, löpande priser, tkr`, region_var = region, year_var = år,accuracy = 1,specific_region = "Dalarna",) %>% 
    slice_max(år, n = 1) 
 
  # Utländskt ägande - antal anställda
  source(here("skript","diag_utlanskt_agande_anstallda_arbetsstallen.R"), encoding="UTF-8")
  gg_utlandskt_agande_anstallda <- funktion_upprepa_forsok_om_fel( function() {diag_utlandskt_agande_bransch_land(cont_klartext = "Antal anställda",
                                                                                                                  skriv_diagramfil = spara_figurer,
                                                                                                                  output_mapp = output_mapp_figur,
                                                                                                                  returnera_data_rmarkdown = TRUE)
  }, hoppa_over = hoppa_over_felhantering)
  
  utlandskt_agande_anstallda_bransch_varde <- get_extremes_by_year(antal_anstallda_utlandskt_agande_bransch_df, varde, Branschgrupp, år,accuracy = 1) %>% 
    slice_max(år, n = 1)
  
  utlandskt_agande_anstallda_land_varde <- get_extremes_by_year(antal_anstallda_utlandskt_agande_land_df, varde, Land, år,accuracy = 1) %>% 
    slice_max(år, n = 1)
  
  # Utländskt ägande - antal arbetsställen
  source(here("skript","diag_utlanskt_agande_anstallda_arbetsstallen.R"), encoding="UTF-8")
  gg_utlandskt_agande_arbetsstallen <- funktion_upprepa_forsok_om_fel( function() {diag_utlandskt_agande_bransch_land(cont_klartext = "Antal arbetsställen",
                                                                                                                      skriv_diagramfil = spara_figurer,
                                                                                                                      output_mapp = output_mapp_figur,
                                                                                                                      returnera_data_rmarkdown = TRUE)
  }, hoppa_over = hoppa_over_felhantering)
  
  utlandskt_agande_arbetsstallen_bransch_varde <- get_extremes_by_year(antal_arbetsstallen_utlandskt_agande_bransch_df, varde, Branschgrupp, år,accuracy = 1) %>% 
    slice_max(år, n = 1)
  
  utlandskt_agande_arbetsstallen_land_varde <- get_extremes_by_year(antal_arbetsstallen_utlandskt_agande_land_df, varde, Land, år,accuracy = 1) %>% 
    slice_max(år, n = 1)
  
  # Sparar global environment i R. Detta för att man skall slippa hämta data varje gång
  save.image(file = glue("{mapp_environment_fil}{repo_namn}.RData"))
  
  end_time <- Sys.time()
  elapsed_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
  cat(sprintf("Hämtning av data klar: Det tog %.2f minuter.", elapsed_time))
  cat("\n\n")
  
  
} else {
  load(glue("{mapp_environment_fil}{repo_namn}.RData"))
}

# Går inte att ladda in från global environment, så skapas alltid här i slutet
# Största arbetsgivare - 1 diagram
source(here("skript","storsta_arbetsgivare_tabell.R"), encoding="UTF-8")
gg_storsta_arbetsgivare <- funktion_upprepa_forsok_om_fel( function() {diag_storsta_arbetsgivare(returnera_data = TRUE)
}, hoppa_over = hoppa_over_felhantering)

storsta_arbetsgivare_privat_varde <- get_extremes_by_year(storsta_arbetsgivare_df, `Antal anställda (privat)`, `Arbetsgivare (privat)`, år,accuracy = 1) %>% 
  slice_max(år, n = 1)

storsta_arbetsgivare_offentlig_varde <- get_extremes_by_year(storsta_arbetsgivare_df, `Antal anställda (offentlig)`, `Arbetsgivare (offentlig)`, år,accuracy = 1) %>% 
  slice_max(år, n = 1)





