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
       glue)

mapp_environment_fil = "g:/skript/projekt/environments/" # OBS: Får ej ändras
repo_namn = "naringslivet_i_Dalarna" # OBS: Får ej ändras

if(uppdatera_data == TRUE){
  
  output_mapp_figur = here("figurer","/")
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  
  cat("Hämtning av data påbörjad\n")
  start_time <- Sys.time()
  
  # Antal aktiva företag
  # Utländskt ägande - antal arbetsställen
  source(here("skript","diag_aktiva_foretag_mm_tva.R"), encoding="UTF-8")
  gg_aktiva_foretag <- funktion_upprepa_forsok_om_fel( function() {diag_aktiva_foretag_bransch_lan(cont_klartext = "Antal aktiva företag",
                                                                                                   skriv_diagramfil = spara_figurer,
                                                                                                   output_mapp = output_mapp_figur,
                                                                                                   returnera_data_rmarkdown = TRUE)
  }, hoppa_over = hoppa_over_felhantering)

  # Förädlingsvärde
  source(here("skript","diag_fek_bransch_fran_2022_korrekt.R"), encoding="UTF-8")
  gg_foradlingsvarde_bransch <- funktion_upprepa_forsok_om_fel( function() {diag_fek_bransch_lan_scb(skriv_diagramfil = spara_figurer,
                                                                                                     output_mapp = output_mapp_figur,
                                                                                                     returnera_data_rmarkdown = TRUE)
  }, hoppa_over = hoppa_over_felhantering)
  
  # Nettoomsättning
  source(here("skript","diag_fek_bransch_fran_2022_korrekt.R"), encoding="UTF-8")
  gg_nettoomsattning_bransch <- funktion_upprepa_forsok_om_fel( function() {diag_fek_bransch_lan_scb(cont_klartext = "Nettoomsättning exkl. merchantingkostnader, mnkr",
                                                                                                     skriv_diagramfil = spara_figurer,
                                                                                                     output_mapp = output_mapp_figur,
                                                                                                     returnera_data_rmarkdown = TRUE)
  }, hoppa_over = hoppa_over_felhantering)
  
  source(here("skript","diag_fek_bransch_fran_2022_korrekt.R"), encoding="UTF-8")
  gg_antal_anstallda_bransch <- funktion_upprepa_forsok_om_fel( function() {diag_fek_bransch_lan_scb(cont_klartext = "Antal anställda",
                                                                                                     skriv_diagramfil = spara_figurer,
                                                                                                     output_mapp = output_mapp_figur,
                                                                                                     returnera_data_rmarkdown = TRUE)
  }, hoppa_over = hoppa_over_felhantering)
  
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
 
  # Utländskt ägande - antal anställda
  source(here("skript","diag_utlanskt_agande_anstallda_arbetsstallen.R"), encoding="UTF-8")
  gg_utlandskt_agande_anstallda <- funktion_upprepa_forsok_om_fel( function() {diag_utlandskt_agande_bransch_land(cont_klartext = "Antal anställda",
                                                                                                                  skriv_diagramfil = spara_figurer,
                                                                                                                  output_mapp = output_mapp_figur,
                                                                                                                  returnera_data_rmarkdown = TRUE)
  }, hoppa_over = hoppa_over_felhantering)
  
  # Utländskt ägande - antal arbetsställen
  source(here("skript","diag_utlanskt_agande_anstallda_arbetsstallen.R"), encoding="UTF-8")
  gg_utlandskt_agande_arbetsstallen <- funktion_upprepa_forsok_om_fel( function() {diag_utlandskt_agande_bransch_land(cont_klartext = "Antal arbetsställen",
                                                                                                                      skriv_diagramfil = spara_figurer,
                                                                                                                      output_mapp = output_mapp_figur,
                                                                                                                      returnera_data_rmarkdown = TRUE)
  }, hoppa_over = hoppa_over_felhantering)
  
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




