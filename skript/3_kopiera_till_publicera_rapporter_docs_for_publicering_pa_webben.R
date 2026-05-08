
kopiera_till_publicera_rapporter_docs <- function(
    repo_namn = NULL, 
    github_lokal_sokvag = "c:/gh/",
    github_publicera_rapporter = "publicera_rapporter/docs/"    # repo-namn för det repo där man publicerar rapporter
  ) {

if (!require("pacman")) install.packages("pacman")
p_load(here)

if (is.null(repo_namn) & !is.null(rstudioapi::getActiveProject())) {
  repo_lokal_sokvag <- here()
} else {
  repo_lokal_sokvag <- glue("{github_lokal_sokvag}{repo_namn}")
}
if (!str_ends(repo_lokal_sokvag, "/")) repo_lokal_sokvag <- paste0(repo_lokal_sokvag, "/")


senaste_html_filen <- list.files(repo_lokal_sokvag, pattern = "\\.html$", full.names = TRUE) %>% 
  .[which.max(file.info(.)$mtime)]

publicera_rapporter_sokvag <- paste0(github_lokal_sokvag, github_publicera_rapporter)
if (!str_ends(publicera_rapporter_sokvag, "/")) publicera_rapporter_sokvag <- paste0(publicera_rapporter_sokvag, "/")
# kopiera html-filen till det lokala repot för publicera rapporter 
file.copy(from = paste0(senaste_html_filen), to = paste0(publicera_rapporter_sokvag, basename(senaste_html_filen)), overwrite = TRUE)



} # slut funktion
