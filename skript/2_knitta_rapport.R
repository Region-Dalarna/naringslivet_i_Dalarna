# Ctrl+A följt av ctrl+enter för att knitta rapport 

knitta_webbrapport <- function(
    repo_namn = NULL, 
    github_lokal_sokvag = "c:/gh/"
    ) {
  
  if (!require("pacman")) install.packages("pacman")
  p_load(here,
         stringr,
         glue)
  
  if (is.null(repo_namn) & !is.null(rstudioapi::getActiveProject())) {
    repo_lokal_sokvag <- here()
  } else {
    repo_lokal_sokvag <- glue("{github_lokal_sokvag}{repo_namn}")
  }
  if (!str_ends(repo_lokal_sokvag, "/")) repo_lokal_sokvag <- paste0(repo_lokal_sokvag, "/")
  
  senaste_rmd_filen <- list.files(repo_lokal_sokvag, pattern = "\\.Rmd$", full.names = TRUE) %>% 
    .[which.max(file.info(.)$mtime)]
  
  renderad_fil <- senaste_rmd_filen %>% 
    str_replace(".Rmd", ".html")
  
  rmarkdown::render(
    input = senaste_rmd_filen,
    output_file = renderad_fil,
    envir = parent.frame()
  )
} # slut funktion

knitta_webbrapport(repo_namn = "naringslivet_i_Dalarna")