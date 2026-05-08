repo_publicera_rapporter_push_github <- function(
    repo_namn,
    github_lokal_sokvag = "c:/gh/",
    github_publicera_rapporter = "publicera_rapporter"  
  ) {
  
  if (!require("pacman")) install.packages("pacman")
  p_load(stringr,
         here)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  
  datum_tid <- format(Sys.time(), "%d %b %Y kl. %H:%M")
  
  github_commit_push(repo = repo_namn, 
                     fran_rmarkdown = TRUE,
                     commit_txt = paste0("Automatisk commit och push av repositoryt till github ", datum_tid))
  
  github_commit_push(repo = github_publicera_rapporter, 
                     fran_rmarkdown = TRUE,
                     commit_txt = paste0("Automatisk commit och push av repositoryt till github ", datum_tid))
  
} # slut funktion