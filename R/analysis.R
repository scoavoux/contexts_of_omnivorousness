compute_model <- function(m){
  require(tidyverse)
  require(stargazer)
  
  # Specification
  response <- 'nb_genres'
  treatment <- "nb_contexts"
  soc_dem_controls <- "sexe + age_cat + education + hh_income"
  intensite <- "mama_q95"
  
  mods <- list()
  mods[["basic"]]  <- lm(as.formula(str_glue("{response} ~ {treatment}")), data=m)
  mods[["socdem"]] <- lm(as.formula(paste(str_glue("{response} ~ {treatment}"), 
                                          soc_dem_controls, 
                                          sep = " + ")), data=m)
  mods[["intensite"]] <- lm(as.formula(paste(str_glue("{response} ~ {treatment}"), 
                                             soc_dem_controls,
                                             intensite,
                                             sep = " + ")), data=m)
  
  stargazer(mods, dep.var.label = rep("Nb. genres", length(mods)), 
            type = "text")
}
