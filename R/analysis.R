plot_genre_context_correlation <- function(.data, .group=NULL){
  require(tidyverse)
  require(rlang)
  g <- ggplot(.data, aes(nb_genres, nb_contexts, group = {{.group}}, color = {{.group}})) +
    geom_point(position="jitter") +
    geom_smooth(method = "lm")
  ggsave(paste0("gg_genre_context_correlation_", quo_name(enquo(.group)), ".pdf"), g, "pdf", path = "output")
  return(paste0("output/gg_genre_context_correlation_", quo_name(enquo(.group)), ".pdf"))
}

compute_model <- function(.data){
  require(tidyverse)

  # Specification
  response <- 'nb_genres'
  treatment <- "nb_contexts"
  soc_dem_controls <- "sexe + age_cat + education + hh_income"
  intensite <- "music_intensity"
  
  mods <- list()
  mods[["basic"]]  <- lm(as.formula(str_glue("{response} ~ {treatment}")), data=.data)
  mods[["socdem"]] <- lm(as.formula(paste(str_glue("{response} ~ {treatment}"), 
                                          soc_dem_controls, 
                                          sep = " + ")), data=.data)
  mods[["intensite"]] <- lm(as.formula(paste(str_glue("{response} ~ {treatment}"), 
                                             soc_dem_controls,
                                             intensite,
                                             sep = " + ")), data=.data)
  return(mods)
}

plot_treatment_effect <- function(mods){
  require(tidyverse)
  require(broom)

  # as plot
  treatment_effect <- map(mods, ~tidy(.x) %>% filter(term == "nb_contexts")) %>% 
    bind_rows() %>% 
    mutate(model = factor(names(mods), levels = rev(unique(names(mods)))))
  g <- ggplot(treatment_effect, aes(estimate, model, xmin = estimate-1.96*std.error, xmax=estimate+1.96*std.error)) +
    geom_point() +
    geom_linerange()
  # stargazer(mods, dep.var.label = rep("Nb. genres", length(mods)), 
  #           type = "text")
  ggsave(paste0(attr(.data, "data_name"), "_gg_treatment_effect", ".pdf"), g, "pdf", path = "output")
  return(paste0("output/", attr(.data, "data_name"), "_gg_tratement_effect_", ".pdf"))
  
}