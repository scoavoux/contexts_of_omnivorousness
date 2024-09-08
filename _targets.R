library(targets)
tar_source("R")

list(
  # data import
  tar_target(m, read_mama_data()),
  
  # Exploratory data analysis
  tar_target(gg_genre_context_correlation_raw,          plot_genre_context_correlation(m)),
  tar_target(gg_genre_context_correlation_intensity,    plot_genre_context_correlation(m, music_intensity)),
  
  # Fit models
  tar_target(mama_models,                               compute_model(m)),
  
  # Model results 
  tar_target(mama_treatment_effect,                     plot_treatment_effect(mama_models))
)
