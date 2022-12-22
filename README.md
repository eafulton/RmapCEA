# RmapCEA
Repository for R scripts used for fisheries cumulative assessments in Australia.

This requires the following R packages: sf, terra, tibble, dplyr, mgcv, RColorBrewer and ggplot2

The main R commands are in Details-Appendices-Cumulative-Impact-Fisheries.Rmd 

# Stage 1 - PSA based
This will call the rds files to load the relevant data on the ERA PSA scores per species and the fishing effort maps and then combine to give the final map per species

# Stage 2 - ecosystem model based GAM for nonlinear effects
This loads the GAMs for the individual functional groups - calculated off Ecopath with Ecosim simulations and then applies that to the effort maps to get the final map (with upper and lower confidence bounds) per species.

Additional files include the R scripts to generate the simulations to use in Ecopath with Ecosim and the script to process the resultng output into a form the GAM could use
