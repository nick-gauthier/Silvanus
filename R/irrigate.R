# this function takes the master dataframe and calculates the per patch climatic yields from per patch precipitation
environmental_dynamics <- function(settlements){
  settlements %>%
    mutate(total_land = map_dbl(households, ~sum(.$land)),
           total_maintainance = map_dbl(households, ~ sum(1 - .$farming_labor)),  #should change from sum when going to 2+ hosheolds
           infrastructure_condition = infrastructure_quality(total_maintainance),
           irrigation_water = 1,#infrastructure_condition * runoff * .8 / total_land,
           irrigation_water = if_else(is.finite(irrigation_water), irrigation_water, 0),
           precipitation = 1,
           climatic_yield = calc_climatic_yield(pmin(precipitation + irrigation_water, 3)))
}
