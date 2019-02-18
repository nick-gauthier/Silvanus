
# this function takes the master dataframe and calculates the per patch climatic yields from per patch precipitation and irrigation
environmental_dynamics <- function(settlements){
  settlements %>%
    mutate(total_land = map_dbl(households, ~sum(.$land)),
           total_maintainance = map_dbl(households, ~ sum(1 - .$farming_labor)),
           infrastructure_condition = infrastructure_performance(total_maintainance),
           irrigation_water = infrastructure_condition * runoff / total_land,
           irrigation_water = if_else(is.finite(irrigation_water), irrigation_water, 0), # catch a divide by zero error
           precipitation = 1,
           climatic_yield = calc_climatic_yield(precipitation + irrigation_water)) %>%
    select(-(total_land:irrigation_water))
}
