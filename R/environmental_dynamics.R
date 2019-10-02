#' Environmental dynamics
#'
#' This function takes the master dataframe and calculates the
#' per patch climatic yields from per patch precipitation and irrigation
#' @param settlements Tibble of settlement agents.
#' @export
#' @examples
#' allocate_time(households)

environmental_dynamics <- function(settlements) {
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


calc_climatic_yield <- function(precipitation) {
  max_yield * pmax(0, 0.51 * log(precipitation) + 1.03)  # annual precipitation impact on yields
}

calc_yield_reduction <- function(fertility, climate_yield) {
  climate_yield * pmax(0, 0.19 * log(fertility / 100) + 1)  # fertility impact on yields
}
#
# #Define parameters controlling the soil fertility dynamics.
# carrying.capacity <- 100
# degradation <- tibble(type = c('reversible', 'hysteresis', 'irreversible'),
#                       degradation_factor = c(0, 1, 2),
#                       regeneration_rate = c(0.05, 0.0844, 0.11880))
# depletion.rate <- 0.25 # .25  to represent biennial fallow, .5 otherwise
#
#
# #Soil fertility dynamics
# soil.dynamics <- function(x, population, soil.type = 'reversible'){
#   params <- filter(degradation, type == soil.type)
#   params$regeneration_rate * x * (x / carrying.capacity) ^ params$degradation_factor * (1 - x / carrying.capacity) - depletion.rate
# }
