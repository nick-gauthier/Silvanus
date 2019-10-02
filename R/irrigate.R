# this function takes the master dataframe and calculates the per patch climatic yields from per patch rainfall
#' Title
#'
#' @param settlements
#'
#' @return
#' @export
#'
#' @examples
#' create_households(5)
irrigate <- function(households) {
  households %>%
    mutate(infrastructure_condition = infrastructure_quality(1 - farming_labor),
           irrigation_water = infrastructure_condition * runoff,
           climatic_yield = calc_climatic_yield(pmin(rainfall + irrigation_water))) %>%
    select(-infrastructure_condition, -irrigation_water)
}

irrigate2 <- function(settlements) {
  settlements %>%
    mutate(total_land = map_dbl(households, ~sum(.$land)),
           total_maintainance = map_dbl(households, ~ sum(1 - .$farming_labor)),  #should change from sum when going to 2+ hosheolds
           infrastructure_condition = infrastructure_quality(total_maintainance),
           irrigation_water = infrastructure_condition * runoff,
           climatic_yield = calc_climatic_yield(pmin(rainfall + irrigation_water, 3)))
}



infrastructure_quality <- function(maintainance_labor, psi = 0.2, epsilon = 0.18, max_irrigation = 1) {
  #replace with case when
  ifelse(0 <= maintainance_labor & maintainance_labor < (psi - epsilon), 0,
         ifelse(between(maintainance_labor, psi - epsilon, psi + epsilon),
                max_irrigation / (2 * epsilon) * (maintainance_labor - psi + epsilon),
                max_irrigation))
}
