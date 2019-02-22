#' Household Dynamics
#'
#' This function takes a settlement (tibble of households), and makes the households do what they need to do.
#' @param settlements Tibble of settlement agents.
#' @export
#' @examples
#' allocate_time(households)

household_dynamics <- function(settlements){
  if(nrow(settlements) > 0){
    settlements %>%
      unnest(households) %>%
      allocate_time %>%
      allocate_land %>%
      farm %>%
      eat %>%
      population_dynamics %>%
      nest(household:last(everything()), .key = households) %>%
      mutate(population = map_dbl(households, ~ sum(.$occupants)))
    } else {settlements}
}
