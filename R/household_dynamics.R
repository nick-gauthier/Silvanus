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
      census %>%
      fission %>%
      census
      nest(household:last(everything()), .key = households) %>%
      mutate(population = map_dbl(households, ~ sum(.$occupants)),
             urban_area = 0.175 * population ^ 0.634)
    } else {settlements}
}

census <- function(households){
  households %>%
    mutate(occupants = map_int(individuals, nrow),
           laborers = map_dbl(individuals, ~filter(.x, between(age, 15, 65)) %>% nrow))
}

new_hh_num <- function(x, n){
  x %>%
    as.integer %>%
    max %>%
    `+`(1:n) %>%
    as.character
}
