#' Household Dynamics
#'
#' This function takes a settlement (tibble of households), and makes the households do what they need to do.
#' @param settlements Tibble of settlement agents.
#' @export
#' @examples
#' allocate_time(households)

household_dynamics <- function(households, cultivable_area = 1, precipitation = 1, runoff = 0){
  if(nrow(households) > 0){
    households %>%
      allocate_time(precipitation, runoff) %>%
      allocate_land(cultivable_area) %>%
      produce_food(precipitation) %>%
      mutate(individuals = map2(individuals, food_ratio, population_dynamics)) %>%
      household_census %>%
      fission %>%
      household_census
    } else {households}
}

# need to think more about the order of allocate_time and allocate_land, and how they should best connect

household_census <- function(households){
  households %>%
    mutate(occupants = map_int(individuals, nrow),
           laborers = map_dbl(individuals, ~filter(.x, between(age, 15, 65)) %>% nrow))
}
