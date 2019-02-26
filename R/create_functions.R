#' Functions for creating individuals, households, and settlements
#'
#'
#' @param n_settlements Number of starting settlement agents
#' @param n_households Number of starting household agents
#' @param n_individuals Number of individuals in each household, 2 by default.
#' @param wheat_req kg of wheat to feed a person for 1 year
#' @export
#' @examples
#' create_settlement(4)
#' create_household(4)
#' create_occupant(4)

create_settlement <- function(n_settlements, n_households = 5){
  tibble(settlement = as.character(1:n_settlements),
         n_households = n_households,
         area = 1, arable = 1, precipitation = 1, runoff = 0, climatic_yield = calc_climatic_yield(precipitation)) %>% # these parameters should be set from environmentla rasters if available
    mutate(households = map2(n_households, climatic_yield, create_household))
}

#' @rdname create_settlement

create_household <- function(n_households, climatic_yield, n_individuals = 2, wheat_req =  283){
  tibble(household = as.character(1:n_households),
         occupants = n_individuals,
         storage = occupants * wheat_req, # start off with a year's supply of food
         yield_memory = climatic_yield, # fond memories
         land = calc_land_need(occupants, yield_memory),
         farming_labor = 1,
         food_ratio = 1) %>%
    mutate(individuals = map(occupants, create_individual),
           laborers = map_dbl(individuals, ~filter(.x, between(age, 15, 65)) %>% nrow))
}

#' @rdname create_settlement

create_individual <- function(occupants){
  tibble(age = rep(25, occupants)) # occupants start off at age 25
}
