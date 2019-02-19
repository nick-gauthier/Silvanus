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
  tibble(settlement = 1:n_settlements,
         n_households = n_households) %>%
    mutate(households = map(n_households, create_household))
}

#' @rdname create_settlement

create_household <- function(n_households, n_individuals = 2, wheat_req =  283){
  tibble(household = 1:n_households,
         n_occupants = n_individuals,
         storage = n_occupants * wheat_req, # start off with a year's supply of food
         #yield_memory = max_yield, # fond memories
         #land = calc_land_req(n_occupants, yield_memory),
         farming_labor = 1,
         food_ratio = 1) %>%
    mutate(occupants = map(n_occupants, create_occupant),
           laborers = map_dbl(occupants, ~filter(.x, between(age, 15, 65)) %>% nrow))
}

#' @rdname create_settlement

create_occupant <- function(n_occupants){
  tibble(age = rep(25, n_occupants)) # occupants start off at age 25
}
