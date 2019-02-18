#' Functions for creating individuals, households, and settlements
#'
#'
#' @param households Tibble of household agents.
#' @export
#' @examples
#' allocate_time(households)

create.households <- function(x){
  tibble(household = 1:x,
         n_occupants = init_occupants,
         storage = n_occupants * wheat_req, # start off with a year's supply of food
         yield_memory = max_yield, # fond memories
         land = calc_land_req(n_occupants, yield_memory),
         farming_labor = 1,
         food_ratio = 1) %>%
    mutate(occupants = map(n_occupants, create.occupants),
           laborers = map_dbl(occupants, ~filter(.x, between(age, 15, 65)) %>% nrow)) #
}

create.occupants <- function(x){
  tibble(age = rep(25, x)) # occupants start off at age 25
}
