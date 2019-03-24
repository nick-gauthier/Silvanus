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

create_settlements <- function(n_settlements, n_households = 5){

  pts <- rbind(c(0,0), c(30,0), c(30,30), c(0,30), c(0,0)) %>%
    list %>%
    st_polygon %>%
    st_sfc %>%
    st_sample(4)

  tbl_graph(nodes = tibble(settlement = as.factor(1:n_settlements),
                           pts = pts,
                           n_households = n_households,
                           # the following parameters should be set from environmentla rasters if available
                           area = 1,
                           arable = 1,
                           precipitation = 1,
                           runoff = 0,
                           climatic_yield = calc_climatic_yield(precipitation)),
            edges = expand.grid(from = 1:4, to = 1:4)) %E>%
    mutate(distance = st_distance(.N()$pts[from], .N()$pts[to], by_element = TRUE)) %>%
    filter(!edge_is_loop()) %N>%
    mutate(households = map2(n_households, climatic_yield, create_households),
           population = map_dbl(households, ~sum(.$occupants)),
           settled_area = 0.175 * population ^ 0.634)
}



  # mutate(households = map(population, ~tibble(household = as.factor(1:.x),
  #                                             food_ratio = sample((1:20/10), .x, replace = TRUE),
  #                                             laborers = 1)))

#' @rdname create_settlement

create_households <- function(n_households, yield_memory = calc_climatic_yield(1), n_individuals = 4){
  tibble(household = as.factor(1:n_households),
         occupants = n_individuals,
         storage = occupants * wheat_req, # start off with a year's supply of food
         yield_memory = yield_memory, # fond memories
         land = calc_land_need(occupants, yield_memory),
         farming_labor = 1,
         food_ratio = 1) %>%
    mutate(individuals = map(occupants, ~create_individuals(occupants = .)),
           laborers = map_dbl(individuals, ~filter(.x, between(age, 15, 65)) %>% nrow))
}

#' @rdname create_settlement

create_individuals <- function(occupants = 4, random_ages = FALSE, age = 25L){
  if(random_ages == FALSE){
    tibble(age = rep(age, occupants)) # if random is FALSE, set ages to age
  } else tibble(age = sample.int(80, size = occupants, replace = TRUE)) # if random TRUE, random ages under 80
}
