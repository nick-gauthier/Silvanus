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

create_world <- function(){
  rbind(c(0, 0), c(50000, 0), c(50000, 50000), c(0, 50000), c(0, 0)) %>%
    list %>%
    st_polygon %>%
    st_sfc %>%
    st_make_grid(cellsize = 7000, square = FALSE) %>%
    st_sf %>%
    mutate(precipitation = .5,
           runoff = 0,
           area = st_area(geometry) * 1e-6,
           arable = 1,
           cultivable_area = area * arable) %>%
    select(-area, -arable)
}

create_settlements <- function(world, n_households = 3){
  n_settlements <- nrow(world)

  pts <- world %>%
    st_centroid() %>%
    st_coordinates()

  world %>%
    mutate(settlement = forcats::fct_explicit_na(as.factor(1:n())),
           x = pts[,1],
           y = pts[,2],
           xy = st_centroid(st_geometry(world)),
           n_households = n_households) %>%
    select(settlement, everything()) %>% # move settlement id to first column
    tbl_graph(nodes = .,
              edges = crossing(from = 1:n_settlements, to = 1:n_settlements)) %E>%
    mutate(distance = as.numeric(st_distance(.N()$xy[from], .N()$xy[to], by_element = TRUE)) / 1000) %>%
    filter(!edge_is_loop()) %N>%
    mutate(households = map2(n_households, precipitation, ~create_households(.x, precipitation = .y), headless = FALSE),
           population = map_dbl(households, ~sum(.$occupants)),
           settled_area = 0.175 * population ^ 0.634) %>%
    select(-xy)
}

#' @rdname create_settlement

create_households <- function(n_households, precipitation = 1, n_individuals = 3, headless = TRUE){
  tibble(household = forcats::fct_explicit_na(as.factor(1:n_households)),
         occupants = n_individuals,
         storage = occupants * wheat_req, # start off with a year's supply of food
         yield_memory = calc_climatic_yield(precipitation), # fond memories
         land = calc_land_need(occupants, yield_memory), # technically they can get more land than is available, should put in a check for this
         farming_labor = 1,
         food_ratio = 1) %>%
    mutate(individuals = map(occupants, ~create_individuals(occupants = .), headless = FALSE),
           laborers = map_dbl(individuals, ~filter(.x, between(age, 15, 65)) %>% nrow))
}

#' @rdname create_settlement

create_individuals <- function(occupants = 4, random_ages = FALSE, age = 25L, headless = TRUE){
  if(random_ages == FALSE){
    tibble(age = rep(age, occupants)) # if random is FALSE, set ages to age
  } else tibble(age = sample.int(80, size = occupants, replace = TRUE)) # if random TRUE, random ages under 80
}
