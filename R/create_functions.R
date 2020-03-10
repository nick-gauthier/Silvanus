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
#' create_individuals(4)

create_world <- function() {
  rbind(c(0, 0), c(50000, 0), c(50000, 50000), c(0, 50000), c(0, 0)) %>%
    list %>%
    st_polygon %>%
    st_sfc %>%
    st_make_grid(cellsize = 7000, square = FALSE) %>%
    st_sf %>%
    mutate(rainfall = 0.7,
           #streamflow = 0,
           runoff = 0,
           area = st_area(geometry) * 1e-6,
           arable = 1,
           cultivable_area = area * arable) %>%
    select(-area, -arable)
}

create_settlements <- function(world, n_households = 3) {
  n_settlements <- nrow(world)

  pts <- world %>%
    st_centroid() %>%
    st_coordinates()

  world %>%
    mutate(settlement = forcats::fct_explicit_na(as.factor(1:n())),
           x = pts[, 1],
           y = pts[, 2],
           xy = st_centroid(st_geometry(world)),
           n_households = n_households) %>%
    select(settlement, everything()) %>% # move settlement id to first column
    tbl_graph(nodes = .,
              edges = crossing(from = 1:n_settlements, to = 1:n_settlements)) %E>%
    mutate(distance = as.numeric(st_distance(.N()$xy[from], .N()$xy[to], by_element = TRUE)) / 1000) %>%
    filter(!edge_is_loop()) %N>%
    mutate(households = map2(n_households, rainfall, ~create_households(.x, rainfall = .y), headless = FALSE),
           population = map_dbl(households, ~sum(.$occupants)),
           settled_area = 0.175 * population ^ 0.634) %>%
    select(-xy)
}

#' @rdname create_settlement
#' @export
create_households <- function(n_households, rainfall_c = 1, n_individuals = 3) {
  tibble(household = forcats::fct_explicit_na(as.factor(1:n_households)),
         occupants = n_individuals,
         storage = occupants * wheat_req, # start off with a year's supply of food
         farming_labor = 1,
         food_ratio = 1) %>%
    {if (!("rainfall" %in% names(.))) mutate(., rainfall = rainfall_c) else .} %>%
    mutate(yield_memory = calc_climatic_yield(rainfall), # fond memories
           land = calc_land_need(occupants, yield_memory), # technically they can get more land than is available, should put in a check for this
           individuals = map(occupants, ~create_individuals(occupants = .)),
           occupants = map_int(individuals, nrow),
           laborers = map_dbl(individuals, ~filter(.x, between(age, min_labor_age, max_labor_age)) %>% nrow),
           relative_calories = map_dbl(individuals, ~ left_join(., life_table, by = "age") %>% pull(relative_cal_need) %>% mean)) %>%
    select(-rainfall) # need to figure out whether to do headless stuff from create_ functions or dynamics_ functions
}

#' @rdname create_settlement
#' @export
create_individuals <- function(occupants = 4, age = 25L) {
  # if(random_ages == FALSE) {
  #   tibble(age = rep(age, occupants)) # if random is FALSE, set ages to age
  # } else tibble(age = sample.int(80, size = occupants, replace = TRUE)) # if random TRUE, random ages under 80
  tibble(age = rep(age, occupants))
}
