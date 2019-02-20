#' Individual-level death and reproduction according to age-specific, food-limimited fertility and mortality rates
#'
#' This function allows household agents to add or remove individual
#' agents via birth and death. The tibble of households is first unnested
#' to access the tibble of individuals. Then, the probability of
#' giving birth is calculate for each individual from its age and
#' the food ratio of its household. Finally, the probability is
#' in a random bernoulli trial to determine how many babies are
#' actually born to each household each year. Babies are added to
#' the household by adding rows to the individuals tibble with Age = 1.
#' @param households Tibble of household agents containg nested tibbles of individual agents
#' @export
#' @examples
#' reproduce(households)
#' die(households)

reproduce <- function(households){
  households %>%
    mutate(births = map2_int(individuals, food_ratio, calculate_births),
           individuals = map2(individuals, births, give_birth)) %>%
    select(-births) %>%
    mutate(occupants = map_int(individuals, nrow),
           laborers = map_dbl(individuals, ~filter(.x, between(age, 15, 65)) %>% nrow))
}

# internal helper function for reproduce
calculate_births <- function(individuals, food_ratio){
  individuals %>%
    left_join(fertility_table, by = 'age') %>%  # find the fertility rate corresponding to age
    mutate(fertility_reduction = pgamma(food_ratio, shape = fertility_shape, scale = fertility_scale),
           baby = rbernoulli(n(), fertility_rate / 2 * fertility_reduction)) %>%  # divide by two to make everyone female ...
    pull(baby) %>%
    sum
}

# internal helper function for reproduce
give_birth <- function(individuals, births){
  if(births > 0) individuals <- add_row(individuals, age = rep(0, births))
  return(individuals)
}

#' @rdname reproduce

die <- function(households){
  households %>%
    unnest(individuals) %>%
    left_join(mortality_table, by = 'age') %>%
    left_join(survival_elasticity_table, by = 'age') %>%
    mutate(survival_reduction = pgamma(food_ratio, shape = survivor_shape, scale = survivor_scale),
           survive = rbernoulli(n(), (1 - mortality_rate) * survival_reduction)) %>%
    filter(survive == T) %>%
    mutate(age = age + 1) %>% # happy birthday!
    select(-c(survive, mortality_rate, survivor_shape, survival_reduction)) %>%
    nest(age, .key = individuals) %>%
    mutate(occupants = map_int(individuals, nrow),
           laborers = map_dbl(individuals, ~filter(.x, between(age, 15, 65)) %>% nrow))
}
