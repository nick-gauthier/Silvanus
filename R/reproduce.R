#' Individual-level reproduction according to age-specific, food-limimited fertility rates
#'
#' This function starts allows household agents to produce new
#' individual agents. The tibble of households is first unnested
#' to access the tibble of individuals. Then, the probability of
#' giving birth is calculate for each individual from its age and
#' the food ratio of its household. Finally, the probability is
#' in a random bernoulli trial to determine how many babies are
#' actually born to each household each year. Babies are added to
#' the household by adding rows to the individuals tibble with Age = 1
#' @param households Tibble of household agents containg nested tibbles of indivudal agents
#' @export
#' @examples
#' reproduce(households)

reproduce <- function(households){
  households %>%
    mutate(births = map2_int(individuals, food_ratio, calculate_births),
           individuals = map2(individuals, births, give_birth)) %>%
    select(-births) %>%
    mutate(occupants = map_int(individuals, nrow),
           laborers = map_dbl(individuals, ~filter(.x, between(age, 15, 65)) %>% nrow))
}

calculate_births <- function(individuals, food_ratio){
  individuals %>%
    left_join(fertility_table, by = 'age') %>%  # find the fertility rate corresponding to age
    mutate(fertility_reduction = pgamma(food_ratio, shape = fertility_shape, scale = fertility_scale),
           baby = rbernoulli(n(), fertility_rate / 2 * fertility_reduction)) %>%  # divide by two to make everyone female ...
    pull(baby) %>%
    sum
}

give_birth <- function(individuals, births){
  if(births > 0) individuals <- add_row(individuals, age = rep(0, births))
  return(individuals)
}

