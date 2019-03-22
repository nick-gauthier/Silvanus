#' Individual-level death and reproduction according to age-specific, food-limited fertility and mortality rates
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
#' population_dynamics(individuals, food_ratio = 1)

population_dynamics <- function(individuals, food_ratio = 1){
  if(nrow(individuals) > 0){individuals %>%
    reproduce(food_ratio) %>%
    die(food_ratio) %>%
    mutate(age = age + 1L) # happy birthday!
  } else individuals
}

reproduce <- function(individuals, food_ratio = 1){
  individuals %>%
    left_join(fertility_table, by = 'age') %>% # find vital rates corresponding to age
    mutate(fertility_reduction = pgamma(pmin(1, food_ratio), shape = fertility_shape, scale = fertility_scale),
           reproduced = rbernoulli(n(), fertility_rate / 2 * fertility_reduction)) %>%  # divide by two to make everyone female ...
    give_birth %>%
    select(-c(fertility_rate, fertility_reduction, reproduced))
}

#still a fertility reduction of 0.981 when food ratio is 1. fix.

# internal helper function for reproduce
give_birth <- function(individuals){
  births <- pull(individuals, reproduced) %>% sum

  if(births > 0){
    individuals <- add_row(individuals, age = rep(0, births))
  }

  return(individuals)
}

#' @rdname reproduce

die <- function(individuals, food_ratio = 1){
  individuals %>%
    left_join(mortality_table, by = 'age') %>% # find vital rates corresponding to age
    mutate(survival_reduction = pgamma(pmin(1, food_ratio), shape = survivor_shape, scale = survivor_scale),
           survived = rbernoulli(n(), (1 - mortality_rate) * survival_reduction)) %>%
    filter(survived == TRUE) %>%
    select(-c(mortality_rate, survivor_shape, survival_reduction, survived))
}



