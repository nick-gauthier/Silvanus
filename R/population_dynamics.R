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

population_dynamics <- function(individuals, food_ratio){
  if (nrow(individuals) > 0) {
    individuals %>%
      calculate_vital_rates %>%
      reproduce %>%
      die %>%
      mutate(age = age + 1L) # happy birthday!
  } else individuals
}

calculate_vital_rates <- function(individuals, food_ratio = 1){
  individuals %>%
    left_join(life_table, by = 'age') %>% # get vital rates corresponding to age
    mutate(fertility_reduction = pgamma(pmin(1, food_ratio), shape = fertility_shape, scale = fertility_scale),
           survival_reduction = pgamma(pmin(1, food_ratio), shape = survivor_shape, scale = survivor_scale))
}
#still a fertility reduction of 0.981 when food ratio is 1. fix.

reproduce <- function(individuals){
  births <- individuals %>%
    mutate(reproduced = rbernoulli(n(), fertility_rate / 2 * fertility_reduction)) %>%  # divide by two to make everyone female
    pull(reproduced) %>%
    sum

  life_table %>%
    slice(rep.int(1L, times = births)) %>%
    bind_rows(individuals, .)
}


#' @rdname reproduce

die <- function(individuals, food_ratio = 1){
  individuals %>%
    mutate(survived = rbernoulli(n(), (1 - mortality_rate) * survival_reduction)) %>% # convert mortality rate to survival rate
    filter(survived == TRUE) %>%
    select(-c(fertility_rate, fertility_reduction, mortality_rate, survivor_shape, survival_reduction, survived))
}

