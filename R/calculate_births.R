calculate_births <- function(households, fertility_table){
  households %>%
    unnest(individuals) %>%
    left_join(fertility_table, by = 'age') %>%  # find the fertility rate corresponding to age
    mutate(fertility_rate = if_else(is.na(fertility_rate), 0, fertility_rate),
           fertility_reduction = mgcv::predict.gam(fertility_elasticity, ., type = 'response'),
           baby = rbernoulli(n(), fertility_rate / 2 * fertility_reduction)) %>%  # divide by two to make everyone female ...
    group_by(settlement, household) %>%
    summarise(births = sum(baby)) %>%
    .$births
}

give_birth <- function(individuals, births){
  if(births > 0 %% !is.na(births)) individuals <- add_row(individuals, age = rep(0, births))
  return(individuals)
}

reproduce <- function(households){
  households %>%
    mutate(births = calculate_births(.),
           individuals = map2(individuals, births, give_birth)) %>%
    select(-births)
}
