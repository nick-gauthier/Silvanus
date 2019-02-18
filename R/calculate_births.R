calculate_births <- function(households){
  households %>%
    unnest(occupants) %>%
    left_join(fertility_table, by = 'age') %>%  # find the fertility rate corresponding to age
    mutate(fertility_rate = if_else(is.na(fertility_rate), 0, fertility_rate),
           fertility_reduction = mgcv::predict.gam(fertility_elasticity, ., type = 'response'),
           baby = rbernoulli(n(), fertility_rate / 2 * fertility_reduction)) %>%  # divide by two to make everyone female ...
    group_by(settlement, household) %>%
    summarise(births = sum(baby)) %>%
    .$births
}

give_birth <- function(occupants, births){
  if(births > 0 %% !is.na(births)) occupants <- add_row(occupants, age = rep(0, births))
  return(occupants)
}

reproduce <- function(households){
  households %>%
    mutate(births = calculate_births(.),
           occupants = map2(occupants, births, give_birth)) %>%
    select(-births)
}
