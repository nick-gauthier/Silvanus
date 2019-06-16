#' Household fusioning
#'
#' This function allows household agents to split when they get too big.
#' @param households Tibble of household agents containg nested tibbles of individual agents
#' @export
#' @examples
#' reproduce(households)
#' die(households)
#households<- create_households(9) %>% mutate(settlement = as.factor(rep(1:3, each = 3))) %>% select(settlement, everything()) %>% mutate(laborers = 6)

fission <- function(households, fission_rate = 0.2){
  check_fission <- households %>%
    unnest(cols = c(individuals)) %>%
    mutate(crowded = if_else(laborers > 5 & between(age, 15, 50), TRUE, FALSE),
           fission = rbernoulli(n(), p = ifelse(crowded, fission_rate, 0))) %>%
    group_by(household) %>%
    mutate(fissioners = sum(fission)) %>%
    ungroup

  old_households <- check_fission %>%
    filter(fission == FALSE) %>%
    mutate(land = land - pmin(calc_land_need(fissioners, yield_memory), land * fissioners / occupants)) # make sure a fissioner doesn't just take the household's whole plot if its too small

  new_households <- check_fission %>%
    filter(fission == TRUE) %>%
    mutate(land = pmin(calc_land_need(1, yield_memory), land / occupants),
           storage = 0)

  bind_households(old_households, new_households) %>%
    select(-c(crowded, fission, fissioners)) %>%
    nest(individuals = c(age))
}
