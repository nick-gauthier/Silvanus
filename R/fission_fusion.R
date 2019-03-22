#' Household fusioning
#'
#' This function allows household agents to split when they get too big.
#' @param households Tibble of household agents containg nested tibbles of individual agents
#' @export
#' @examples
#' reproduce(households)
#' die(households)

fission <- function(households, fission_rate = 0.2){
  check_fission <- households %>%
    unnest %>%
    mutate(crowded = if_else(laborers > 5 & between(age, 15, 50), TRUE, FALSE),
           fission = rbernoulli(n(), p = ifelse(crowded, fission_rate, 0))) %>%
    group_by(household) %>%
    mutate(fissioners = sum(fission)) %>%
    ungroup

  old_households <- check_fission %>%
    filter(fission == FALSE) %>%
    mutate(land = land - pmin(calc_land_need(fissioners, yield_memory), land * fissioners / occupants)) # make sure a fissioner doesn't just take the household's whole plot if its too small

  new_households <- check_fission %>%
    filter(fission == TRUE)

  if(nrow(new_households) > 0){
    new_hh <- new_hh_num(old_households, nrow(new_households))

    new_households <- new_households %>%
      mutate(household = new_hh,
             land = pmin(calc_land_need(1, yield_memory), land / occupants),
             storage = 0)
    out <- bind_rows(old_households, new_households) %>%
      mutate(household = as.factor(household)) # binding converts factors to characters so need to convert back
  } else {out <- old_households}

  out %>%
    select(-c(crowded, fission, fissioners)) %>%
    nest(age, .key = 'individuals')
}
