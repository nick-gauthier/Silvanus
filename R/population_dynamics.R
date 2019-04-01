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
population_dynamics <- function(x, food_ratio_c = 1){
  if (nrow(x) > 0) {
    if ('settlement' %in% names(x)) {}

    if ('household' %in% names(x)) individuals <- zoom_in2(x) else individuals <- x

    new_individuals <- individuals %>%
      left_join(life_table, by = 'age') %>% # get vital rates corresponding to age
      {if (!('food_ratio' %in% names(.))) mutate(., food_ratio = food_ratio_c) else .} %>%
      reproduce %>%
      die %>%
      mutate(age = age + 1L) %>% # happy birthday!
      select(-c(fertility_rate, survival_rate, survival_shape, survival_reduction, survived, food_ratio, relative_cal_need))

    if ('household' %in% names(x)) zoom_out2(new_individuals, x) else new_individuals

  } else x
}

#still a fertility reduction of 0.981 when food ratio is 1. fix.
reproduce <- function(individuals){
  individuals %>%
    mutate(fertility_reduction = pgamma(pmin(1, food_ratio), shape = fertility_shape, scale = fertility_scale),
           reproduced = rbernoulli(n(), fertility_rate / 2 * fertility_reduction)) %>% # divide by two to make everyone female
    filter(reproduced == TRUE) %>% # if nrows == 0, will give a (for some reason unsuppressible) warning
    {if (('household' %in% names(.)) & (nrow(.) > 0)) group_by(., household) else .} %>%
    tally %>% # count births per household
    uncount(n) %>% # repeat rows based on birth count per household
    mutate(age = 0) %>%
    left_join(life_table, by = 'age') %>%
    bind_rows(individuals, .) %>%
    {if (('household' %in% names(.)) & (nrow(.) > 0)) group_by(., household) else .} %>%
    fill(food_ratio) %>% #  group here so fill respects household membership while propagating food_ratio. This line is the bulk of the computational expense of the entire model ... refactor!
    ungroup
}

#currently newborns age at the end of the time step, so technically there never are any newborns.
# so perhaps age 0 should be different from newborns. that is, the model year of birth is like gestation.
# if you are born, there's no chance of dying, but your age is NA. Next year, your age is 0 and there is a chance of dying.
#basically I wonder if there current implementation "misses" a year of life. Something to think about, although its not a huge deal rn.

#' @rdname reproduce

die <- function(individuals, food_ratio){
  individuals %>%
    mutate(survival_reduction = pgamma(pmin(1, food_ratio), shape = survival_shape, scale = survivor_scale),
           survived = rbernoulli(n(), survival_rate * survival_reduction)) %>%
    filter(survived == TRUE)
}
