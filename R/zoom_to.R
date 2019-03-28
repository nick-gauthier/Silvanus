#' Title
#'
#' @param level
#' @param FUN
#'
#' @return
#' @export
#'
#' @examples
zoom_to <- function(settlements, level){
# what does all this nesting and unnesting do to the "unique' hh id factor levels?
  if (level == 'individuals') {
    old_settlements <- settlements %N>%
      as_tibble %>%
      select(settlement, households)

    old_households <- old_settlements %>%
      unnest

    new_individuals <- old_households %>%
      unnest %>%
      select(settlement, household, food_ratio, age) %>%
      population_dynamics(headless = FALSE)

    new_households <- old_households %>%
      select(-individuals) %>%
      nest_join(new_individuals, by = c('settlement', 'household'), name = 'individuals') %>%
      household_census()

    new_settlements <- old_settlements %>%
      select(-households) %>%
      nest_join(new_households, by = 'settlement', name = 'households')

    settlements %>%
      select(-households) %>%
      left_join(new_settlements, by = 'settlement')
  }

  if (level == 'households'){
    settlements %N>%
      as_tibble() %>%
      select(settlement, precipitation, runoff, cultivable_area, households) %>%
      unnest(households) %>%
      household_dynamics(headless = FALSE)
  }

}

add_groups <- function(x){
  {if ('settlement' %in% names(x)) group_by(x, settlement) else x} %>%
  {if ('household' %in% names(.)) group_by(., household, add = TRUE) else .}
}
