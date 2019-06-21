#' Zoom functions
#'
#' These are  internal convencience functions used by the *_dynamics commands
#' that allow functions meant for one level to be called from another level.
#' zoom_in takes a settlement or household tibble and produces as household or
#' individuals tibble, respectively. zoom_out does the opposite.
#' @param level
#' @param FUN
#'
#' @return
#' @export
#'
#' @examples
#'

zoom_in2 <- function(x){
  unnest(x, cols = c(individuals)) #%>%
   #select(household, food_ratio, age)
}


zoom_out2 <- function(x, y){
  cal_need <- x %>%
    group_by(household) %>%
    summarise(relative_calories = mean(relative_cal_need))
  x <- select(x, -relative_cal_need)

  y %>%
    select(-individuals, - relative_calories) %>%
    nest_join(x, by = 'household', name = 'individuals') %>%
      left_join(cal_need, by = 'household') %>%
      household_census() %>%
      filter(occupants > 0)
}
# zoom_in <- function(x, level){
# # what does all this nesting and unnesting do to the "unique' hh id factor levels?
#   #if ('settlement' %in% names(x)){
#   #  x %N>%
#   #}
#   if (level == 'individuals') {
#
#
#
#
#     old_settlements <- settlements %N>%
#       as_tibble %>%
#       select(settlement, households)
#
#     old_households <- old_settlements %>%
#       unnest
#
#     new_individuals <- old_households %>%
#       unnest %>%
#       select(household, food_ratio, age) %>%
#       population_dynamics()
#
#     new_households <- old_households %>%
#       select(-individuals) %>%
#       nest_join(new_individuals, by = c('settlement', 'household'), name = 'individuals') %>%
#       household_census()
#   }
#
#
#   if (level == 'households'){
#     old_settlements <- settlements %N>%
#       as_tibble()
#
#     new_households <- old_settlements %>%
#       select(settlement, precipitation, runoff, cultivable_area, households) %>%
#       unnest(households) %>%
#       household_dynamics()
#   }
#
# }
#
# zoom_out <- function(from, to){
#     new_settlements <- old_settlements %>%
#       select(-households) %>%
#       nest_join(new_households, by = 'settlement', name = 'households') %>%
#       select(settlement, households)
#
#     to %>%
#       select(-households) %>%
#       left_join(from, by = 'settlement')
#     # so the question is do we want healdess be a way to differentiate create_ functions or _dynamics functions
# }
#
# add_groups <- function(x){
#   {if ('settlement' %in% names(x)) group_by(x, settlement) else x} %>%
#   {if ('household' %in% names(.)) group_by(., household, add = TRUE) else .}
# }
