#' Title
#'
#' @param old_households
#' @param new_households
#'
#' @return
#' @export
#'
#' @examples
#' old_households <- create_households(4);new_households <- create_households(5) %>% mutate(household = NA)
#'
bind_households <- function(old_households, new_households){
  if (nrow(new_households) > 0) {
    old_hh_nums <- pull(old_households, household)

    new_hh_nums <- old_hh_nums %>%
      levels %>%
      as.integer %>%
      max %>%
      `+`(1:nrow(new_households)) %>%
      as.factor %>%
      list(old_hh_nums, .) %>%
      fct_unify()

    bind_rows(
      mutate(old_households, household = new_hh_nums[[1]]),
      mutate(new_households, household = new_hh_nums[[2]])
    )
  } else {old_households}
}
