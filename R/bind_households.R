#' Title
#'
#' @param old_households
#' @param new_households
#'
#' @return
#' @export
#'
#' @examples
bind_households <- function(old_households, new_households){
  if (nrow(new_households) > 0) {
    new_households <- new_households %>%
      mutate(household = new_hh_num(old_households, n()))

    out <- suppressWarnings(bind_rows(old_households, new_households)) %>% # suppress warning about binding unequal factor levels
      mutate(household = as.factor(household)) # binding converts factors to characters so need to convert back
  } else {out <- old_households}
  return(out)
}

new_hh_num <- function(x, n){
  x %>%
    pull(household) %>%
    levels %>%
    as.integer %>%
    max %>%
    `+`(1:n) %>%
    as.factor
}
