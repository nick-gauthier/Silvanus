#' Functions for creating individuals, households, and settlements
#'
#'
#' @param households Tibble of household agents.
#' @export
#' @examples
#' allocate_time(households)

farm <- function(households){
  households %>%
    mutate(yield = climatic_yield,
           yield_memory = yield,
           harvest = land * yield * .5 - land * sowing_rate) # halve the yields to represent biennial fallow
}
