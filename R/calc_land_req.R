#' Functions for creating individuals, households, and settlements
#'
#'
#' @param households Tibble of household agents.
#' @export
#' @examples
#' allocate_time(households)

calc_land_req <- function(n_occupants, yield, fallow = T){
  wheat_req * n_occupants * (1 + seed_proportion) / yield * ifelse(fallow, 2, 1)
}
