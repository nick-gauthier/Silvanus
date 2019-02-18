#' Functions for creating individuals, households, and settlements
#'
#'
#' @param households Tibble of household agents.
#' @export
#' @examples
#' allocate_time(households)

allocate_land <- function(households){
  households %>%
    mutate(land = calc_land_req(n_occupants, yield_memory),
           max_land = max_cultivable_land(laborers, farming_labor, area * arable_proportion * 100, fallow = T, type = 'asymptote')) %>%
    mutate(land = pmin(land, max_land)) %>%
    select(-max_land)
}
