#' Functions for creating individuals, households, and settlements
#'
#'
#' @param households Tibble of household agents.
#' @export
#' @examples
#' allocate_time(households)

allocate_land <- function(households){
  households %>%
    mutate(land = calc_land_req(occupants, yield_memory),
           max_land = max_cultivable_land(laborers, farming_labor, area * arable_proportion * 100)) %>%
    mutate(land = pmin(land, max_land)) %>%
    select(-max_land)
}

calc_land_req <- function(occupants, yield, fallow = TRUE){
  wheat_req * n_occupants * (1 + seed_proportion) / yield * ifelse(fallow, 2, 1)
}

max_cultivable_land <- function(laborers, farming_labor, available_area, fallow = TRUE, type = 'asymptote'){
  potential_area <- max_labor * farming_labor * laborers * ifelse(fallow, 2, 1) / labor_per_hectare
  if(type == 'unlimited') return(potential_area)
  if(type == 'step') return(pmin(available_area, potential_area))
  if(type == 'asymptote') return(available_area * (1 - exp(-potential_area / available_area)))
}

