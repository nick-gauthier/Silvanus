#' Functions for creating individuals, households, and settlements
#'
#'
#' @param households Tibble of household agents.
#' @export
#' @examples
#' allocate_time(households)

max_cultivable_land <- function(laborers, farming_labor, available_area, fallow = T, type = 'asymptote'){
  potential_area <- max_labor * farming_labor * laborers * ifelse(fallow, 2, 1) / labor.per.hectare
  if(type == 'unlimited') return(potential_area)
  if(type == 'step') return(pmin(available_area, potential_area))
  if(type == 'asymptote') return(available_area * (1 - exp(-potential_area / available_area)))
}
