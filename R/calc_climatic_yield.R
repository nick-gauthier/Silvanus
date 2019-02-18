#' Functions for creating individuals, households, and settlements
#'
#'Crop yields are determined by rainfall and soil fertility.
#'The climatic potential yield (determined by precipitation) is
#'calculated separately from successive local yield reduction
#'factors. This allows the climatic potential yield to be easily
#'substituted by potential yields from the crop model component
#'of an Earth System Model in future simulations.
#' @param households Tibble of household agents.
#' @export
#' @examples
#' allocate_time(households)


calc_climatic_yield <- function(precipitation){
  max_yield * pmax(0, 0.51 * log(precipitation) + 1.03)  # annual precipitation impact on yields
}

calc_yield_reduction <- function(fertility, climate_yield){
  climate_yield * pmax(0, 0.19 * log(fertility / 100) + 1)  # fertility impact on yields
}
