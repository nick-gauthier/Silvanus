#' Household Dynamics
#'
#' This function takes a settlement (tibble of households), and makes the households do what they need to do.
#' @param settlements Tibble of settlement agents.
#' @export
#' @examples
#' allocate_time(households)

household_dynamics <- function(households, cultivable_area_c = 1, rainfall_c = 1, streamflow_c = 0){
  if(nrow(households) > 0){
    households %>%
    {if (!('cultivable_area' %in% names(.))) mutate(., cultivable_area = cultivable_area_c) else .} %>%
    {if (!('rainfall' %in% names(.))) mutate(., rainfall = rainfall_c) else .} %>%
    {if (!('streamflow' %in% names(.))) mutate(., streamflow = streamflow_c) else .} %>%
      allocate_time %>%
      allocate_land %>%
      produce_food %>%
      fission %>%
      household_census
    } else {households}
}

# need to think more about the order of allocate_time and allocate_land, and how they should best connect

household_census <- function(households){
  households %>%
    mutate(occupants = map_int(individuals, nrow),
           laborers = map_dbl(individuals, ~filter(.x, between(age, 15, 65)) %>% nrow))
}
