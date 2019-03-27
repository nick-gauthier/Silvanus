#' Functions for converting vegetation succession stage to woody biomass (per m2) and back again.
#'
#' @param vegetation
#'
#' @return
#' @export
#'
#' @examples

stage_to_biomass <- function(stage){
  case_when(stage > 35 ~ stage * 0.08066667 - 2.083333,
            stage > 18 ~ stage * 0.004705882 + 0.5752941,
            stage > 7 ~ stage * 0.05090909 - 0.2563636,
            stage >= 0 ~ stage * 0.01428571)
}

biomass_to_stage <- function(biomass){
  round(case_when(biomass > 0.74 ~ biomass * 12.39669 + 25.82645,
                  biomass > 0.66 ~ biomass * 212.5 - 122.25,
                  biomass > 0.1 ~ biomass * 19.64286 + 5.035714,
                  biomass >= 0 ~ biomass * 70))
}

wood_req <- 1600 # yearly firewood requirement (kg) per person
max_wood_gather_intensity <- 0.08 * 1e6 # maximum wood gathering intensity in kg/m2, converted to to kg/km2 ### should tweak to account for grid cellsbeing less than 1km2

gather_wood <- function(settlements){
  settlements %>%
    mutate(available_biomass = stage_to_biomass(vegetation_stage) * vegetated_fraction * 1e6,  # the available biomass in the entire vegetated fraction of the cell
           new_biomass = available_biomass - wood_req * pop ^ 0.8,
           vegetation_stage = biomass_to_stage(new_biomass / (vegetated_fraction * 1e6))
    ) %>%
    select(-c(available_biomass, new_biomass)) %>%
    mutate(vegetation_stage = pmin(vegetation_stage + 1, max_veg)) # regenerate one succession unit of vegetation a year, up to the maximum vegetation
}
