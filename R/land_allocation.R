#' Functions for allocating land
#'
#'
#' @param households Tibble of household agents.
#' @export
#' @examples
#' allocate_time(households)

allocate_land <- function(households){
  households %>%
    mutate(max_land = max_cultivable_land(laborers, farming_labor, area * arable * 100, type = 'unlimited'),
           land_need = pmin(max_land, calc_land_need(occupants, yield_memory)),# land in hectares to support the household, but no more than the laborer can work
           new_land = if_else(land_need > land, land_need - land, 0)) %>%
    group_by(settlement) %>%
    mutate(available_land = area * arable * 100 - sum(land),
           total_land_need = sum(new_land)) %>%
    ungroup %>%
    mutate(land = if_else(total_land_need > available_land,
                          land + new_land / total_land_need * available_land,
                          land + new_land)) %>%
    select(-c(land_need, max_land, new_land, total_land_need, available_land))
}

calc_land_need <- function(occupants, yield, fallow = TRUE){
  wheat_req * occupants * (1 + seed_proportion) / yield * ifelse(fallow, 2, 1)
}

max_cultivable_land <- function(laborers, farming_labor, available_area, fallow = TRUE, type = 'asymptote'){
  potential_area <- max_labor * farming_labor * laborers * ifelse(fallow, 2, 1) / labor_per_hectare
  if(type == 'unlimited') return(potential_area)
  if(type == 'step') return(pmin(available_area, potential_area))
  if(type == 'asymptote') return(available_area * (1 - exp(-potential_area / available_area)))
}
# need to replace the above function with one that changes the value of labor based on the fraction available
# similar idea, but it requires a different implementation than above because we have multiple households competing
# for land here. right?
