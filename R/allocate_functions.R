#' Household level time allocation
#'
#' Given knowledge of the irrigation system and simple heuristics
#' for relative returns to labor spent farming and maintaining
#' infrastructure, households solve a constrained optimization
#' problem [@david2015effect] to determine the proportion of
#' available time they should devote to each activity so as to
#' maximize their expected utility.
#'
#' #' Infrastructure performance
#'
#' This function calculates the performance of irrigation
#' infrastructure, given an amount of maintainance labor and
#' parameters controling the scalability of the infrastructure.
#' The performance of infrastructure is a piecewise linear
#' function of labor inputs. Two parameters
#' \eqn{\psi} and \eqn{\epsilon} determine how much labor is required to
#' keep irrigation infrastructure working at maximum capacity.
#' By default, \eqn{\psi \approx \epsilon} to make the
#' infrastructure scalable, that is the agents can spend more
#' or less time maintaining infrastructure and still be assured
#' of at least some water. This equation is derived from [@david2015effect].
#' @param total_labor
#' @param j
#' @param k
#' @param precipitation
#' @param households Tibble of household agents.
#' @param psi
#' @param epsilon
#' @param maintainance_labor The proportion of total labor allocated to maintaining infrastructure.
#' @param max_irrigation Maximum irrigation, defaults to 1.
#' @param psi The proportion of a household's labor needed to keep irrigation infrastructure at half capacity, defaults to 0.2.
#' @param epsilon The scalability of irrigation infrastructure, defaults to 0.18.
#' @export
#' @examples
#' infrastructure_performance(maintainance_labor = 0.5)
allocate_time <- function(households, total_labor = 1, j = 0.2, k = 0.6, psi = 0.2, epsilon = 0.18){
  households %>%   #calculate optimum values for the different regions of the step function
    mutate(r1_maintainance = 0,
           r1_utility = yield_memory * land ^ (1 - j - k) * total_labor ^ j * precipitation ^ k,
           r3_maintainance = psi + epsilon,
           r2_maintainance = pmin(pmax((1 / (j + k)) * (k * total_labor + j * (psi - epsilon) - 2 * j * epsilon * precipitation / runoff),
                                       0), r3_maintainance),
           r2_utility = yield_memory * land ^ (1 - j - k) * (total_labor - r2_maintainance) ^ j * (runoff / (2 * epsilon) * (r2_maintainance - psi + epsilon) + precipitation) ^ k,
           r3_utlity = yield_memory * land ^ (1 - j - k) * (total_labor - psi - epsilon) ^ j * (runoff + precipitation) ^ k,
           max_utility = pmax(r1_utility, r2_utility, r3_utlity),
           farming_labor = if_else(max_utility == r3_utlity, 1 - r3_maintainance,
                                   if_else(max_utility == r2_utility, 1 - r2_maintainance, 1 - r1_maintainance))) %>%
    select(-(r1_maintainance:max_utility))  # remove all the temporary columns
  # should simplify with case when or something
}

allocate_land <- function(households){
  households %>%
    mutate(max_land = max_cultivable_land(laborers, farming_labor, cultivable_area * 100, type = 'unlimited'),
           land_need = pmin(max_land, calc_land_need(occupants, yield_memory)), # land in hectares to support the household, but no more than the laborer can work
           new_land = if_else(land_need > land, land_need - land, 0)) %>%
    group_by(settlement) %>%
    mutate(available_land = cultivable_area * 100 - sum(land),
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

# need to replace this function with one that changes the value of labor based on the fraction available
# similar idea, but it requires a different implementation than above because we have multiple households competing
# for land here. right?
max_cultivable_land <- function(laborers, farming_labor, available_area, fallow = TRUE, type = 'asymptote'){
  potential_area <- max_labor * farming_labor * laborers * ifelse(fallow, 2, 1) / labor_per_hectare
  if(type == 'unlimited') return(potential_area)
  if(type == 'step') return(pmin(available_area, potential_area))
  if(type == 'asymptote') return(available_area * (1 - exp(-potential_area / available_area)))
}



infrastructure_performance <- function(maintainance_labor, psi = 0.2, epsilon = 0.18, max_irrigation = 1){
  ifelse(0 <= maintainance_labor & maintainance_labor < (psi - epsilon), 0,
         ifelse(between(maintainance_labor, psi - epsilon, psi + epsilon),
                max_irrigation / (2 * epsilon) * (maintainance_labor - psi + epsilon),
                max_irrigation))
}

