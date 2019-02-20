#' Household level time allocation
#'
#' Given knowledge of the irrigation system and simple heuristics
#' for relative returns to labor farming and maintaining
#' infrastructure, households solve a constrained optimization
#' problem [@david2015effect] to determine of the proportion of
#' available time they should devote to each activity so as to
#' maximize their expected utility.
#' @param households Tibble of household agents.
#' @export
#' @examples
#' allocate_time(households)


allocate_time <- function(households, total_labor = 1, j = 0.2, k = 0.6){
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
}
