#' Calcualte yields and whatnot
#'
#'Here households calculate how much land they need, pull the
#'crop yield from the environment, remember the yield, and
#'determine their harvests by multiplying the yield by the amount
#'of land they have (also removing some of the crop to save as
#'seed for next year).
#' @param households Tibble of household agents.
#' @export
#' @examples
#'

farm <- function(households, climatic_yield = calc_climatic_yield(1)){
  households %>%
    mutate(yield = climatic_yield,
           yield_memory = yield,
           harvest = land * yield * .5 - land * sowing_rate) # halve the yields to represent biennial fallow
}

eat <- function(households){
  households %>%
    mutate(total_cal_req = occupants * wheat_req,
           food_ratio = (storage + harvest) / total_cal_req,
           old_storage = storage,
           storage = if_else(total_cal_req <= storage, harvest, pmax(harvest - (total_cal_req - old_storage), 0))) %>%
    select(-old_storage, -total_cal_req, -harvest, -yield)
}

# farm <- function(households){
#   households %>%
#     mutate(yield = climatic_yield, #* n_inhabitants ^ labor_elasticity,
#            yield_memory = yield, #map2(yield_memory, yield, remember),
#            harvest = land * yield * .5 - land * sowing_rate) #%>%  # *.5 is for fallow
#   #select(-yield)
# }
#
#
# remember <- function(yield_memory, yield){
#   # rnorm(1, yield, yield * 0.0333) %>%  #memory is fuzzy
#   append(yield_memory[-length(yield_memory)], yield, after = 0) # remove the last entry in the vector and add new yield to the begining
# }
#
#
# #Agents use the peak-end rule when accessing memory.
# peak_end <- function(x){
#   map_dbl(x, ~mean(c(.x[1], min(.x))))
# }
