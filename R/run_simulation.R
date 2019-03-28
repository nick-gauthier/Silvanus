#' Run the simulation
#' We run the actual simulation using the *accumulate* function
#' from purrr. Under the hood its doing the same thing as a for loop,
#' but saving all the intervening steps for us to make plotting
#' and analysis easier later.
#'
#' @param input_data
#' @param nsim
#' @param replicates
#'
#' @export
#' @examples
#'
#'
#'have an option for 'save intermediate files', then make decide whether to use reduce or accumulate
#'


run_simulation <- function(input_data, nsim, replicates = 1, intermediate = TRUE){

  #if (replicates > 1) plan(multicore)
  #future_map(1:replicates, ~ . %>%
  #{if}

  if (intermediate) {
    accumulate(1:nsim, ~ household_dynamics(.x) %>%
               settlement_dynamics() %>%
               population_dynamics() %>%
               ecological_dynamics(),
             .init = input_data) %>%
      bind_rows(.id = 'simulation')
  } else {
    reduce(1:nsim, ~ household_dynamics(.x) %>%
                 settlement_dynamics() %>%
                 population_dynamics() %>%
                 ecological_dynamics(),
               .init = input_data) %>%
      bind_rows(.id = 'simulation')
  }


}


# This code (not run), presents an alternative parallelized approach for those with multiple cores
# library(parallel)
# run_simulation_par <- function(input_data, nsim, replicates = 1){
#   mclapply(1:replicates, function(x, input_data, nsim){
#     tibble(year = 1:nsim) %>%
#       mutate(data = accumulate(year, ~ household_dynamics(environmental_dynamics(.x)), .init = input_data)[-1],
#              population = map_dbl(data, ~sum(.$population))) %>%
#       select(-data)
#   }, input_data = input_data, nsim = nsim, mc.cores = detectCores() - 1) %>%
#     bind_rows(.id = 'simulation')
# }
