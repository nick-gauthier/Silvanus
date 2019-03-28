#' Settlement dynamics
#'
#' @param net
#'
#' @return
#' @export
#'
#' @examples

settlement_dynamics <- function(settlements){
  settlements %N>%
    zoom_to('households') %>%
    migrate %>%
    zoom_to('individuals') %>%
    settlement_census
}

migrate <- function(settlements){
  settlements %N>%
    mutate(households = map(households, check_migrate),
           emmigrants = map(households, leave),
           households = map(households, ~filter(., migrated == FALSE) %>%
                              select(-migrated))) %>%
    interact %E>%
    group_by(from) %>%
    mutate(migrants_from = .N()$emmigrants[from],
           n_mig = map_dbl(migrants_from, nrow),
           out_mig = round_preserve_sum(n_mig * flow / sum(flow))) %>%
    ungroup %>%
    mutate(migrants_to = map2(migrants_from, out_mig, ~sample_n(.x, .y))) %N>%
    mutate(immigrants = future_map(settlement, ~bind_rows(.E()$migrants_to[.E()$to == as.numeric(.x)])),
           households = map2(households, immigrants, bind_households)) %E>%
    select(-c(flow, migrants_from, n_mig, out_mig, migrants_to)) %N>%
    select(-c(emmigrants, outflow, immigrants))
}

check_migrate <- function(households, migration_prob = 0.2){
  mutate(households,
         migrated = if_else(food_ratio < 1 &
                              laborers > 0 &
                              rbernoulli(n(), migration_prob),
                            TRUE, FALSE))
}

leave <- function(households){
  households %>%
    filter(migrated == TRUE) %>%
    select(-migrated) %>%
    mutate(household = NA, land = 0, storage = 0)
}

interact <- function(net, alpha = 1.15, beta = 0.07){
  net %E>%
    mutate(interaction_strength = .N()$population[to] ^ alpha * exp(-beta * distance)) %N>%
    mutate(outflow = centrality_degree(weights = interaction_strength, mode = 'out', loops = FALSE)) %E>%
    mutate(flow = distance / .N()$outflow[from]) %>%
    select(-interaction_strength)
}



# source https://www.r-bloggers.com/round-values-while-preserve-their-rounded-sum-in-r/
# original https://biostatmatt.com/archives/2902
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}

settlement_census <- function(settlements){
  settlements %N>%
  mutate(population = map_dbl(households, ~sum(.$occupants)),
         settled_area = 0.175 * population ^ 0.634,
         n_households = map_int(households, nrow))
}

# not currently used

interact2 <- function(net){
  net %E>%
    mutate(interaction_strength = .N()$attractiveness[to] ^ alpha * exp(-beta * log(distance)))  %N>%
    mutate(outflow = population / centrality_degree(weights = interaction_strength, mode = 'out', loops = FALSE)) %E>%
    mutate(flow = .N()$outflow[from] * interaction_strength) %N>%
    mutate(inflow = centrality_degree(weights = flow, mode = 'in', loops = F),
           attractiveness = attractiveness + .01 * (inflow  - attractiveness),
           population = population * pop_start * attractiveness / sum(attractiveness))
}

nystuen_dacey <- function(net){
  net %E>%
    group_by(from) %>%
    filter(flow == max(flow), .N()$population[from] < .N()$population[to]) %N>%
    mutate(terminal = node_is_sink()) %>%
    ungroup
}

# try using node_is_ functions to calculate instead
