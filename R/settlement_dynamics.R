#' Settlement dynamics
#'
#' @param net
#'
#' @return
#' @export
#'
#' @examples

interact <- function(net, alpha = 1.15, beta = 0.5){
  net %E>%
    mutate(interaction_strength = .N()$population[to] ^ alpha * exp(-beta * log(distance))) %N>%
    mutate(outflow = centrality_degree(weights = interaction_strength, mode = 'out', loops = FALSE)) %E>%
    mutate(flow = distance / .N()$outflow[from])
}

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
