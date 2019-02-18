#' Infrastructure performance
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
#' @param maintainance_labor The proportion of total labor allocated to maintaining infrastructure.
#' @param max_irrigation Maximum irrigation, defaults to 1.
#' @param psi The proportion of a household's labor needed to keep irrigation infrastructure at half capacity, defaults to 0.2.
#' @param epsilon The scalability of irrigation infrastructure, defaults to 0.18.
#' @export
#' @examples
#' infrastructure_performance(maintainance_labor = 0.5)

infrastructure_performance <- function(maintainance_labor, psi = 0.2, epsilon = 0.18, max_irrigation = 1){
  ifelse(0 <= maintainance_labor & maintainance_labor < (psi - epsilon), 0,
         ifelse(between(maintainance_labor, psi - epsilon, psi + epsilon),
                max_irrigation / (2 * epsilon) * (maintainance_labor - psi + epsilon),
                max_irrigation))
}
