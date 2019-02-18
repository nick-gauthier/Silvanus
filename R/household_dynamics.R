# this function takes a settlement (tibble of households), and makes the households do what they need to do
household_dynamics <- function(settlements){
  unnest(settlements) %>%
    allocate_time %>%
    allocate_land %>%
    farm %>%
    eat %>%
    birth_death %>%
    nest(household:last(everything()), .key = households) %>%
    mutate(population = map_dbl(households, ~ sum(.$n_occupants)))
}
