birth_death <- function(households){
  households %>%
    reproduce %>%
    die %>%
    mutate(n_occupants = map_int(occupants, nrow),
           laborers = map_dbl(occupants, ~filter(.x, between(age, 15, 65)) %>% nrow))
}
