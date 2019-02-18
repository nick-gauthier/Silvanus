die <- function(households){
  households %>%
    unnest(occupants) %>%
    inner_join(mortality_table, by = 'age') %>% # inner join has the effect of killing off all those over 99
    mutate(survive = rbernoulli(n(), (1 - mortality_rate))) %>%
    filter(survive == T) %>%
    mutate(age = age + 1) %>% # happy birthday!
    select(-survive, -mortality_rate) %>%
    nest(age, .key = occupants)
}
