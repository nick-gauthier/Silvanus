eat <- function(households){
  households %>%
    mutate(total_cal_req = n_occupants * wheat_req,
           food_ratio = pmin(1, (storage + harvest) / total_cal_req),
           old.storage = storage,
           storage = if_else(total_cal_req <= storage, harvest, pmax(harvest - (total_cal_req - old.storage), 0))) %>%
    select(-old.storage, -total_cal_req, -harvest)
}
