



max_yield <- 1000 # baseline wheat yield, in kg/ha, should range from 1000 - 2000

calorie_req <- 2582 * 365  # annual individual calorie requirement
wheat_calories <- 3320 # calories in a kg of wheat
wheat_calorie_prop <- 1 # percent of individual's food calories coming from wheat
wheat_req <- calorie_req / wheat_calories * wheat_calorie_prop # kg of wheat to feed a person for 1 year

sowing_rate <- 135  # kg of wheat to sow a hectare (range 108 - 135 from Roman agronomists)
seed_proportion <- 135 / max_yield # proportion of harvest to save as seed for next year's sowing

labor_per_hectare <- 40 # person days required to farm a hectare
max_labor <- 280 # maximum days per year an individual can devote to farming



# probability of a female giving birth at each age
# all agent's are "female", which means these values are halved at runtime
fertility_table <- tibble(
  age = 10:49,
  fertility_rate = rep(c(0.022, 0.232, 0.343, 0.367, 0.293, 0.218, 0.216, 0.134), each = 5)
) %>%
  .[-1:-2,] # 10 and 11 year olds can't give birth

# probability of an individual dying at each age
mortality_table <- tibble(
  age = 0:99,
  mortality_rate = c(0.381, rep(0.063, 4), rep(c(0.013, 0.010, 0.013, 0.016, 0.018, 0.020, 0.022, 0.024, 0.025, 0.033, 0.042, 0.062, 0.084, 0.122, 0.175, 0.254, 0.376, 0.552, 0.816), each = 5))
)
