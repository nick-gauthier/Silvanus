



max_yield <- 1000 # theoretical maximum wheat yield, in kg/ha, should range from 1000 - 2000

calorie_req <- 2785 * 365  # annual individual calorie requirement, alternatively 2582
wheat_calories <- 3320 # calories in a kg of wheat
wheat_calorie_prop <- 1 # percent of individual's food calories coming from wheat
wheat_req <- calorie_req / wheat_calories * wheat_calorie_prop # kg of wheat to feed a person for 1 year

sowing_rate <- 135  # kg of wheat to sow a hectare (range 108 - 135 from Roman agronomists)
seed_proportion <- 135 / max_yield # proportion of harvest to save as seed for next year's sowing

tax <- 0.15 # proportion of yields consumed for taxation (typically 0 - 0.3)

labor_per_hectare <- 80 # person days required to farm a hectare (should range from ~ 50 to 135 for manual tilling, sowing, harvesting, and storing, from Halstead 2014)
max_labor <- 280 # maximum days per year an individual can devote to farming


# fertility_table <- tibble(
#   age = 0:119,
#   fertility_rate = c(rep(0, 12), 0.022, 0.022, 0.022, rep(c(0.232, 0.343, 0.367, 0.293, 0.218, 0.216, 0.134), each = 5), rep(0, 70))
# )
#
# mortality_table <- tibble(
#   age = 0:119,
#   mortality_rate = c(0.381, rep(0.063, 4), rep(c(0.013, 0.010, 0.013, 0.016, 0.018, 0.020, 0.022, 0.024, 0.025, 0.033, 0.042, 0.062, 0.084, 0.122, 0.175, 0.254, 0.376, 0.552, 0.816, 1, 1, 1, 1), each = 5))
# )

fertility_shape <- 4.579627
fertility_scale <- 0.1
survivor_scale <- 0.1
