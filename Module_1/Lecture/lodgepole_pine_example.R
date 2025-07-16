library(tidymodels)
library(ggplot2)

data <- read.csv("lab_data.csv", header = TRUE)
data$lodgepole_pine <- factor(data$lodgepole_pine)

# using the standard predictive analytics/machine learning approach with the tidymodels framework 
data_split <- initial_split(data, strata = "lodgepole_pine", prop = 0.75)

training_set <- training(data_split)
test_set  <- testing(data_split)

pine_recipe <- 
  recipe(
    lodgepole_pine ~ elevation + aspect + slope + horizontal_distance_to_hydrology +
      vertical_distance_to_hydrology + horizontal_distance_to_roadways + hillshade_9am + hillshade_noon + 
      hillshade_3pm + horizontal_distance_to_fire_points + wilderness_area + soil_type, 
    data = training_set
  ) %>%
  step_other(soil_type) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  prep(training = training_set)

logreg_fit <- 
  logistic_reg() %>%
  set_engine("glm") %>%
  fit(lodgepole_pine ~ ., data = bake(pine_recipe, new_data = training_set))
logreg_fit

test_baked <- bake(pine_recipe, new_data = test_set, all_predictors())

test_results <- 
  test_set %>%
  select(lodgepole_pine) %>%
  bind_cols(
    predict(logreg_fit, new_data = test_baked) %>%
      rename(logreg_pred = .pred_class)
  )

test_results %>% metrics(truth = lodgepole_pine, estimate = logreg_pred) 
