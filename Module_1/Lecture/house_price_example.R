library(tidymodels)

data(ames)
plot(ames$Gr_Liv_Area, ames$Sale_Price)

# using the standard predictive analytics/machine learning approach with the tidymodels framework 
data_split <- initial_split(ames, strata = "Sale_Price", prop = 0.75)

training_set <- training(data_split)
test_set  <- testing(data_split)

ames_recipe <- 
  recipe(
    Sale_Price ~ Longitude + Latitude + Lot_Area + Neighborhood + Year_Sold + Gr_Liv_Area, 
    data = training_set
  ) %>%
  step_other(Neighborhood) %>%
  step_dummy(all_nominal()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  prep(training = training_set)

lm_fit <- 
  linear_reg() %>% 
  set_engine("lm") %>%
  fit(Sale_Price ~ ., data = bake(ames_recipe, new_data = training_set))
lm_fit

test_baked <- bake(ames_recipe, new_data = test_set, all_predictors())

test_results <- 
  test_set %>%
  select(Sale_Price) %>%
  bind_cols(
    predict(lm_fit, new_data = test_baked) %>%
      rename(lm_pred = .pred)
  )

test_results %>% metrics(truth = Sale_Price, estimate = lm_pred) 

test_results %>% 
  ggplot(aes(x = lm_pred, y = Sale_Price)) + 
  geom_abline(col = "green", lty = 2) + 
  geom_point(alpha = .4) + 
  coord_fixed()
