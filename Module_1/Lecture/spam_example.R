library(tidymodels)
library(openintro)

data(email)
email <- email

# using the standard predictive analytics/machine learning approach with the tidymodels framework 
data_split <- initial_split(email, strata = "spam", prop = 0.75)

training_set <- training(data_split)
test_set  <- testing(data_split)

email_recipe <- 
  recipe(
    spam ~ to_multiple + cc + image + attach + winner + password + line_breaks + format + re_subj + urgent_subj + exclaim_mess, 
    data = training_set
  ) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  prep(training = training_set)

logreg_fit <- 
  logistic_reg() %>%
  set_engine("glm") %>%
  fit(spam ~ ., data = bake(email_recipe, new_data = training_set))
logreg_fit

test_baked <- bake(email_recipe, new_data = test_set, all_predictors())

test_results <- 
  test_set %>%
  select(spam) %>%
  bind_cols(
    predict(logreg_fit, new_data = test_baked) %>%
      rename(logreg_pred = .pred_class)
  )

test_results %>% metrics(truth = spam, estimate = logreg_pred) 
