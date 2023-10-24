library(tidymodels)

# Helper packages
library(readr)       # for importing data
library(broom.mixed) # for converting bayesian models to tidy tibbles

urchins <-
  # Data were assembled for a tutorial
  # at https://www.flutterbys.com.au/stats/tut/tut7.5a.html
  read_csv("https://tidymodels.org/start/models/urchins.csv") %>%
  # Change the names to be a little more verbose
  setNames(c("food_regime", "initial_volume", "width")) %>%
  # Factors are very helpful for modeling, so we convert one column
  mutate(food_regime = factor(food_regime, levels = c("Initial", "Low", "High")))


ggplot(urchins,
       aes(x = initial_volume,
           y = width,
           group = food_regime,
           col = food_regime)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  scale_color_viridis_d(option = "plasma", end = .7) +
  facet_wrap(~food_regime)
#> `geom_smooth()` using formula 'y ~ x'

lm(data = urchins, width ~ initial_volume * food_regime)


# functional form of the model
lm_model <- linear_reg() %>%
  set_engine("lm")

lm_fit <- lm_model %>%
  fit(width ~ initial_volume * food_regime, data = urchins)

new_points <- crossing(initial_volume = 20, food_regime = unique(urchins$food_regime)) %>%
  bind_cols(predict(lm_fit, new_data = .)) %>%
  bind_cols(predict(lm_fit, new_data = new_points, type = "conf_int"))


ggplot(new_points) +
  geom_errorbar(aes(x = food_regime, ymin = .pred_lower, ymax = .pred_upper),
                width = .2) +
  geom_point(aes(x = food_regime, y = .pred))




# set the prior distribution
prior_dist <- rstanarm::student_t(df = 1)

set.seed(123)
# make the parsnip model
bayes_mod <-
  linear_reg() %>%
  set_engine("stan",
             prior_intercept = prior_dist,
             prior = prior_dist)

library(tidymodels)
# Helper packages
library(nycflights13)    # for flight data
library(skimr)           # for variable summaries

set.seed(123)

flight_data <-
  flights %>%
  mutate(
    # Convert the arrival delay to a factor
    arr_delay = ifelse(arr_delay >= 30, "late", "on_time"),
    arr_delay = factor(arr_delay),
    # We will use the date (not date-time) in the recipe below
    date = as.Date(time_hour)
  ) %>%
  # Include the weather data
  inner_join(weather, by = c("origin", "time_hour")) %>%
  # Only retain the specific columns we will use
  select(dep_time, flight, origin, dest, air_time, distance,
         carrier, date, arr_delay, time_hour) %>%
  # Exclude missing data
  na.omit() %>%
  # For creating models, it is better to have qualitative columns
  # encoded as factors (instead of character strings)
  mutate_if(is.character, as.factor)


# Fix the random numbers by setting the seed
# This enables the analysis to be reproducible when random numbers are used
set.seed(555)
# Put 3/4 of the data into the training set
data_split <- initial_split(flight_data, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

# Make our recipe

flights_rec <- recipe(arr_delay ~ ., data = train_data) %>%
  update_role(flight, time_hour, new_role = "ID") %>%
  step_date(date, features = c("dow", "month")) %>%
  step_holiday(date, holidays = timeDate::listHolidays("US")) %>%
  step_rm(date) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors())


lr_model <- logistic_reg() %>%
  set_engine("glm")

flights_workflow <- workflow() %>%
  add_model(lr_model) %>%
  add_recipe(flights_rec)

flights_fit <- flights_workflow %>% fit(data = train_data)


flights_fit %>%
  pull_workflow_fit() %>%
  tidy()

predict(flights_fit, test_data)

flights_pred <-
  predict(flights_fit, test_data, type = "prob") %>%
  bind_cols(test_data %>% select(arr_delay, time_hour, flight))

flights_pred %>%
  ggplot(aes(x = .pred_on_time, y = arr_delay)) +
  geom_jitter(alpha = .01)

flights_pred %>%
  roc_curve(truth = arr_delay, .pred_late) %>%
  autoplot()
