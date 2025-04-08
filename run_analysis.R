library(tidyverse)
library(tidymodels)
set.seed(0)

data_raw <- read_csv('./data/pima-indians-diabetes-data.csv')
data <- data_raw |> 
  mutate(
    class = factor(class, levels = c("1", "0"), labels = c("Yes", "No"))
  ) |> 
  rename(
    is_diabetic = class
  )

# Explore -----------------------------------------------------------------

clean <- function(data) {
  data |> 
    mutate(
      bmi = na_if(bmi, 0),
      diastolic_blood_pressure = na_if(diastolic_blood_pressure, 0),
      plasma_concentration = na_if(plasma_concentration, 0),
      serum_insulin = na_if(serum_insulin, 0),
      triceps_skinfold_thickness = na_if(triceps_skinfold_thickness, 0),
    )
}

visualise <- function(data) {
  data |> 
    pivot_longer(
      cols = 1:8,
      names_to = "measurement",
      values_to = "value",
    ) |> 
    ggplot(aes(x = value, fill = is_diabetic)) + 
    geom_histogram(bins = 50, position = "dodge") +
    facet_wrap(~measurement, nrow = 2, ncol = 4, scales = "free") +
    labs(
      title = "Distribution of Eight Health Measurements by Diabetes Status",
      x = "Value",
      y = "Count",
      fill = "Diabetic?",
    )
}

visualise(data)
ggsave("./plots/exploratory-1.png")
data <- clean(data)
visualise(data)
ggsave("./plots/exploratory-2.png")
