library(tidyverse)

date <- "2024-12-03 10-45-13"
shosha <- read_csv(glue::glue("data/{date}/shosha.csv"))

shosha
shosha |> count()
shosha |> group_by(category) |> count()

shosha |> 
  mutate(nicotine = str_extract(details, pattern)) |> 
  filter(!is.na(nicotine))

nicotine_text <- "(?i)(Nicotine Concentration:|Nicotine concentration:|
nicotine concentration:|Nicotine Strength:|Nicotine strength:|nicotine strength:|
Nicotine salt strength:|Nicotine Salt Strength:|nicotine salt strength:|
Nicotine:|nicotine)\\s?\\d+(\\.\\d+)?\\s?mg/ml\\s?[-/]?\\s?\\d+(\\.\\d+)?\\s?mg/ml"

first_number <- "(?i)(?<=\\s|^)(\\d+(\\.\\d+)?)\\s?mg/ml"

shosha |> 
  mutate(nicotine = str_extract(details, pattern)) |> 
  filter(!is.na(nicotine)) |> 
  mutate(nicotine_lower = as.numeric(str_remove(str_extract(nicotine, first_number), "mg/ml|mg/mL")))  |> 
  mutate(nicotine2 = str_remove(nicotine, first_number)) |> 
  mutate(nicotine_upper = as.numeric(str_remove(str_extract(nicotine2, first_number), "mg/ml|mg/mL")))  |>
  select(-nicotine2) |> 
  arrange(desc(nicotine_upper))
  
