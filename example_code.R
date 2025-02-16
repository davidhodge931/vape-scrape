library(tidyverse)

# Example character vector in a tibble
df <- tibble(buttons_text = c(
  "The concentration is 2.5 mg/ml and 30  mg/ml, but not 40ml or 50mg. Size: 116.4x16mm",
  "Another example: 10mg/ml, 15MG/ML, and 20mg/ML are valid, but 25ml is not.",
  "No valid concentrations here: 30ml, 40mg, 50mg/l."
))

# Extract numbers followed by "mg/ml"
# still incorrectly getting the 116 in the example above
df |>
  mutate(nicotine_num_buttons = str_extract_all(buttons_text, regex("[-+]?\\d*\\.\\d+|\\b\\d+\\b(?=\\s*mg/ml)", ignore_case = TRUE))) |> 
  mutate(nicotine_min_buttons = map_dbl(nicotine_num_buttons, \(x) min(as.numeric(x), na.rm = TRUE))) |>
  mutate(nicotine_max_buttons = map_dbl(nicotine_num_buttons, \(x) max(as.numeric(x), na.rm = TRUE))) 
