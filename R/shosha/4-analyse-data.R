library(tidyverse)

shosha <- read_csv("data/2024-11-28 08-55-55/shosha.csv")

shosha |>
  group_by(category) |> 
  count()

flavour_keywords <- glue::glue_collapse(c(
  "Flavor Profile:|Flavor profile:|flavor profile:", 
  "Flavors Profile:|Flavors profile:|flavors profile:", 
  "Flavour Profile:|Flavour profile:|flavour profile:",
  "Flavours Profile:|Flavours profile:|flavours profile:", 
  "Flavor:|flavor:", 
  "Flavour:|flavour:" 
), sep = "|")

#doesn't work
shosha |>
  mutate(
    flavour = details |>
      str_subset(flavour_keywords) |>
      str_remove(flavour_keywords) |>
      str_trim()
  )

nicotine_keywords <- glue::glue_collapse(c(
  "Nicotine Concentration:|Nicotine concentration:|nicotine concentration:",
  "Nicotine Concentration:|Nicotine concentration:|nicotine concentration:", 
  "Nicotine Strength:|Nicotine strength:|nicotine strength:", 
  "Nicotine Strength:|Nicotine strength:|nicotine strength:",
  "Nicotine:|nicotine:"
), sep = "|")

#doesn't work
shosha |>
  mutate(
    nicotine = details |>
      str_subset(nicotine_keywords) |>
      str_remove(nicotine_keywords) |>
      str_trim()
  )

