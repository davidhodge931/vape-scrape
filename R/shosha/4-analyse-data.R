library(tidyverse)

date <- "2024-12-03 10-45-13"
shosha <- read_csv(glue::glue("data/{date}/shosha.csv"))

shosha
shosha |> count()
shosha |> group_by(category) |> count()

d <- shosha |> 
  mutate(details2 = details) |> 
  mutate(details2 = str_remove(details2, "GET FREE SHIPPING.*")) |> 
  mutate(flavour = str_extract(details2, "(?i)(Flavor\\s+Profile:.*?)(?=\\n|\\|)")) |> 
  mutate(details2 = str_remove(details2, "(?i)(Flavor\\s+Profile:.*?)(?=\\n|\\|)")) |> 
  mutate(size = str_extract(details2, "(Size:.*?)(?=\\n|\\|)")) |> 
  mutate(size = str_remove(size, ", Made in .*")) |> 
  mutate(details2 = str_remove(details2, "(Size:.*?)(?=\\n|\\|)")) |>
  mutate(vgpg = str_extract(details2, "(?i)(VG/PG:.*?)(?=\\n|\\|)")) |> 
  mutate(details2 = str_remove(details2, "(?i)(VG/PG:.*?)(?=\\n|\\|)")) |> 
  mutate(nicotine = str_extract(details2, "(?i)(Nicotine\\s+(concentration|strength|salt\\sstrength):[^\\n]*)")) |> 
  mutate(nicotine = str_remove(nicotine, "(?i)(Caution:.*)")) |> 
  mutate(nicotine = str_remove(nicotine, "(?i)(Specifications:.*)")) |>
  mutate(nicotine = str_remove(nicotine, "(?i)(Pod Capacity:.*)")) |> 
  mutate(nicotine = str_extract(nicotine, "^[^|]+")) |> 
  select(-details2) |> 
  relocate(details, .after = nicotine) |> 
  mutate(across(everything(), str_trim)) |> 
  filter(!is.na(flavour)) |> 
  mutate(size = str_remove(size, "Size: ")) |> 
  mutate(flavour = str_remove(flavour, "(?i)flavor profile: ")) |> 
  mutate(flavour = str_remove(flavour, "(?i)flavour profile: ")) |> 
  mutate(vgpg = str_remove(vgpg, "(?i)vgpg")) |> 
  mutate(nicotine = str_remove(nicotine, "\\(.*")) |>
  mutate(nicotine = str_replace_all(nicotine, "mg/mL", "mg/ml")) |>  
  mutate(nicotine_type = word(nicotine, 1, sep = ": ")) |> 
  mutate(nicotine_values = word(nicotine, -1, sep = ": ")) |> 
  mutate(nicotine_num = nicotine_values |> str_extract_all("\\d+(\\.\\d+)?(?=mg/ml)") %>% map(\(x) as.double(x))) |>
  mutate(nicotine_min = map_dbl(nicotine_num, min)) |>
  mutate(nicotine_max = map_dbl(nicotine_num, max)) |>
  select(-nicotine_num) |> 
  relocate(details, .after = nicotine_max) 

d |> filter(!is.na(size)) #491 products
d |> filter(!is.na(vgpg)) #294 products
d |> filter(!is.na(flavour)) #240 products
d |> filter(!is.na(nicotine)) #307 products


