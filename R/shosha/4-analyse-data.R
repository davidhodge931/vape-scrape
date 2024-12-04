library(tidyverse)

date <- "2024-12-03 10-45-13"
shosha <- read_csv(glue::glue("data/{date}/shosha.csv"))

shosha
shosha |> count()
shosha |> group_by(category) |> count()

# str_extract(test, "(?i)(Flavor Profile:|Flavour Profile:|Flavor:|Flavour:)(.*?)(?=\n)")
# str_extract(test, "(?i)(VG/PG:|PG/VG:|VG/PG ratio:|PG/VG ratio:)[\\s]*(\\d{1,2}/\\d{1,2})")
pattern <- "(?i)(Flavor Profile:|Flavour Profile:|Flavor:|Flavour:)(.*?)(?=\n|\\|)"

d <- shosha |> 
  mutate(details2 = details) |> 
  mutate(details2 = str_remove(details2, "GET FREE SHIPPING.*")) |> 
  mutate(flavour = str_extract(details2, "(?i)(Flavor Profile:|Flavour Profile:|Flavor:|Flavour:|Flavors Profile:|Flavours Profile:|Flavors:|Flavours:)(.*?)(?=\n|\\|)")) |> 
  mutate(details2 = str_remove(details2, "(?i)(Flavor Profile:|Flavour Profile:|Flavor:|Flavour:|Flavors Profile:|Flavours Profile:|Flavors:|Flavours:)(.*?)(?=\n|\\|)")) |>
  
  mutate(size = str_extract(details2, "(Size:.*?)(?=\\n|\\|)")) |> 
  mutate(size = str_remove(size, ", Made in .*")) |> 
  mutate(details2 = str_remove(details2, "(Size:.*?)(?=\\n|\\|)")) |>
  
  mutate(vgpg = str_extract(details2, "(?i)(VG/PG:|PG/VG:|VG/PG ratio:|PG/VG ratio:)[\\s]*(\\d{1,2}/\\d{1,2})")) |>
  mutate(vgpg2 = purrr::map(str_extract_all(details, "\\b\\d+/\\d+\\b"), unique)) |>
  
  
  mutate(details2 = str_remove(details2, "(?i)(VG/PG:|PG/VG:|VG/PG ratio:|PG/VG ratio:.*?)(?=\\n|\\|)")) |> 
  mutate(nicotine = str_extract(details2, "(?i)(Nicotine\\s+(concentration|strength|salt\\sstrength):[^\\n]*)")) |> 
  mutate(nicotine = str_remove(nicotine, "(?i)(Caution:.*)")) |> 
  mutate(nicotine = str_remove(nicotine, "(?i)(Specifications:.*)")) |>
  mutate(nicotine = str_remove(nicotine, "(?i)(Pod Capacity:.*)")) |> 
  mutate(nicotine = str_extract(nicotine, "^[^|]+")) |> 
  select(-details2) |> 
  relocate(details, .after = nicotine) |> 
  mutate(size = str_remove(size, "Size: ")) |> 
  mutate(flavour = str_remove(flavour, "(?i)(Flavor Profile:|Flavour Profile:|Flavor:|Flavour:|Flavors Profile:|Flavours Profile:|Flavors:|Flavours:)")) |>
  mutate(flavour = str_remove(flavour, "\\.")) |> 
  mutate(vgpg = str_remove(vgpg, "(?i)VG/PG:|PG/VG:|VG/PG ratio:|PG/VG ratio:")) |> 
  mutate(nicotine = str_remove(nicotine, "\\(.*")) |>
  mutate(nicotine = str_replace_all(nicotine, "mg/mL", "mg/ml")) |>  
  mutate(nicotine_type = word(nicotine, 1, sep = ": ")) |> 
  mutate(nicotine_values = word(nicotine, -1, sep = ": ")) |> 
  mutate(nicotine_num = nicotine_values |> str_extract_all("\\d+(\\.\\d+)?(?=mg/ml)") %>% map(\(x) as.double(x))) |>
  mutate(nicotine_min = map_dbl(nicotine_num, min)) |>
  mutate(nicotine_max = map_dbl(nicotine_num, max)) |>
  select(-nicotine_num, -nicotine_values) |> 
  mutate(across(everything(), str_trim)) |> 
  relocate(details, .after = nicotine_max) |> 
  filter(name != "about-us")

d |> filter(!is.na(size)) #491 products
d |> filter(!is.na(vgpg)) #313 products
d |> filter(!is.na(flavour)) #355 products
d |> filter(!is.na(flavour2)) #355 products
d |> filter(!is.na(nicotine)) #307 products
d |> View() #313 products

d |> 
  filter(!is.na(vgpg)) |> 
  mutate(vgpg2 = purrr::map(vgpg2, unique)) |> 
  View() #313 products

m
utate(char_vector = map(char_vector, unique))

d |> filter(is.na(vgpg)) |> View() #313 products

d |> filter(!is.na(flavour)) |> View()

# d |> View()
d |> filter(!is.na(flavour)) |> View() #356 products

d |> filter(is.na(flavour)) |> View() #240 products

#NEED to adjust code to invlude Vg/PG etc
d |> 
  select(vgpg, details) |> 
  filter(is.na(vgpg)) |> 
  View()#224 products
