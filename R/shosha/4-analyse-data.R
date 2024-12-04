library(tidyverse)

date <- "2024-12-03 10-45-13"
shosha <- read_csv(glue::glue("data/{date}/shosha.csv"))

shosha
shosha |> count()
shosha |> group_by(category) |> count()

# str_extract(test, "(?i)(Flavor Profile:|Flavour Profile:|Flavor:|Flavour:)(.*?)(?=\n)")
# str_extract(test, "(?i)(VG/PG:|PG/VG:|VG/PG ratio:|PG/VG ratio:)[\\s]*(\\d{1,2}/\\d{1,2})")
# pattern <- "(?i)(Flavor Profile:|Flavour Profile:|Flavor:|Flavour:)(.*?)(?=\n|\\|)"

pattern <- "(?i)(VG/PG:|PG/VG:|VG/PG ratio:|PG/VG ratio:)(\\s*\\d+/\\d+)"

# Extract the matches
matches <- str_extract_all(text, pattern)

# Clean up the output by removing the initial matching phrase (e.g., "VG/PG:", "PG/VG ratio:", etc.)
matches_clean <- str_remove_all(matches[[1]], "(?i)(VG/PG:|PG/VG:|VG/PG ratio:|PG/VG ratio:)", "")

d <- shosha |> 
  mutate(details2 = details) |> 
  mutate(details2 = str_remove(details2, "(?i)(GET FREE SHIPPING).*")) |> 
  
  #flavour
  mutate(flavour = str_extract(details2, "(?i)(Flavor Profile:|Flavour Profile:|Flavor:|Flavour:|Flavors Profile:|Flavours Profile:|Flavors:|Flavours:)(.*?)(?=\n|\\|)")) |> 
  mutate(flavour = str_remove(flavour, "(?i)(Flavor Profile:|Flavour Profile:|Flavor:|Flavour:|Flavors Profile:|Flavours Profile:|Flavors:|Flavours:)")) |>
  mutate(flavour = str_remove(flavour, "\\.")) |> 
  mutate(details2 = str_remove(details2, "(?i)(Flavor Profile:|Flavour Profile:|Flavor:|Flavour:|Flavors Profile:|Flavours Profile:|Flavors:|Flavours:)(.*?)(?=\n|\\|)")) |>
  
  #size
  mutate(size = str_extract(details2, "(Size:.*?)(?=\\n|\\|)")) |> 
  mutate(size = str_remove(size, ", Made in .*")) |>
  mutate(size = str_remove(size, "Size: ")) |> 
  mutate(details2 = str_remove(details2, "(Size:.*?)(?=\\n|\\|)")) |>
  
  #vgpg
  # mutate(vgpg = str_extract(details2, "(?i)(VG/PG:|PG/VG:|VG/PG ratio:|PG/VG ratio:)[\\s]*(\\d{1,2}/\\d{1,2})")) |> 
  # mutate(vgpg = str_remove(vgpg, "(?i)VG/PG:|PG/VG:|VG/PG ratio:|PG/VG ratio:")) |> 
  mutate(vgpg = purrr::map(str_extract_all(details, "\\b\\d+/\\d+\\b"), unique)) |> 
  mutate(vgpg = ifelse(vgpg == "character(0)", NA_character_,  vgpg)) |> 
  mutate(vgpg = str_remove_all(vgpg, 'c\\(|\\)|"')) |> 
  mutate(details2 = str_remove(details2, "(?i)(VG/PG:|PG/VG:|VG/PG ratio:|PG/VG ratio:.*?)(?=\\n|\\|)")) |>

  #nicotine
  mutate(nicotine = str_extract(details2, "(?i)(Nicotine\\s+(concentration|strength|salt\\sstrength):[^\\n]*)")) |> 
  mutate(nicotine = str_remove(nicotine, "(?i)(Caution:.*)")) |> 
  mutate(nicotine = str_remove(nicotine, "(?i)(Specifications:.*)")) |>
  mutate(nicotine = str_remove(nicotine, "(?i)(Pod Capacity:.*)")) |> 
  mutate(nicotine = str_extract(nicotine, "^[^|]+")) |> 
  select(-details2) |> 
  relocate(details, .after = nicotine) |> 
  mutate(nicotine = str_remove(nicotine, "\\(.*")) |>
  mutate(nicotine = str_replace_all(nicotine, "mg/mL", "mg/ml")) |>  
  mutate(nicotine_type = word(nicotine, 1, sep = ": ")) |> 
  mutate(nicotine_values = word(nicotine, -1, sep = ": ")) |> 
  mutate(nicotine_num = nicotine_values |> str_extract_all("\\d+(\\.\\d+)?(?=mg/ml)") %>% map(\(x) as.double(x))) |>
  mutate(nicotine_min = map_dbl(nicotine_num, min)) |>
  mutate(nicotine_max = map_dbl(nicotine_num, max)) |>
  select(-nicotine_num, -nicotine_values) |> 
  
  #clean-up
  mutate(across(everything(), str_trim)) |> 
  relocate(details, .after = nicotine_max) |> 
  filter(name != "about-us")

d |> filter(!is.na(size)) #491 products
d |> filter(!is.na(vgpg)) #313 products. Some extra irrelevant ratios
d |> filter(!is.na(flavour)) #355 products
d |> filter(!is.na(nicotine)) #307 products
