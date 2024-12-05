library(tidyverse)

date <- "2024-12-03 10-45-13"
date <- "2024-12-05 16-05-15"

latest_run <- fs::dir_ls(fs::path("shosha", "data-scraped")) |>
  as_tibble() |>
  mutate(value = fs::path_sanitize(str_remove(value, fs::path("shosha", "data-scraped") ))) |>
  slice_max(value) |>
  pull()

shosha <- read_csv(fs::path("shosha", "data-scraped", latest_run, "data-scraped", ext = "csv"))

shosha
shosha |> count()
shosha |> group_by(category) |> count()

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
  mutate(size = str_replace_all(size, "mL", "ml")) |> 
  mutate(size = str_replace(size, "/", ", ")) |>   

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

# d |> 
#   filter(str_detect(category, "E-Liquids")) |> 
#   View()

d |> 
  filter(str_detect(category, "E-Liquids")) |> 
  mutate(nicotine_na = is.na(nicotine)) |> 
  group_by(nicotine_na) |> 
  count()

d |> 
  filter(str_detect(category, "E-Liquids")) |> 
  mutate(flavour_na = is.na(flavour)) |> 
  group_by(flavour_na) |> 
  count()

d |> 
  filter(str_detect(category, "E-Liquids")) |> 
  mutate(size_na = is.na(size)) |> 
  group_by(size_na) |> 
  count()

###
d |> 
  filter(str_detect(category, "E-Liquids")) |> 
  mutate(nicotine_na = is.na(nicotine)) |> 
  filter(nicotine_na) 

d |> 
  filter(str_detect(category, "E-Liquids")) |> 
  mutate(flavour_na = is.na(flavour)) |> 
  filter(flavour_na)

d |> 
  filter(str_detect(category, "E-Liquids")) |> 
  mutate(size_na = is.na(size)) |> 
  filter(size_na) 
