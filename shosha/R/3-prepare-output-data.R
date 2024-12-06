library(tidyverse)

latest_run <- fs::dir_ls(fs::path("shosha", "data")) |>
  as_tibble() |>
  mutate(value = fs::path_sanitize(str_remove(value, fs::path("shosha", "data") ))) |>
  slice_max(value) |>
  pull()

shosha <- read_csv(fs::path("shosha", "data", latest_run, "scraped", ext = "csv"))

shosha
shosha |> count()
shosha |> group_by(category) |> count()

d <- shosha |> 
  mutate(details2 = details) |> 
  mutate(details2 = str_remove(details2, "(?i)(GET FREE SHIPPING).*")) |> 
  mutate(details2 = str_replace_all(details2, regex("mg/ml", ignore_case = TRUE), "mg/ml")) |> 
  
  #flavour
  mutate(details_flavour = str_extract(details2, "(?i)(Flavor Profile:|Flavour Profile:|Flavor:|Flavour:|Flavors Profile:|Flavours Profile:|Flavors:|Flavours:)(.*?)(?=\n|\\|)")) |> 
  mutate(details_flavour = str_remove(details_flavour, "(?i)(Flavor Profile:|Flavour Profile:|Flavor:|Flavour:|Flavors Profile:|Flavours Profile:|Flavors:|Flavours:)")) |>
  mutate(details_flavour = str_remove(details_flavour, "\\.")) |> 
  mutate(details2 = str_remove(details2, "(?i)(Flavor Profile:|Flavour Profile:|Flavor:|Flavour:|Flavors Profile:|Flavours Profile:|Flavors:|Flavours:)(.*?)(?=\n|\\|)")) |>
  
  #size
  mutate(details_size = str_extract(details2, "(Size:.*?)(?=\\n|\\|)")) |> 
  mutate(details_size = str_remove(details_size, ", Made in .*")) |>
  mutate(details_size = str_remove(details_size, "Size: ")) |> 
  mutate(details_size = str_replace(details_size, "/", ", ")) |>   
  
  #vgpg
  mutate(details_vgpg = purrr::map(str_extract_all(details, "\\b\\d+/\\d+\\b"), unique)) |> 
  mutate(details_vgpg = ifelse(details_vgpg == "character(0)", NA_character_,  details_vgpg)) |> 
  mutate(details_vgpg = str_remove_all(details_vgpg, 'c\\(|\\)|"')) |> 
  mutate(details2 = str_remove(details2, "(?i)(VG/PG:|PG/VG:|VG/PG ratio:|PG/VG ratio:.*?)(?=\\n|\\|)")) |>
  
  #nicotine
  mutate(details_nicotine = str_extract(details2, "(?i)(Nicotine\\s+(concentration|strength|salt\\sstrength):[^\\n]*)")) |> 
  mutate(details_nicotine = str_remove(details_nicotine, "(?i)(Caution:.*)")) |> 
  mutate(details_nicotine = str_remove(details_nicotine, "(?i)(Specifications:.*)")) |>
  mutate(details_nicotine = str_remove(details_nicotine, "(?i)(Pod Capacity:.*)")) |> 
  mutate(details_nicotine = str_extract(details_nicotine, "^[^|]+")) |> 
  mutate(details_nicotine = str_remove(details_nicotine, "\\(.*")) |>
  mutate(details_nicotine_type = word(details_nicotine, 1, sep = ": ")) |> 
  mutate(details_nicotine_values = word(details_nicotine, -1, sep = ": ")) |> 
  mutate(details_nicotine_num = details_nicotine_values |> str_extract_all("\\d+(\\.\\d+)?(?=mg/ml)") %>% map(\(x) as.double(x))) |>
  mutate(details_nicotine_min = map_dbl(details_nicotine_num, min)) |>
  mutate(details_nicotine_max = map_dbl(details_nicotine_num, max)) |>
  # select(-details_nicotine_num, -details_nicotine_values) |>
  
  #price
  #where 2 prices, take the 2nd one. I.e. previous price, new sale price 
  mutate(price2 = as.numeric(str_remove(ifelse(str_detect(price, "\n"), word(price, 2, sep = "\n"), price), "\\$"))) |>
  relocate(price2, .after = price) |> 
  
  #clean-up
  mutate(across(where(is.character), str_trim)) |> 
  select(-details2) |> 
  mutate(disposable_keyword = str_detect(details, "(?i)(disposable)")) |> 
  select(name, category, buttons, details, price, everything())

d |> view()

write_csv(d, fs::path("shosha", "data", latest_run, "cleaned", ext = "csv"))

