library(tidyverse)

flavours_approved <- c("Tobacco", "Pepper", "Grape", "Strawberry", 
                       "Menthol",	"Spice",	"Guava",	"Tropical",
                       "Mint",	"Cappuccino",	"Kiwifruit",	"Watermelon",
                       "Peppermint",	"Coffee",	"Lemon",	"Caramel",
                       "Spearmint",	"Espresso",	"Lime", "Chocolate",
                       "Almond",	"Latte",	"Lychee",	"Cream",
                       "Hazelnut",	"Tea",	"Mango",	"Custard",
                       "Nut",	"Apple",	"Orange",	"Honey",
                       "Oat",	"Banana",	"Passionfruit",	"Sour",
                       "Peanut",	"Berry",	"Peach",	"Sweet",
                       "Pecan",	"Blackberry",	"Pear",	"Vanilla",
                       "Cinnamon",	"Blueberry",	"Pineapple", "Unflavoured",
                       "Clove",	"Cherry",	"Plum",	
                       "Licorice",	"Citrus",	"Pomegranate",	
                       "Nutmeg",	"Coconut",	"Raspberry")

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
  mutate(details_flavour = str_replace_all(details_flavour, ", &", ",")) |>
  mutate(details_flavour = str_replace_all(details_flavour, " &", ",")) |>
  mutate(details_flavour = str_replace_all(details_flavour, ", and ", ", ")) |>
  mutate(details_flavour = str_replace_all(details_flavour, " and ", ", ")) |>
  mutate(details_flavour = str_replace_all(details_flavour, ",,", ",")) |>
  mutate(details_flavour = str_replace_all(details_flavour, ";", ",")) |>
  mutate(details2 = str_remove(details2, "(?i)(Flavor Profile:|Flavour Profile:|Flavor:|Flavour:|Flavors Profile:|Flavours Profile:|Flavors:|Flavours:)(.*?)(?=\n|\\|)")) |>
  
  #size
  mutate(details_size_values = str_extract(details2, "(Size:.*?)(?=\\n|\\|)")) |> 
  mutate(details_size_values = str_remove(details_size_values, ", Made in .*")) |>
  mutate(details_size_values = str_remove(details_size_values, "Size: ")) |> 
  mutate(details_size_values = str_replace(details_size_values, "/", ", ")) |>   
  
  mutate(details_size_num = details_size_values |> str_extract_all("\\d+(\\.\\d+)?(?=ml)") %>% map(\(x) as.double(x))) |>
  mutate(details_size_min = map_dbl(details_size_num, min)) |>
  mutate(details_size_max = map_dbl(details_size_num, max)) |> 
  
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
  mutate(details_nicotine_values = str_replace_all(details_nicotine_values, "\\s*[-–—]+\\s*", ", ")) |> 
  
  #price
  #where 2 prices, take the 2nd one. I.e. previous price, new sale price 
  mutate(price_num = as.numeric(str_remove(ifelse(str_detect(price, "\n"), word(price, 2, sep = "\n"), price), "\\$"))) |>
  relocate(price_num, .after = price) |> 
  
  #buttons
  mutate(
    buttons_nicotine_values = buttons |> 
      str_split(", ") %>%
      purrr::map(\(x) str_subset(x, regex("\\dmg/ml$", ignore_case = TRUE))) %>%
      purrr::map_chr(\(x) paste(x, collapse = ", "))
  ) |> 
  mutate(
    buttons_size = buttons |> 
      str_split(", ") %>%
      purrr::map(\(x) str_subset(x, regex("\\dml$", ignore_case = TRUE))) %>%
      purrr::map_chr(\(x) paste(x, collapse = ", "))
  ) |> 
  mutate(
    buttons_nicotine_values = ifelse(buttons_nicotine_values == "" | category != "E-Liquids", NA, buttons_nicotine_values),
    buttons_size_values = ifelse(buttons_size == "" | category != "E-Liquids", NA, buttons_size),
  ) |> 
  mutate(buttons_nicotine_num = buttons_nicotine_values |> str_extract_all("\\d+(\\.\\d+)?(?=mg/ml)") %>% map(\(x) as.double(x))) |>
  mutate(buttons_nicotine_min = map_dbl(buttons_nicotine_num, min)) |>
  mutate(buttons_nicotine_max = map_dbl(buttons_nicotine_num, max)) |>
  mutate(buttons_size_num = buttons_size_values |> str_extract_all("\\d+(\\.\\d+)?(?=ml)") %>% map(\(x) as.double(x))) |>
  mutate(buttons_size_min = map_dbl(buttons_size_num, min)) |>
  mutate(buttons_size_max = map_dbl(buttons_size_num, max)) |> 

  #clean-up
  mutate(across(where(is.character), str_trim)) |> 
  select(-details2) |> 
  mutate(details_disposable_keyword = str_detect(details, "(?i)(disposable)")) |> 
  
  # estimate nicotine and size by prioritising buttons info
  mutate(
    estimate_nicotine_type = case_when(
      !is.na(buttons_nicotine_num) ~ "Nicotine Concentration",
      TRUE ~ details_nicotine_type,
    )
  ) |>
  mutate(
    estimate_nicotine_num = case_when(
      !is.na(buttons_nicotine_num) ~ buttons_nicotine_num,
      !is.na(details_nicotine_num) ~ details_nicotine_num,
      TRUE ~ NA,
    )
  ) |>
  mutate(
    estimate_nicotine_min = case_when(
      !is.na(buttons_nicotine_min) ~ buttons_nicotine_min,
      !is.na(details_nicotine_min) ~ details_nicotine_min,
      TRUE ~ NA,
    )
  ) |> 
  mutate(
    estimate_nicotine_max = case_when(
      !is.na(buttons_nicotine_max) ~ buttons_nicotine_max,
      !is.na(details_nicotine_max) ~ details_nicotine_max,
      TRUE ~ NA,
    )
  ) |> 
  mutate(
    estimate_size_num = case_when(
      !is.na(buttons_size_num) ~ buttons_size_num,
      !is.na(details_size_num) ~ details_size_num,
      TRUE ~ NA,
    )
  ) |>
  mutate(
    estimate_size_min = case_when(
      !is.na(buttons_size_min) ~ buttons_size_min,
      !is.na(details_size_min) ~ details_size_min,
      TRUE ~ NA,
    )
  ) |> 
  mutate(
    estimate_size_max = case_when(
      !is.na(buttons_size_max) ~ buttons_size_max,
      !is.na(details_size_max) ~ details_size_max,
      TRUE ~ NA,
    )
  ) |> 
  
  mutate(
    details_flavours_all_approved = 
      ifelse(is.na(details_flavour), NA, details_flavour |> 
               str_split(",\\s*") |> 
               map_lgl(~ all(.x %in% flavours_approved)) 
      )
  ) |> 
  mutate(details_flavours_count = str_count(details_flavour, ",") + 1) |>

  #make sure nice order
  select(name, category, contains("price"), contains("details"), contains("buttons"), contains("estimate"), everything()) 

d
d |> glimpse()

write_csv(d_summary, fs::path("shosha", "data", latest_run, glue::glue("shosha-workings-{str_sub(latest_run, 1, 10)}"), ext = "csv"))

d |> 
  glimpse()

d_summary <- d |> 
  select(name, 
         category, 
         contains("estimate"),
         contains("vpvg"), 
         contains("flavour"),
         contains("disposable"),
  ) |> 
  rename_with(\(x) str_remove(x, "details_")) |> 
  rename_with(\(x) str_remove(x, "estimate_")) 

write_csv(d_summary, fs::path("shosha", "data", latest_run, glue::glue("shosha-{str_sub(latest_run, 1, 10)}"), ext = "csv"))


