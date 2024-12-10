library(tidyverse)

moh_flavours_approved <- c("Tobacco", "Pepper", "Grape", "Strawberry", 
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

shosha_scraped <- read_csv(fs::path("shosha", "data", latest_run, glue::glue("shosha-scraped-{str_sub(latest_run, 1, 10)}"), ext = "csv"))  

shosha_cleaned <- shosha_scraped |> 
  rename_with(\(x) glue::glue("{x}_text")) |> 
  mutate(details_text2 = details_text) |> 
  mutate(details_text2 = str_remove(details_text2, "(?i)(GET FREE SHIPPING).*")) |> 
  mutate(details_text2 = str_replace_all(details_text2, regex("mg/ml", ignore_case = TRUE), "mg/ml")) |> 
  
  #flavour
  mutate(flavours_text = str_extract(details_text2, "(?i)(Flavor Profile:|Flavour Profile:|Flavor:|Flavour:|Flavors Profile:|flavours_text Profile:|Flavors:|flavours_text:)(.*?)(?=\n|\\|)")) |> 
  mutate(flavours_text = str_remove(flavours_text, "(?i)(Flavor Profile:|Flavour Profile:|Flavor:|Flavour:|Flavors Profile:|flavours_text Profile:|Flavors:|flavours_text:)")) |>
  mutate(flavours_text = str_remove(flavours_text, "\\.")) |> 
  mutate(flavours_text = str_replace_all(flavours_text, ", &", ",")) |>
  mutate(flavours_text = str_replace_all(flavours_text, " &", ",")) |>
  mutate(flavours_text = str_replace_all(flavours_text, ", and ", ", ")) |>
  mutate(flavours_text = str_replace_all(flavours_text, " and ", ", ")) |>
  mutate(flavours_text = str_replace_all(flavours_text, ",,", ",")) |>
  mutate(flavours_text = str_replace_all(flavours_text, ";", ",")) |>
  mutate(details_text2 = str_remove(details_text2, "(?i)(Flavor Profile:|Flavour Profile:|Flavor:|Flavour:|Flavors Profile:|flavours_text Profile:|Flavors:|flavours_text:)(.*?)(?=\n|\\|)")) |>
  
  #vgpg_text
  mutate(vgpg_text = purrr::map(str_extract_all(details_text, "\\b\\d+/\\d+\\b"), unique)) |> 
  mutate(vgpg_text = ifelse(vgpg_text == "character(0)", NA_character_,  vgpg_text)) |> 
  mutate(vgpg_text = str_remove_all(vgpg_text, 'c\\(|\\)|"')) |> 
  mutate(details_text2 = str_remove(details_text2, "(?i)(VG/PG:|PG/VG:|VG/PG ratio:|PG/VG ratio:.*?)(?=\\n|\\|)")) |>
  
  #size
  mutate(size_values_details = str_extract(details_text2, "(Size:.*?)(?=\\n|\\|)")) |> 
  mutate(size_values_details = str_remove(size_values_details, ", Made in .*")) |>
  mutate(size_values_details = str_remove(size_values_details, "Size: ")) |> 
  mutate(size_values_details = str_replace(size_values_details, "/", ", ")) |>   
  
  mutate(size_num_details = size_values_details |> str_extract_all("\\d+(\\.\\d+)?(?=ml)") %>% map(\(x) as.double(x))) |>
  mutate(size_min_details = map_dbl(size_num_details, min)) |>
  mutate(size_max_details = map_dbl(size_num_details, max)) |> 
  
  #nicotine
  mutate(nicotine_details = str_extract(details_text2, "(?i)(Nicotine\\s+(concentration|strength|salt\\sstrength):[^\\n]*)")) |> 
  mutate(nicotine_details = str_remove(nicotine_details, "(?i)(Caution:.*)")) |> 
  mutate(nicotine_details = str_remove(nicotine_details, "(?i)(Specifications:.*)")) |>
  mutate(nicotine_details = str_remove(nicotine_details, "(?i)(Pod Capacity:.*)")) |> 
  mutate(nicotine_details = str_extract(nicotine_details, "^[^|]+")) |> 
  mutate(nicotine_details = str_remove(nicotine_details, "\\(.*")) |>
  mutate(nicotine_type_details = word(nicotine_details, 1, sep = ": ")) |> 
  mutate(nicotine_values_details = word(nicotine_details, -1, sep = ": ")) |> 
  mutate(nicotine_num_details = nicotine_values_details |> str_extract_all("\\d+(\\.\\d+)?(?=mg/ml)") %>% map(\(x) as.double(x))) |>
  mutate(nicotine_min_details = map_dbl(nicotine_num_details, min)) |>
  mutate(nicotine_max_details = map_dbl(nicotine_num_details, max)) |>
  mutate(nicotine_values_details = str_replace_all(nicotine_values_details, "\\s*[-–—]+\\s*", ", ")) |> 
  
  #price_text
  #where 2 price_texts, take the 2nd one. I.e. previous price_text, new sale price_text 
  mutate(price_num = as.numeric(str_remove(ifelse(str_detect(price_text, "\n"), word(price_text, 2, sep = "\n"), price_text), "\\$"))) |>
  relocate(price_num, .after = price_text) |> 
  
  #buttons_text
  mutate(
    nicotine_values_buttons = buttons_text |> 
      str_split(", ") %>%
      purrr::map(\(x) str_subset(x, regex("\\dmg/ml$", ignore_case = TRUE))) %>%
      purrr::map_chr(\(x) paste(x, collapse = ", "))
  ) |> 
  mutate(
    size_buttons = buttons_text |> 
      str_split(", ") %>%
      purrr::map(\(x) str_subset(x, regex("\\dml$", ignore_case = TRUE))) %>%
      purrr::map_chr(\(x) paste(x, collapse = ", "))
  ) |> 
  mutate(
    nicotine_values_buttons = ifelse(nicotine_values_buttons == "" | category_text != "E-Liquids", NA, nicotine_values_buttons),
    size_values_buttons = ifelse(size_buttons == "" | category_text != "E-Liquids", NA, size_buttons),
  ) |> 
  mutate(nicotine_num_buttons = nicotine_values_buttons |> str_extract_all("\\d+(\\.\\d+)?(?=mg/ml)") %>% map(\(x) as.double(x))) |>
  mutate(nicotine_min_buttons = map_dbl(nicotine_num_buttons, min)) |>
  mutate(nicotine_max_buttons = map_dbl(nicotine_num_buttons, max)) |>
  mutate(size_num_buttons = size_values_buttons |> str_extract_all("\\d+(\\.\\d+)?(?=ml)") %>% map(\(x) as.double(x))) |>
  mutate(size_min_buttons = map_dbl(size_num_buttons, min)) |>
  mutate(size_max_buttons = map_dbl(size_num_buttons, max)) |> 
  
  #clean-up
  mutate(across(where(is.character), str_trim)) |> 
  select(-details_text2) |> 
  mutate(disposable_keyword = str_detect(details_text, "(?i)(disposable)")) |> 
  
  # estimate nicotine and size by prioritising buttons_text info
  mutate(
    nicotine_type = case_when(
      !is.na(nicotine_num_buttons) ~ "Nicotine Concentration",
      TRUE ~ nicotine_type_details,
    )
  ) |>
  mutate(
    nicotine_num = case_when(
      !is.na(nicotine_num_buttons) ~ nicotine_num_buttons,
      !is.na(nicotine_num_details) ~ nicotine_num_details,
      TRUE ~ NA,
    )
  ) |>
  mutate(
    nicotine_min = case_when(
      !is.na(nicotine_min_buttons) ~ nicotine_min_buttons,
      !is.na(nicotine_min_details) ~ nicotine_min_details,
      TRUE ~ NA,
    )
  ) |> 
  mutate(
    nicotine_max = case_when(
      !is.na(nicotine_max_buttons) ~ nicotine_max_buttons,
      !is.na(nicotine_max_details) ~ nicotine_max_details,
      TRUE ~ NA,
    )
  ) |> 
  mutate(
    nicotine_size_num = case_when(
      !is.na(size_num_buttons) ~ size_num_buttons,
      !is.na(size_num_details) ~ size_num_details,
      TRUE ~ NA,
    )
  ) |>
  mutate(
    size_min = case_when(
      !is.na(size_min_buttons) ~ size_min_buttons,
      !is.na(size_min_details) ~ size_min_details,
      TRUE ~ NA,
    )
  ) |> 
  mutate(
    size_max = case_when(
      !is.na(size_max_buttons) ~ size_max_buttons,
      !is.na(size_max_details) ~ size_max_details,
      TRUE ~ NA,
    )
  ) |> 
  
  mutate(
    flavours_approved = 
      ifelse(is.na(flavours_text), NA, flavours_text |> 
               str_split(",\\s*") |> 
               map_lgl(~ all(.x %in% moh_flavours_approved)) 
      )
  ) |> 
  mutate(flavours_count = str_count(flavours_text, ",") + 1) |>
  
  #convert infinites to NA
  mutate(across(c(size_min, size_max, nicotine_min, nicotine_max), \(x) ifelse(is.infinite(x), NA, x)))  |> 
  
  #get rid of unneeded vars and make nice order
  select(-ends_with("details"), -ends_with("buttons")) |> 
  mutate(url = glue::glue("https://www.shosha.co.nz/{name_text}")) |> 
  select(
    name_text,
    category_text,
    starts_with("price"),
    starts_with("flavours"),
    starts_with("vgpg"),
    starts_with("nicotine"),
    starts_with("size"),
    disposable_keyword,
    url,
    everything(),
  )  

write_csv(shosha_cleaned, 
          file = fs::path("shosha", "data", latest_run, glue::glue("shosha-cleaned-{str_sub(latest_run, 1, 10)}"), ext = "csv"),
          na = "")

d |> glimpse()


