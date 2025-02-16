library(tidyverse)

name <- "Vapo"
url <- "https://www.vapo.co.nz"

moh_flavours_approved   <- c("Tobacco", "Pepper", "Grape", "Strawberry", 
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

latest_run <- fs::dir_ls(fs::path("vapo", "data")) |>
  as_tibble() |>
  mutate(value = fs::path_sanitize(str_remove(value, fs::path("vapo", "data") ))) |>
  slice_max(value) |>
  pull()

vapo_scraped <- read_csv(fs::path("vapo", "data", latest_run, glue::glue("vapo-scraped-{str_sub(latest_run, 1, 10)}"), ext = "csv"))  |> 
  rename_with(\(x) glue::glue("{x}_text"))  |> 
  mutate(id = row_number()) |> 
  relocate(id)
  
vapo_cleaned  <- vapo_scraped |>
  
  #price (from the price_text tag)
  mutate(price_num = as.numeric(str_remove_all(ifelse(str_detect(price_text, "\n"), word(price_text, 2, sep = "\n"), price_text), "[\\$,]")))  |>
  
  #disposable (i.e. if the name_text contains disposable)
  mutate(disposable_keyword = str_detect(name_text, "(?i)(disposable)")) |>
  
  #eliquid (i.e. if the name_text contains e-liquid)
  mutate(eliquid_keyword = str_detect(name_text, "(?i)(E-Liquid)")) |> 

  #flavour 
  mutate(name_length = str_count(name_text, "\\|") + 1) |> 
  mutate(name_2nd = str_extract(name_text, ".*(?= \\|)")) |> 
  mutate(flavours_text = ifelse(name_length == 2, str_extract(name_text, ".*(?= \\|)"), NA)) |> 
  select(-name_length, -name_2nd) |> 
  
  #pgvg (i.e. ratios in form XX:XX or XX/XX)
  mutate(pgvg = details_text %>%
           str_extract_all("\\b\\d{2}\\s*[:/]\\s*\\d{2}\\b") %>% # extract two-digit ratios in the format XX/XX or XX:XX
           map(\(x) str_replace_all(x, ":", "/")) %>%
           map_chr(\(x) paste(x, collapse = ", "))
  ) |> 
  
  # nicotine concentration 
  # (i.e. from buttons, numbers preceding mg/ml. Otherwise use details. Safe for whitespace, decimal points, case etc)
  mutate(nicotine_num_buttons = str_extract_all(buttons_text, regex("\\b(\\d+\\.?\\d*)\\s*mg/ml\\b", ignore_case = TRUE))) |> 
  mutate(nicotine_num_buttons = map(nicotine_num_buttons, ~ str_remove_all(.x, "(?i)mg/ml"))) |> 
  mutate(nicotine_min_buttons = map_dbl(nicotine_num_buttons, \(x) min(as.numeric(x), na.rm = TRUE))) |>
  mutate(nicotine_max_buttons = map_dbl(nicotine_num_buttons, \(x) max(as.numeric(x), na.rm = TRUE))) |>
  mutate(nicotine_num_details = str_extract_all(details_text, regex("\\b(\\d+\\.?\\d*)\\s*mg/ml\\b", ignore_case = TRUE))) |> 
  mutate(nicotine_num_details = map(nicotine_num_details, ~ str_remove_all(.x, "(?i)mg/ml"))) |> 
  mutate(nicotine_min_details = map_dbl(nicotine_num_details, \(x) min(as.numeric(x), na.rm = TRUE))) |>
  mutate(nicotine_max_details = map_dbl(nicotine_num_details, \(x) max(as.numeric(x), na.rm = TRUE))) |>
  mutate(across(matches("min|max"), \(x) ifelse(is.infinite(x), NA_real_, x))) |> #glimpse()
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
  )  |> 
  mutate(
    nicotine_max = case_when(
      !is.na(nicotine_max_buttons) ~ nicotine_max_buttons,
      !is.na(nicotine_max_details) ~ nicotine_max_details,
      TRUE ~ NA,
    )
  )  |> 

  # size
  # (i.e. from buttons, numbers preceding ml. Otherwise use details. Safe for whitespace, decimal points, case etc)

  mutate(size_num_buttons = str_extract_all(buttons_text, regex("\\b(\\d+\\.?\\d*)\\s*ml\\b", ignore_case = TRUE))) |> 
  mutate(size_num_buttons = map(size_num_buttons, ~ str_remove_all(.x, "(?i)ml"))) |> 
  mutate(size_min_buttons = map_dbl(size_num_buttons, \(x) min(as.numeric(x), na.rm = TRUE))) |>
  mutate(size_max_buttons = map_dbl(size_num_buttons, \(x) max(as.numeric(x), na.rm = TRUE))) |>
  mutate(size_num_details = str_extract_all(details_text, regex("\\b(\\d+\\.?\\d*)\\s*ml\\b", ignore_case = TRUE))) |>
  mutate(size_num_details = map(size_num_details, ~ str_remove_all(.x, "(?i)ml"))) |> 
  mutate(size_min_details = map_dbl(size_num_details, \(x) min(as.numeric(x), na.rm = TRUE))) |>
  mutate(size_max_details = map_dbl(size_num_details, \(x) max(as.numeric(x), na.rm = TRUE))) |>
  mutate(across(matches("min|max"), \(x) ifelse(is.infinite(x), NA_real_, x))) |> #glimpse()
  mutate(
    size_num = case_when(
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
    name = name,
    url = glue::glue("{url}/{name_text}"))  |> 
  relocate(name, url)

vapo_cleaned |> glimpse()

write_csv(vapo_cleaned, 
          file = fs::path("vapo", "data", latest_run, glue::glue("{str_to_lower(name)}-cleaned-{str_sub(latest_run, 1, 10)}"), ext = "csv"),
          na = "")
