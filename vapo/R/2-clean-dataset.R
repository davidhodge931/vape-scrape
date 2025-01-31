library(tidyverse)

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
  rename_with(\(x) glue::glue("{x}_text"))  
  
vapo_cleaned <- vapo_scraped |> 
  mutate(details_text2 = details_text) |> 
  mutate(details_text2 = str_remove(details_text2, "(?i)(GET FREE SHIPPING).*")) |> 
  mutate(details_text2 = str_replace_all(details_text2, regex("mg/ml", ignore_case = TRUE), "mg/ml")) |>
  
  #price
  mutate(price_num = as.numeric(str_remove_all(ifelse(str_detect(price_text, "\n"), word(price_text, 2, sep = "\n"), price_text), "[\\$,]")))  |>
  
  #disposable
  mutate(disposable_keyword = str_detect(details_text, "(?i)(disposable)") | str_detect(name_text, "(?i)(disposable)")) |> 
  
  #flavour
  mutate(name_length = str_count(name, "\\|") + 1) |> 
  mutate(flavour_text = ifelse(name_length == 2, str_split(name, pattern = " \\| ")[[1]][1], NA)) |> 
  select(-name_length) |> 

  #vgpg_text
  mutate(vgpg_text = purrr::map(str_extract_all(details_text, "\\b\\d+/\\d+\\b"), unique)) |> 
  mutate(vgpg_text = ifelse(vgpg_text == "character(0)", NA_character_,  vgpg_text)) |> 
  mutate(vgpg_text = str_remove_all(vgpg_text, 'c\\(|\\)|"')) |> 
  mutate(details_text2 = str_remove(details_text2, "(?i)(VG/PG:|PG/VG:|VG/PG ratio:|PG/VG ratio:.*?)(?=\\n|\\|)")) |>
  
  #size
  mutate(
    # Extract the size information (everything after "Size:" and up to the next newline or the end of the string)
    size_values_details = str_trim(str_extract(details_text2, "(?i)Size:.*?(?=\\n|$)")),
    # Extract all numeric values for the sizes (ignoring 'mL' or 'ml')
    size_num_details = str_extract_all(size_values_details, "\\d+(\\.\\d+)?(?=\\s?[mM][lL])") |> 
      map(~ as.double(.)),
    # Calculate min and max for each vector of sizes
    size_min_details = map_dbl(size_num_details, ~ min(.x, na.rm = TRUE)),
    size_max_details = map_dbl(size_num_details, ~ max(.x, na.rm = TRUE))
  ) |> 

  mutate(size_values_details = str_extract(details_text2, "(?i)(Size:.*?)(?=\\n|\\|)")) |> 
  mutate(size_values_details = str_remove(size_values_details, ", Made in .*")) |>
  mutate(size_values_details = str_remove(size_values_details, "Size: ")) |> 
  mutate(size_values_details = str_replace(size_values_details, "/", ", ")) |>   
  
  
  # mutate(size_num_details = size_values_details |> str_extract_all("\\d+(\\.\\d+)?(?=ml)") %>% map(\(x) as.double(x))) |>
  # mutate(size_num_details = size_values_details |> str_extract_all("\\d+(\\.\\d+)?(?=ml)") %>% map(\(x) as.double(x))) |>
  
  # vapo_cleaned |> select(size_values_details) |> 
  mutate(size_num_details = size_values_details |> str_extract_all("\\d+(\\.\\d+)?(?=ml)") %>% str_replace_all("(?i)[,ml]", "") |> map(\(x) as.double(x))) |>
  
  # mutate(size_num_details = as.numeric(str_replace_all(size_values_details, "[,mL]", ""))) |> filter(!is.na(size_values_details)) 
  
  mutate(size_min_details = map_dbl(size_num_details, min)) |>
  mutate(size_max_details = map_dbl(size_num_details, max)) |>
  
  mutate(
    size_buttons = buttons_text |> 
      str_split(", ") %>%
      purrr::map(\(x) str_subset(x, regex("\\dml$", ignore_case = TRUE))) %>%
      purrr::map_chr(\(x) paste(x, collapse = ", "))
  ) |> 
  
  mutate(size_values_buttons = ifelse(size_buttons == "", NA, size_buttons)) |>
  # mutate(size_values_buttons = ifelse(size_buttons == "" | category_text != "E-Liquids", NA, size_buttons)) |>
  
  mutate(size_num_buttons = size_values_buttons |> str_extract_all("\\d+(\\.\\d+)?(?=ml)") %>% map(\(x) as.double(x))) |>
  mutate(size_min_buttons = map_dbl(size_num_buttons, min)) |>
  mutate(size_max_buttons = map_dbl(size_num_buttons, max)) |> 
  
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
  mutate(across(c(size_min, size_max), \(x) ifelse(is.infinite(x), NA, x)))  |> 

  #nicotine
  mutate(nicotine_details = str_extract(details_text2, "(?i)(nicotine\\s+(concentration|strength)?\\s*[:]?\\s*(\\d+\\.?\\d*)\\s*mg/ml)")) |> 
  mutate(nicotine_details = str_remove(nicotine_details, "(?i)(Caution:.*)")) |> 
  mutate(nicotine_details = str_remove(nicotine_details, "(?i)(Specifications:.*)")) |>
  mutate(nicotine_details = str_remove(nicotine_details, "(?i)(Pod Capacity:.*)")) |> 
  mutate(nicotine_details = str_extract(nicotine_details, "^[^|]+")) |> 
  mutate(nicotine_details = str_remove(nicotine_details, "\\(.*")) |>
  mutate(nicotine_values_details = word(nicotine_details, -1, sep = ": ")) |>
  mutate(nicotine_num_details = nicotine_values_details |> str_extract_all("\\d+(\\.\\d+)?(?=mg/ml)") %>% map(\(x) as.double(x))) |>
  mutate(nicotine_min_details = map_dbl(nicotine_num_details, min)) |>
  mutate(nicotine_max_details = map_dbl(nicotine_num_details, max)) |>
  mutate(nicotine_values_details = str_replace_all(nicotine_values_details, "\\s*[-–—]+\\s*", ", ")) |>
  
  mutate(
    nicotine_values_buttons = buttons_text |> 
      str_split(", ") %>%
      purrr::map(\(x) str_subset(x, regex("\\dmg/ml$", ignore_case = TRUE))) %>%
      purrr::map_chr(\(x) paste(x, collapse = ", "))
  ) |> 
  mutate(nicotine_values_buttons = ifelse(nicotine_values_buttons == "" | category_text != "E-Liquids", NA, nicotine_values_buttons)) |>
  mutate(nicotine_num_buttons = nicotine_values_buttons |> str_extract_all("\\d+(\\.\\d+)?(?=mg/ml)") %>% map(\(x) as.double(x))) |>
  mutate(nicotine_min_buttons = map_dbl(nicotine_num_buttons, min)) |>
  mutate(nicotine_max_buttons = map_dbl(nicotine_num_buttons, max)) |>
  
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
  mutate(across(c(nicotine_min, nicotine_max), \(x) ifelse(is.infinite(x), NA, x)))  |> 

  #approved
  mutate(across(where(is.character), str_trim)) |> 
  
  #approved
  mutate(
    flavours_each_approved   = 
      ifelse(is.na(flavours_text), NA, flavours_text |> 
               str_split(",\\s*") |> 
               map_lgl(~ all(.x %in% moh_flavours_approved  )) 
      )
  ) |> 
  mutate(flavours_count_approx = str_count(flavours_text, ",") + 1) |>
  
  mutate(
    nicotine_approved = case_when(
      disposable_keyword & nicotine_max > 20 ~ FALSE,!disposable_keyword &
        nicotine_max > 28.5 ~ FALSE,
      TRUE ~ TRUE,
    )
  ) |> 
  
  #clean-up
  select(-details_text2, -ends_with("details"), -ends_with("buttons")) |> 
  mutate(
    ais = "https://www.vapo.co.nz",
    url = glue::glue("https://www.vapo.co.nz/{name_text}")) |> 
  select(
    ais,
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

vapo_cleaned |> glimpse()

write_csv(vapo_cleaned, 
          file = fs::path("vapo", "data", latest_run, glue::glue("vapo-cleaned-{str_sub(latest_run, 1, 10)}"), ext = "csv"),
          na = "")

openxlsx::write.xlsx(vapo_cleaned, 
                     file = fs::path("vapo", "data", latest_run, glue::glue("vapo-cleaned-{str_sub(latest_run, 1, 10)}"), ext = "xlsx"))

vapo_cleaned |> glimpse()

vapo_cleaned |> 
  filter(category_text == "E-Liquids") |> 
  view()

vapo_cleaned |> 
  group_by(category_text) |> 
  count()
