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
  rename_with(\(x) glue::glue("{x}_text"))  |> 
  mutate(id = row_number()) |> 
  relocate(id)
  
# vapo_cleaned <- 
d <- vapo_scraped |> 
  #price (from the price_text tag)
  mutate(price_num = as.numeric(str_remove_all(ifelse(str_detect(price_text, "\n"), word(price_text, 2, sep = "\n"), price_text), "[\\$,]")))  |>
  
  #disposable (i.e. if the name_text contains disposable)
  mutate(disposable_keyword = str_detect(name_text, "(?i)(disposable)")) |> 
  
  #flavour 
  mutate(name_length = str_count(name_text, "\\|") + 1) |> 
  mutate(name_2nd = str_extract(name_text, ".*(?= \\|)")) |> 
  mutate(flavours_text = ifelse(name_length == 2, str_extract(name_text, ".*(?= \\|)"), NA)) |> 
  select(-name_length) |> 
  
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
  ) 

d |> glimpse()

###############################################################################################################
###############################################################################################################





# ifelse(is.infinite(size_min_details), NA, size_min_details  

  # size (i.e. from buttons, numbers preceding ml)
  mutate(size_values_buttons = str_extract_all(buttons_text, regex("\\d+(?=\\s*ml)", ignore_case = TRUE))) |> 
  mutate(size_min_buttons = map_dbl(size_values_buttons, \(x) min(as.numeric(x), na.rm = TRUE))) |>
  mutate(size_max_buttons = map_dbl(size_values_buttons, \(x) max(as.numeric(x), na.rm = TRUE))) 





  #size (i.e extract from buttons)
  mutate(
    # Extract the size information (everything after "Size:" and up to the next newline or the end of the string)
    size_values_details = NA, #str_trim(str_extract(details_text, "(?i)Size:.*?(?=\\n|$)")),
    # Extract all numeric values for the sizes (ignoring 'mL' or 'ml')
    size_num_details = str_extract_all(buttons_text, "(?i)\\b\\d+(?=\\s*ml\\b(?!\\s*/?mg))") |>
      map(~ as.double(.)),
    # Calculate min and max for each vector of sizes
    size_min_details = map_dbl(size_num_details, ~ min(.x, na.rm = TRUE)),
    size_max_details = map_dbl(size_num_details, ~ max(.x, na.rm = TRUE))
  ) |> 
  mutate(size_min_details = ifelse(is.infinite(size_min_details), NA, size_min_details)) |> 
  mutate(size_max_details = ifelse(is.infinite(size_max_details), NA, size_max_details)) |> #glimpse()

  mutate(size_values_details = details_text |> str_extract_all("\\d+(\\.\\d+)?(?=ml)") %>% str_replace_all("(?i)[,ml]", "") |> map(\(x) as.double(x))) |>
  #could get any size info from details text field - and then use this if no buttons size info (see shosha code)
  
  #nicotine

  
  # mutate(nicotine_details = str_extract(details_text, "(?i)(nicotine\\s+(concentration|strength)?\\s*[:]?\\s*(\\d+\\.?\\d*)\\s*mg/ml)")) |> 
  # mutate(nicotine_details = str_remove(nicotine_details, "(?i)(Caution:.*)")) |> 
  # mutate(nicotine_details = str_remove(nicotine_details, "(?i)(Specifications:.*)")) |>
  # mutate(nicotine_details = str_remove(nicotine_details, "(?i)(Pod Capacity:.*)")) |> 
  # mutate(nicotine_details = str_extract(nicotine_details, "^[^|]+")) |> 
  # mutate(nicotine_details = str_remove(nicotine_details, "\\(.*")) |>
  # mutate(nicotine_values_details = word(nicotine_details, -1, sep = ": ")) |>
  # mutate(nicotine_num_details = nicotine_values_details |> str_extract_all("\\d+(\\.\\d+)?(?=mg/ml)") %>% map(\(x) as.double(x))) |>
  # mutate(nicotine_min_details = map_dbl(nicotine_num_details, min)) |>
  # mutate(nicotine_max_details = map_dbl(nicotine_num_details, max)) |>
  # mutate(nicotine_values_details = str_replace_all(nicotine_values_details, "\\s*[-–—]+\\s*", ", ")) |>
  
  mutate(
    nicotine_values_buttons = buttons_text |> 
      str_split(", ") %>%
      purrr::map(\(x) str_subset(x, regex("\\b(\\d+)\\s*mg/ml\\b", ignore_case = TRUE))) %>%
      purrr::map_chr(\(x) paste(x, collapse = ", "))
    ) |> 
   mutate(nicotine_values_buttons = ifelse(nicotine_values_buttons == "", NA, nicotine_values_buttons)) |>
  glimpse()
  
# 
# Extract all matches
# matches <- str_extract_all(text, regex(pattern, ignore_case = TRUE))
# pattern <- "\\b(\\d+)\\s*mg/ml\\b"
# 
# # Extract only the numeric values using tidyverse functions
# tibble(text = text) %>%
#   mutate(text = str_match_all(text, regex(pattern, ignore_case = TRUE)) %>% # Extract matches
#            map(\(x) x[, 2] %>% as.numeric()) %>% # Extract the captured group (numbers) and convert to numeric
#            set_names(seq_along(text)) # Optional: Add names to the list for clarity
#   ) #|> View()


  
  # glimpse()
  mutate(nicotine_num_buttons = nicotine_values_buttons |> 
           str_extract_all("\\b(\\d+)\\s*mg/ml\\b") %>% 
           str_remove_all(pattern = "mg/ml") |> 
           map(\(x) as.double(x))) |>
  glimpse()
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
  glimpse()

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
  select(-ends_with("details"), -ends_with("buttons")) |> 
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

# vapo_cleaned |> 
#   filter(category_text == "E-Liquids") |> 
#   view()
# 
# vapo_cleaned |> 
#   group_by(category_text) |> 
#   count()
