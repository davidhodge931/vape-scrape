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
  mutate(details2 = str_remove(details2, "(Size:.*?)(?=\\n|\\|)")) |>
  mutate(vgpg = str_extract(details2, "(?i)(VG/PG:.*?)(?=\\n|\\|)")) |> 
  mutate(details2 = str_remove(details2, "(?i)(VG/PG:.*?)(?=\\n|\\|)")) |> 
  mutate(nicotine = str_extract(details2, "(?i)(Nicotine\\s+(concentration|strength|salt\\sstrength):[^\\n]*)")) |> 
  mutate(nicotine = str_remove(nicotine, "(?i)(Caution:.*)")) |> 
  mutate(nicotine = str_remove(nicotine, "(?i)(Specifications:.*)")) |>
  mutate(nicotine = str_remove(nicotine, "(?i)(Pod Capacity:.*)")) |> 
  mutate(nicotine = str_remove(nicotine, "\\|")) |> 
  select(-details2) |> 
  relocate(details, .after = nicotine) 

d |> filter(!is.na(size)) |> View() #514 products
d |> filter(!is.na(vgpg)) #294 products
d |> filter(!is.na(flavour)) #240 products
d |> filter(!is.na(nicotine)) #307 products

d |> 
  filter(!is.na(nicotine)) |>
  select(nicotine) |> 
  arrange(desc(nicotine)) |> 
  View()  

shosha |> 
  mutate(nicotine = str_extract(details, pattern)) |> 
  filter(!is.na(nicotine))

nicotine_text <- "(?i)(Nicotine Concentration:|Nicotine concentration:|
nicotine concentration:|Nicotine Strength:|Nicotine strength:|nicotine strength:|
Nicotine salt strength:|Nicotine Salt Strength:|nicotine salt strength:|
Nicotine:|nicotine)\\s?\\d+(\\.\\d+)?\\s?mg/ml\\s?[-/]?\\s?\\d+(\\.\\d+)?\\s?mg/ml\\s?[-/]?\\s?\\d+(\\.\\d+)?\\s?mg/ml\\s?[-/]?\\s?\\d+(\\.\\d+)?\\s?mg/ml\\s?[-/]?\\s?\\d+(\\.\\d+)?\\s?mg/ml"

first_number <- "(?i)(?<=\\s|^)(\\d+(\\.\\d+)?)\\s?mg/ml"

shosha |> 
  mutate(nicotine = list(as.numeric(str_extract_all(details, "\\b\\d+(\\.\\d+)?\\s*(mg/ml|mg/mL)\\b")[[1]]))) |>
  mutate(blah = unlist(min(x, na.rm = TRUE))) |> 
  filter(!is.infinite(blah)) |> 
  filter(!is.na(blah))

d <- shosha |> #distinct(category)
  rowwise() |> 
  mutate(nicotine1 = as.numeric(str_extract(str_extract_all(details, "\\b\\d+(\\.\\d+)?\\s*(mg/ml|mg/mL)\\b")[[1]][1], "\\d+(\\.\\d+)?"))) |> 
  mutate(nicotine2 = as.numeric(str_extract(str_extract_all(details, "\\b\\d+(\\.\\d+)?\\s*(mg/ml|mg/mL)\\b")[[1]][2], "\\d+(\\.\\d+)?"))) |> 
  mutate(nicotine3 = as.numeric(str_extract(str_extract_all(details, "\\b\\d+(\\.\\d+)?\\s*(mg/ml|mg/mL)\\b")[[1]][3], "\\d+(\\.\\d+)?"))) |> 
  mutate(nicotine4 = as.numeric(str_extract(str_extract_all(details, "\\b\\d+(\\.\\d+)?\\s*(mg/ml|mg/mL)\\b")[[1]][4], "\\d+(\\.\\d+)?"))) |> 
  mutate(nicotine5 = as.numeric(str_extract(str_extract_all(details, "\\b\\d+(\\.\\d+)?\\s*(mg/ml|mg/mL)\\b")[[1]][5], "\\d+(\\.\\d+)?"))) |> 
  mutate(nicotine6 = as.numeric(str_extract(str_extract_all(details, "\\b\\d+(\\.\\d+)?\\s*(mg/ml|mg/mL)\\b")[[1]][6], "\\d+(\\.\\d+)?"))) |> 
  mutate(nicotine7 = as.numeric(str_extract(str_extract_all(details, "\\b\\d+(\\.\\d+)?\\s*(mg/ml|mg/mL)\\b")[[1]][7], "\\d+(\\.\\d+)?"))) |> 
  mutate(nicotine8 = as.numeric(str_extract(str_extract_all(details, "\\b\\d+(\\.\\d+)?\\s*(mg/ml|mg/mL)\\b")[[1]][8], "\\d+(\\.\\d+)?"))) |> 
  mutate(nicotine9 = as.numeric(str_extract(str_extract_all(details, "\\b\\d+(\\.\\d+)?\\s*(mg/ml|mg/mL)\\b")[[1]][9], "\\d+(\\.\\d+)?"))) |> 
  mutate(nicotine10 = as.numeric(str_extract(str_extract_all(details, "\\b\\d+(\\.\\d+)?\\s*(mg/ml|mg/mL)\\b")[[1]][10], "\\d+(\\.\\d+)?"))) |>
  mutate(nicotine_min = min(nicotine1,nicotine2,nicotine3,nicotine4,nicotine5,nicotine6,nicotine7,nicotine8,nicotine9,nicotine10, na.rm = TRUE)) |> 
  mutate(nicotine_min = ifelse(is.infinite(nicotine_min), NA, nicotine_min)) |> 
  mutate(nicotine_max = max(nicotine1,nicotine2,nicotine3,nicotine4,nicotine5,nicotine6,nicotine7,nicotine8,nicotine9,nicotine10, na.rm = TRUE)) |> 
  mutate(nicotine_max = ifelse(is.infinite(nicotine_max), NA, nicotine_max)) |> 
  mutate(nicotine_salt_strength = str_detect(details, "Nicotine strength|Nicotine salt strength|Nicotine Salt strength|Nicotine Salt Strength|nicotine salt strength")) |> 
  mutate(nicotine_concentration = str_detect(details, "Nicotine concentration|Nicotine Concentration|nicotine concentration")) |> 
  mutate(nicotine_strength = str_detect(details, "Nicotine strength|Nicotine Strength|nicotine strength")) |> 
  select(name, category, nicotine_min, nicotine_max, nicotine_salt_strength, nicotine_concentration, nicotine_strength)  

d |> 
  View()

d |> tail()
d |> head()

d |> 
  mutate()

min(c(NA, 5, 6), na.rm = T)

d |> 

d |> 
  write_excel_csv(glue::glue("shosha-{today()}.csv"), na = "")

test1 <- "Previously known as Shosha Nothing But Green E-Liquid.

Flavor Profile: Cool Refreshing Mint

Size: 30ml

VG/PG: 50/50

Shosha Nothing But Green gives you the feeling of cool icy mint with an amazing fragrance. Filled with a cool mint feeling all through the body, new and exquisite invigorating. Cold and refreshing, that is the feeling that we often associate with mint which is what this amazing E-liquid will bring to you.

Nicotine Strength: 0mg/ml - 3mg/ml - 6mg/ml - 9mg/ml - 12mg/ml - 18mg/ml

Caution: Keep out of reach of children or pets. Wash contact area thoroughly if skin, eye or mouth contact with the substance occurs. Do not swallow. If taken into the mouth, rinse mouth thoroughly. Contact 0800 POISON (0800 764 766) for advice if swallowed. Seek medical advice if you feel unwell after contact with the substance or use of this product."

test2 <- "Previously known as Salty Berry World Blue Razz Lemonade Freebase E-liquid. 

Flavor Profile: Blueberry, Raspberry, Lemonade

Size: 30ml

VG/PG: 50/50

Also available in Nicotine Salt

The most iconic berries taste is here. Blueberry and Raspberry beautifully mixed with a refreshing lemonade. It’s a match made in heaven.

Nicotine Strength: 0mg/ml - 3mg/ml - 6mg/ml

Caution: Keep out of reach of children or pets. Wash contact area thoroughly if skin, eye or mouth contact with the substance occurs. Do not swallow. If taken into the mouth, rinse mouth thoroughly. Contact 0800 POISON (0800 764 766) for advice if swallowed. Seek medical advice if you feel unwell after contact with the substance or use of this product.
"

test <- c(test1, test2)

tibble(details = test) |> 
  mutate(flavour = str_extract(details, "(?i)(Flavor\\s+Profile:.*?)(?=\\n|$)")) |> 
  mutate(details = str_remove(details, "(?i)(Flavor\\s+Profile:.*?)(?=\\n|$)")) |> 
  mutate(size = str_extract(details, "(?i)(Size:.*?)(?=\\n|$)")) |> 
  mutate(details = str_remove(details, "(?i)(Size:.*?)(?=\\n|$)")) |>
  mutate(vgpg = str_extract(details, "(?i)(VG/PG:.*?)(?=\\n|$)")) |>
  mutate(details = str_remove(details, "(?i)(VG/PG:.*?)(?=\\n|$)")) |>
  mutate(nicotine = str_extract(details, "(?i)(Nicotine\\s+(concentration|strength|salt\\sstrength):[^\\n]*)")) |> 
  mutate(details = str_remove(details, "(?i)(Nicotine\\s+(concentration|strength|salt\\sstrength):[^\\n]*)")) |> 
  select(everything(), details)

size <- str_extract(text, "(?i)(Size:.*?)(?=\\n|$)")

# Regular expression to match 'VG/PG:' followed by any text until line break
vgpg <- str_extract(text, "(?i)(VG/PG:.*?)(?=\\n|$)")



tibble(details = test) |> 
  rowwise() |> 
  mutate(nicotine1 = as.numeric(str_extract(str_extract_all(details, "\\b\\d+(\\.\\d+)?\\s*(mg/ml|mg/mL)\\b")[[1]][1], "\\d+(\\.\\d+)?"))) |> 
  mutate(nicotine2 = as.numeric(str_extract(str_extract_all(details, "\\b\\d+(\\.\\d+)?\\s*(mg/ml|mg/mL)\\b")[[1]][2], "\\d+(\\.\\d+)?"))) |> 
  mutate(nicotine3 = as.numeric(str_extract(str_extract_all(details, "\\b\\d+(\\.\\d+)?\\s*(mg/ml|mg/mL)\\b")[[1]][3], "\\d+(\\.\\d+)?"))) |> 
  mutate(nicotine4 = as.numeric(str_extract(str_extract_all(details, "\\b\\d+(\\.\\d+)?\\s*(mg/ml|mg/mL)\\b")[[1]][4], "\\d+(\\.\\d+)?"))) |> 
  mutate(nicotine5 = as.numeric(str_extract(str_extract_all(details, "\\b\\d+(\\.\\d+)?\\s*(mg/ml|mg/mL)\\b")[[1]][5], "\\d+(\\.\\d+)?"))) |> 
  mutate(nicotine6 = as.numeric(str_extract(str_extract_all(details, "\\b\\d+(\\.\\d+)?\\s*(mg/ml|mg/mL)\\b")[[1]][6], "\\d+(\\.\\d+)?"))) |> 
  mutate(nicotine7 = as.numeric(str_extract(str_extract_all(details, "\\b\\d+(\\.\\d+)?\\s*(mg/ml|mg/mL)\\b")[[1]][7], "\\d+(\\.\\d+)?"))) |> 
  mutate(nicotine8 = as.numeric(str_extract(str_extract_all(details, "\\b\\d+(\\.\\d+)?\\s*(mg/ml|mg/mL)\\b")[[1]][8], "\\d+(\\.\\d+)?"))) |> 
  mutate(nicotine9 = as.numeric(str_extract(str_extract_all(details, "\\b\\d+(\\.\\d+)?\\s*(mg/ml|mg/mL)\\b")[[1]][9], "\\d+(\\.\\d+)?"))) |> 
  mutate(nicotine10 = as.numeric(str_extract(str_extract_all(details, "\\b\\d+(\\.\\d+)?\\s*(mg/ml|mg/mL)\\b")[[1]][10], "\\d+(\\.\\d+)?"))) |>
  mutate(nicotine_min = min(nicotine1,nicotine2,nicotine3,nicotine4,nicotine5,nicotine6,nicotine7,nicotine8,nicotine9,nicotine10, na.rm = TRUE)) |> 
  mutate(nicotine_max = max(nicotine1,nicotine2,nicotine3,nicotine4,nicotine5,nicotine6,nicotine7,nicotine8,nicotine9,nicotine10, na.rm = TRUE)) |> 
  mutate(nicotine_salt_strength = str_detect(details, "Nicotine strength|Nicotine salt strength|Nicotine Salt strength|Nicotine Salt Strength|nicotine salt strength")) |> 
  mutate(nicotine_concentration = str_detect(details, "Nicotine concentration|Nicotine Concentration|nicotine concentration")) |> 
  mutate(nicotine_strength = str_detect(details, "Nicotine strength|Nicotine Strength|nicotine strength")) |> 
  glimpse()  

tibble(details = test) |> 
  # rowwise() |>
  mutate(nicotine = list(as.numeric(str_extract(str_extract_all(details, "\\b\\d+(\\.\\d+)?\\s*(mg/ml|mg/mL)\\b")[[1]], "\\d+(\\.\\d+)?")))) |>
  # mutate(nicotine_min = purrr:::map_dbl(nicotine, min)) |> 
  # mutate(nicotine_max = purrr:::map_dbl(nicotine, max)) |> 
  # glimpse()
  # 
  mutate(nicotine_salt_strength = str_detect(details, "Nicotine strength|Nicotine salt strength|Nicotine Salt strength|Nicotine Salt Strength|nicotine salt strength")) |> 
  mutate(nicotine_concentration = str_detect(details, "Nicotine concentration|Nicotine Concentration|nicotine concentration")) |> 
  mutate(nicotine_strength = str_detect(details, "Nicotine strength|Nicotine Strength|nicotine strength")) |> 
  glimpse()  
    
  # Load the stringr package
library(stringr)

extract_nicotine_info <- function(text) {
  # Regular expression to match 'Nicotine concentration:', 'Nicotine strength:', or 'Nicotine salt strength' followed by any text until line break
  nicotine_info <- str_extract_all(text, "(?i)(Nicotine\\s+(concentration|strength|salt\\sstrength):.*?)(?=\\n|$)")
  
  return(nicotine_info)
}

# Apply the function to the test text blocks
extract_nicotine_info(test)

shosha |> #distinct(category)
  # mutate(nicotine = str_extract_all(details, "(?i)(Nicotine\\s+(concentration|strength|salt\\sstrength):.*?)(?=\\n|$)")) |> 
  mutate(nicotine = str_extract_all(details, "(?i)(Nicotine\\s+(concentration|strength|salt\\sstrength):[^\\n]*)")) |> 
  filter(name == "frozen-salty-world-ice-strawberry-guava-e-liquid")  |> 
  select(nicotine) |> 
  unlist()


# Print the results
nicotine_info_test1
nicotine_info_test2