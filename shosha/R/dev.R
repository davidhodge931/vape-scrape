d |> 
  select(
    name, contains("nicotine_"), contains("size_") 
  ) |> #glimpse() 
  mutate(
    estimate_nicotine_values = case_when(
      !is.na(buttons_nicotine_values) ~ buttons_nicotine_values,
      !is.na(details_nicotine_values) ~ details_nicotine_values,
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
    estimate_size_values = case_when(
      !is.na(buttons_size_values) ~ buttons_size_values,
      !is.na(details_size_values) ~ details_size_values,
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
  )

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


d |> 
  select(name, category, price_num, contains("estimate"), contains("flavour"), contains("disposable")) |> 
  mutate(details_flavour = details_flavour) |> 
  mutate(details_flavour = str_replace_all(details_flavour, ", &", ",")) |>
  mutate(details_flavour = str_replace_all(details_flavour, " &", ",")) |>
  mutate(details_flavour = str_replace_all(details_flavour, ", and ", ", ")) |>
  mutate(details_flavour = str_replace_all(details_flavour, " and ", ", ")) |>
  mutate(details_flavour = str_replace_all(details_flavour, ",,", ",")) |>
  mutate(details_flavour = str_replace_all(details_flavour, ";", ",")) |>
  mutate(
    estimate_flavours_all_approved = 
      ifelse(is.na(details_flavour2), NA,test |> 
               str_split(",\\s*") |> 
               map_lgl(~ all(.x %in% flavours_approved)) 
             )
  ) |> 
  mutate(estimate_flavours_count = str_count(details_flavour2, ",") + 1) |> 
  select(-details_flavour2)



str_detect(test, approved_flavours)



library(dplyr)
library(stringr)
library(purrr)

# Example vector of unstructured text
flavours_text <- c(NA, "Red Apple, Green Apple", "Watermelon", "Watermelon, Mint", 
                   "Gum, Mint", "Orange, Mint", "Red Apple, Green Apple, Mint", 
                   "Gum", "Blueberry, Mint", "Mint", "Watermelon, Strawberry", 
                   "Lemon, Raspberry", "Fuji Apple, Strawberry, Nectarine", 
                   "Starfruit, Grape", "Sweet Tobacco", "Mint, Tobacco", "Tobacco", 
                   "Vanilla", "Cola, Lemon", "Mango", "Lemon, Cola", "Cool Refreshing Mint", 
                   "Peach", "Apple", "Fruits, Energy Drink", "Traditional Tobacco", 
                   "Coffee", "Sweet, Sour Berry", "Condensed Milk, Vanilla", "Grape")

# Vector of approved flavours
# approved_flavours <- c("Red Apple", "Green Apple", "Watermelon", "Mint", "Gum", 
#                        "Orange", "Blueberry", "Strawberry", "Lemon", "Raspberry", 
#                        "Fuji Apple", "Nectarine", "Starfruit", "Grape", "Sweet Tobacco", 
#                        "Tobacco", "Vanilla", "Cola", "Mango", "Peach", "Energy Drink", 
#                        "Traditional Tobacco", "Coffee", "Sour Berry", "Condensed Milk")

# Function to check if all flavours in a string

check_all_approved <- function(text, approved) {
  if (is.na(text)) {
    return(NA)  # Handle missing values
  }
  
  # Split the flavours in the text (split by comma and trim spaces)
  flavours_in_text <- str_split(text, ",\\s*")[[1]]
  
  # Check if each flavour in the text is in the approved list
  
}

# Apply the function to the flavours_text vector
 map_lgl(flavours_text, check_all_approved, approved_flavours)


 
 tibble()