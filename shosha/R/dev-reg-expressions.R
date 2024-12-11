library(tidyverse)

tibble(
  details_text2 = "blah\nSize: 50 ml, 60 mL \nBlah"
) |> 
  mutate(
    # Extract the size information (everything after "Size:" and up to the next newline or the end of the string)
    size_values_details = str_trim(str_extract(details_text2, "(?i)Size:.*?(?=\\n|$\\|)")),
    # Extract all numeric values for the sizes (ignoring 'mL' or 'ml')
    size_num_details = str_extract_all(size_values_details, "\\d+(\\.\\d+)?(?=\\s?[mM][lL])") |> 
      map(~ as.double(.)),
    # Calculate min and max for each vector of sizes
    size_min_details = map_dbl(size_num_details, ~ min(.x, na.rm = TRUE)),
    size_max_details = map_dbl(size_num_details, ~ max(.x, na.rm = TRUE))
  ) |> 
  glimpse()

shosha_cleaned |> 
  distinct(category_text )

shosha_cleaned |> 
  filter(category_text == "Vape Devices") |> 
  filter(disposable_keyword)

url_check <- "https://www.shosha.co.nz/ice-edition-raspberry-lime-salty-pulse-replacement-pod"
url_check <-"https://www.shosha.co.nz/salty-get-bar-mint-grape-disposable-vape"
url_check <-"https://www.shosha.co.nz/ice-edition-raspberry-lime-salty-pulse-replacement-pod"
url_check <-"https://www.shosha.co.nz/ultra-bar-green-grape-disposabe-vape"

shosha_cleaned |> 
  filter(url == url_check)  |> 
  glimpse()

shosha_cleaned |> 
  count(disposable_keyword)

#only 74 of 

shosha_cleaned |> 
  select(name_text, category_text, details_text, buttons_text) |> 
  mutate(disposable_keyword = str_detect(details_text, "(?i)(disposable)") | str_detect(name_text, "(?i)(disposable)")) |> 
  mutate(replacement_pod_keyword = str_detect(details_text, fixed("replacement pod", ignore_case = TRUE)) | str_detect(name_text, fixed("replacement pod", ignore_case = TRUE))) |> 
  # mutate(disposable_keyword = str_detect(details_text, "(?i)(disposable)") | str_detect(name_text, "(?i)(disposable)")) |> 
  # mutate(disposable_name = str_detect(name_text, "(?i)(disposable)")) |> 
  filter(disposable_keyword | replacement_pod_keyword)



