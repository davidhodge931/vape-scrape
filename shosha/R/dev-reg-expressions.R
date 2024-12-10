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












tibble(
  details_text2 = "blah : 50ml, 60 mL \n Blah"
) |> 
  