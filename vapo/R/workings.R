library(tidyverse)
if (length(str_split(name, pattern = " \\| ")) == 2) {
  str_split(name, pattern = " \\| ")[[1]][1]
}

tibble(name = c("Tobacco | VAPO E-Liquid",  "Tobacco | Bud Replacement", "Peppermint | solo Dispo")) |> 
  mutate(name_length = str_count(name, "\\|") + 1) |> 
  mutate(flavours_text = ifelse(name_length == 2, str_extract(name, ".*(?= \\|)"), NA))

# str_extract(name, "(?<=\\| ).*")
mutate(extracted = str_extract(name, ".*(?= \\|)"))
