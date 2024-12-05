library(xml2)
library(tidyverse)

# sitemap <- read_xml("https://www.shosha.co.nz/sitemap_nz.xml")
sitemap <- read_xml(fs::path("shosha", "sitemap", "shosha-sitemap", ext = "xml"))

nodes <- sitemap |>
  xml_children() |>
  xml_contents()

urls <- xml_text(nodes) |>
  str_subset("^https") |>
  str_subset("/cache/", negate = TRUE) |> 
  as_tibble() |> 
  rename(url = value) 

#as products are listed as direct children of https://www.shosha.co.nz/
#we can filter so that only urls with 3 slashs are included
#this allows approx 15% of redundant urls to be removed 

urls <- urls |>
  mutate(slash_count = str_count(url, pattern = "/")) |>
  mutate(slash_count_3 = slash_count == 3) |>
  filter(slash_count_3) |>
  select(-slash_count, -slash_count_3)

urls
# urls |> view()

f <- fs::path("shosha", "data", "shosha_urls", ext = "csv")
readr::write_csv(urls, file = f)
