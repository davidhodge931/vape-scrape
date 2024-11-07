library(xml2)
library(tidyverse)

sitemap <- read_xml("https://www.shosha.co.nz/sitemap_nz.xml")

nodes <- sitemap |>
  xml_children() |>
  xml_contents()

urls <- xml_text(nodes) |>
  str_subset("^https") |>
  str_subset("/cache/", negate = TRUE) |> 
  as_tibble() |> 
  rename(url = value)

f <- fs::path("data", "shosha_urls", ext = "csv")
readr::write_csv(urls, file = f)

