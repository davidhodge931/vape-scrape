library(xml2)
library(tidyverse)

sitemap <- read_xml("https://www.shosha.co.nz/sitemap_nz.xml")

nodes <- sitemap |>
  xml_children() |>
  xml_contents()

urls <- xml_text(nodes) |>
  str_subset("^https") |>
  str_subset("/cache/", negate = TRUE)

urls
