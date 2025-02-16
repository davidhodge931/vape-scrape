# Scraping process
library(tidyverse)
time_start <- Sys.time()

################################################################################
name <- "Vapo"
url <- "https://www.vapo.co.nz"
sitemap <- "https://www.vapo.co.nz/sitemap_products_1.xml?from=4013452164&to=7614385815691"
################################################################################

# 1. Generate a unique folder name and file path
timestamp <- format(Sys.time(), "%Y-%m-%d %H-%M-%S")  
folder_path <- fs::path(str_to_lower(name), "data", timestamp)
fs::dir_create(folder_path) #create dir to save data into

#2. mannually save sitemap into data subfolder with latest timestamp
# browseURL("https://www.vapo.co.nz/sitemap.xml")
browseURL(sitemap)

#3. scrape the data and then clean into an output dataset
scripts <- fs::dir_ls(fs::path(str_to_lower(name), "R")) |>
  str_subset("z-archive", negate = TRUE) |>
  str_subset("0-run", negate = TRUE) 

purrr::walk(scripts, \(x) source(x))

time_end <- Sys.time()
time_duration <- time_end - time_start
tume_duration
