# Scraping process
# E: clean data to extract nicotine, flavour, price, vgpg, etc  

# 1. Generate a unique folder name and file path
timestamp <- format(Sys.time(), "%Y-%m-%d %H-%M-%S")  
folder_path <- fs::path("shosha", "data", timestamp)
fs::dir_create(folder_path) #create dir to save data into

#2. save sitemap into latest data folder

#3. run other scripts

scripts <- fs::dir_ls(fs::path("shosha", "R")) |>
  str_subset("z-archive", negate = TRUE) |>
  str_subset("0-run", negate = TRUE) 

purrr::walk(scripts, \(x) source(x))

