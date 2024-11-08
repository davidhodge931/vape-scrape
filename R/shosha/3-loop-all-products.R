library(tidyverse)
library(chromote)
library(rvest)
library(purrr)

# urls <- c("https://www.shosha.co.nz/sour-apple-salty-smart-bar-replacement-pod",
#           "https://www.shosha.co.nz/tote-aoturoa-green-grape-freebase-e-liquid-30ml")

urls_path <- fs::path("data", "shosha_urls", ext = "csv")

urls <- readr::read_csv(file = urls_path) 

urls <- urls |> 
  pull() |> 
  magrittr::extract(1:100)

d <- map(urls, function(x) {

  url_html_live <- x |> 
    read_html_live()
  
  # Sys.sleep(10) #otherwise category fails
  
  #name
  name <- url_html_live |> 
    html_elements("h1.productFullDetail-productName-6ZL")  |> 
    html_text2()
  
  if (vctrs::vec_is_empty(name)) name <- NA
  
  name
  
  #brand
  brand <- url_html_live |> 
    html_elements("div.productFullDetail-brand-zRg")  |> 
    html_text2() |> 
    str_sub(4) |> #remove ' by'  
    str_trim()
  
  if (vctrs::vec_is_empty(brand)) brand <- NA
  
  brand 
  
  #category
  category <- url_html_live |>
    html_elements("a.breadcrumbs-link-mHX.breadcrumbs-text-lAa") |> 
    html_text2() |> 
    magrittr::extract(2) 
  # str_subset("Home", negate = TRUE)
  
  # if (vctrs::vec_is_empty(category)) category <- NA
  
  category
  
  #details
  details <- url_html_live |> 
    html_elements(".richContent-root-CMO p") |> 
    html_text2()
  
  #flavour
  flavour_keywords <- glue::glue_collapse(c(
    "Flavor Profile:|Flavor profile:|flavor profile:", 
    "Flavors Profile:|Flavors profile:|flavors profile:", 
    "Flavour Profile:|Flavour profile:|flavour profile:",
    "Flavours Profile:|Flavours profile:|flavours profile:", 
    "Flavor:|flavor:", 
    "Flavour:|flavour:" 
  ), sep = "|")
  
  flavour <- details |> 
    str_subset(flavour_keywords) |> 
    str_remove(flavour_keywords) |> 
    str_trim()
  
  if (vctrs::vec_is_empty(flavour)) flavour <- NA
  
  flavour
  
  #nicotine
  nicotine_keywords <- glue::glue_collapse(c(
    "Nicotine Concentration:|Nicotine concentration:|nicotine concentration:",
    "Nicotine Concentration:|Nicotine concentration:|nicotine concentration:", 
    "Nicotine Strength:|Nicotine strength:|nicotine strength:", 
    "Nicotine Strength:|Nicotine strength:|nicotine strength:",
    "Nicotine:|nicotine:"
  ), sep = "|")
  
  nicotine <- details |> 
    str_subset(nicotine_keywords) |> 
    str_remove(nicotine_keywords) |> 
    str_trim()
  
  if (vctrs::vec_is_empty(nicotine)) nicotine <- NA
  
  #vgpg
  vg_pg_keywords <- glue::glue_collapse(c(
    "VG/PG:|Vg/Pg:|vg/pg:"
  ), sep = "|")
  
  vg_pg <- details |> 
    str_subset(vg_pg_keywords) |> 
    str_remove(vg_pg_keywords) |> 
    str_trim()
  
  if (vctrs::vec_is_empty(vg_pg)) vg_pg <- NA
  
  name
  brand
  category
  nicotine
  flavour
  vg_pg
  
  #bind together
  tibble(
    name = name,
    brand = brand,
    category = category,
    nicotine = nicotine,
    flavour = flavour,
    vg_pg = vg_pg,
  ) 
}) |> 
  purrr::list_rbind()

d

f <- fs::path("data", "shosha", ext = "csv")
readr::write_csv(d, file = f)
