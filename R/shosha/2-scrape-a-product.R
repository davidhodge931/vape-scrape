library(tidyverse)
library(chromote)
library(rvest)

url <- "https://www.shosha.co.nz/sour-apple-salty-smart-bar-replacement-pod" #works
# url <- "https://www.shosha.co.nz/tote-aoturoa-green-grape-freebase-e-liquid-30ml" #works 
# url <- "https://www.shosha.co.nz/brands/freebase-e-liquids-brands/tote-aoturoa-green" #doesn't NOT A PRODUCT
# url <- "https://www.shosha.co.nz/smok-mag-solo-kit" #works
# url <- "https://www.shosha.co.nz/tote-aoturoa-green-grape-nicotine-salt-e-liquid-30ml"
# url <- urls[991]

url <- "https://www.shosha.co.nz/tote-aoturoa-green-grape-freebase-e-liquid-30ml"

url_html_live <- url |> read_html_live()

#name
name <- url_html_live |> 
  html_elements("h1.productFullDetail-productName-6ZL")  |> 
  html_text2()

#brand
brand <- url_html_live |> 
  html_elements("div.productFullDetail-brand-zRg")  |> 
  html_text2() |> 
  str_sub(4) |> #remove ' by'  
  str_trim()

#category
url_html_live |> 
  html_elements("a.breadcrumbs-link-mHX breadcrumbs-text-lAa")   
  # html_text2()

# url |> 
#   read_html_live() |> 
#   html_element("div.productFullDetail-brand-zRg") |> 
#   html_text2() 
#   
# <a class="a.breadcrumbs-link-mHX breadcrumbs-text-lAa"

#details
details <- url_html_live |> 
  html_elements(".richContent-root-CMO p") |> 
  html_text2()

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

vg_pg_keywords <- glue::glue_collapse(c(
  "VG/PG:|Vg/Pg:|vg/pg:"
), sep = "|")

vg_pg <- details |> 
  str_subset(vg_pg_keywords) |> 
  str_remove(vg_pg_keywords) |> 
  str_trim()

if (vctrs::vec_is_empty(vg_pg)) vg_pg <- NA

tibble(
 name = name,
 brand = brand,
 nicotine = nicotine,
 flavour = flavour,
 vg_pg = vg_pg,
) 
