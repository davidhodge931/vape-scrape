library(tidyverse)
library(chromote)
library(rvest)
library(purrr)

urls_test <- c("https://www.shosha.co.nz/sour-apple-salty-smart-bar-replacement-pod",
           "https://www.shosha.co.nz/tote-aoturoa-green-grape-freebase-e-liquid-30ml"
           )

map(urls_test, function(x) {
  #name
  name <- x |> 
    read_html_live() |> 
    html_nodes("h1.productFullDetail-productName-6ZL")  |> 
    html_text2()
  
  #brand
  brand <- x |>
    read_html_live() |>
    html_nodes("div.productFullDetail-brand-zRg")  |>
    html_text2() |>
    str_sub(4) |> #remove ' by'
    str_trim()
  
  #details
  details <- x |>
    read_html_live() |>
    html_nodes(".richContent-root-CMO p")  |>
    html_text2()
  
  flavour <- details |> 
    str_subset("Flavor Profile:|Flavors Profile:|Flavour Profile:|Flavours Profile:") |> 
    str_remove("Flavor Profile:|Flavors Profile:|Flavour Profile:|Flavours Profile:") |> 
    str_trim()
  
  if (vctrs::vec_is_empty(flavour)) flavour <- NA
  
  nicotine <- details |> 
    str_subset("Nicotine Concentration:|Nicotine concentration:|nicotine concentration:") |> 
    str_remove("Nicotine Concentration:|Nicotine concentration:|nicotine concentration:") |> 
    str_subset("Nicotine:|nicotine:") |> 
    str_trim()
  
  if (vctrs::vec_is_empty(nicotine)) nicotine <- NA
  
  vg_pg <- details |> 
    str_subset("VG/PG:|Vg/Pg:|vg/pg:|Vg/pG:") |> 
    str_remove("VG/PG:|Vg/Pg:|vg/pg:|Vg/pG:") |> 
    str_trim()
  
  if (vctrs::vec_is_empty(vg_pg)) vg_pg <- NA
  
  tibble(
    name = name,
    brand = brand,
    nicotine = nicotine,
    flavour = flavour,
    vg_pg = vg_pg,
  )
})
