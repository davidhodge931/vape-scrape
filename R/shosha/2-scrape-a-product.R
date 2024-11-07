library(tidyverse)
library(chromote)
library(rvest)

url <- "https://www.shosha.co.nz/sour-apple-salty-smart-bar-replacement-pod" #works
url <- "https://www.shosha.co.nz/tote-aoturoa-green-grape-freebase-e-liquid-30ml" #works 
url <- "https://www.shosha.co.nz/brands/freebase-e-liquids-brands/tote-aoturoa-green" #doesn't NOT A PRODUCT
url <- "https://www.shosha.co.nz/smok-mag-solo-kit" #works
url <- "https://www.shosha.co.nz/tote-aoturoa-green-grape-nicotine-salt-e-liquid-30ml"
# url <- urls[991]

#product name
name <- url |> 
  read_html_live() |> 
  html_nodes("h1.productFullDetail-productName-6ZL")  |> 
  html_text()

#product brand
brand <- url |> 
  read_html_live() |> 
  html_nodes("div.productFullDetail-brand-zRg")  |> 
  html_text() |> 
  str_sub(4) |> #remove ' by'  
  str_trim()

#details list#details listinfert
details <- url |> 
  read_html_live() |> 
  html_nodes(".richContent-root-CMO p")  |> 
  html_text()

flavour <- details |> 
  str_subset("Flavor Profile:") |> 
  str_remove("Flavor Profile:") |> 
  str_trim()

nicotine <- details |> 
  str_subset("Nicotine Concentration:") |> 
  str_remove("Nicotine Concentration:") |> 
  str_trim()

vg_pg <- details |> 
  str_subset("VG/PG:") |> 
  str_remove("VG/PG:") |> 
  str_trim()

tibble(
 name = name,
 brand = brand,
 nicotine = nicotine,
 flavour = flavour,
 vg_pg = vg_pg,
) 
