library(tidyverse)
library(chromote)
library(rvest)

# url <- "https://www.shosha.co.nz/sour-apple-salty-smart-bar-replacement-pod" #works
# url <- "https://www.shosha.co.nz/tote-aoturoa-green-grape-freebase-e-liquid-30ml" #works 
# url <- "https://www.shosha.co.nz/brands/freebase-e-liquids-brands/tote-aoturoa-green" #doesn't NOT A PRODUCT
# url <- "https://www.shosha.co.nz/smok-mag-solo-kit" #works
# url <- "https://www.shosha.co.nz/tote-aoturoa-green-grape-nicotine-salt-e-liquid-30ml"
url <- "https://www.shosha.co.nz/tote-aoturoa-green-grape-freebase-e-liquid-30ml"
url <-"https://shoshavape.co.nz/products/salty-smart-bar-watermelon-strawberry-disposable-vape?variant=48943386034455"

url_html_live <- url |> 
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

if (vctrs::vec_is_empty(category)) category <- NA

category

#price
price <- url_html_live |>
  html_elements("div.productFullDetail-price-p6T") |> 
  html_text2() 

if (vctrs::vec_is_empty(price)) price <- NA

price

#buttons
buttons <- url_html_live |>
  html_elements("button.tileNicotine-root-syX span") |> 
  html_text2() |>
  stringr::str_flatten_comma()

if (vctrs::vec_is_empty(buttons)) buttons <- NA

buttons

#details
details <- url_html_live |> 
  html_elements(".richContent-root-CMO p") |> 
  html_text2()

#bind together
tibble(
  name = name,
  brand = brand,
  category = category,
  price = price,
  buttons = buttons,
  details = details,
) 

