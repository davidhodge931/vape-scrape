library(tidyverse)
library(chromote)
library(rvest)

url <- "https://www.vapo.co.nz/products/vapo-eliquid-tobacco"

url_html_live <- url |> 
  read_html_live()

# Sys.sleep(10) #otherwise category fails

#name
name <- url_html_live |> 
  html_elements("h1.product-title")  |> 
  html_text2()

if (vctrs::vec_is_empty(name)) name <- NA

name

#brand 
brand <- url_html_live |> 
  html_elements("p.product--text")  |> 
  html_text2() |> 
  str_sub(4) |> #remove ' by'  
  str_trim()

if (vctrs::vec_is_empty(brand)) brand <- NA

brand 

#category 
category <- url_html_live |>
  html_elements("nav.breadcrumbs.override--section") |> 
  html_text2() #|> 
  # magrittr::extract(2) 

if (vctrs::vec_is_empty(category)) category <- NA

category

#price
price <- url_html_live |>
  html_elements("span.amount") |> 
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
  html_elements("div.metafield-rich_text_field") |> 
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

