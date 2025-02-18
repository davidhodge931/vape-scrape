library(tidyverse)
library(chromote)
library(rvest)

url <- "https://www.vapo.co.nz/products/vapo-eliquid-tobacco"
url <- "https://www.vapo.co.nz/products/bud-replacement-pod-tobacco"
url <- "https://www.vapo.co.nz/products/solo-peppermint-disposable-vape" 
url <- "https://www.vapo.co.nz/products/vapo-eliquid-tobacco"

# browseURL(url)

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
  html_elements(".product--text.style_vendor a.product-vendor--link")  |> 
  html_text2() 

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
buttons1 <- url_html_live |>
  html_elements("select#Option-template--16181853814923__main-product-0") |> 
  html_text2() 

if (vctrs::vec_is_empty(buttons1)) buttons1 <- NA

buttons1

buttons2 <- url_html_live |>
  html_elements("select#Option-template--16181853814923__main-product-1") |> 
  html_text2() 

if (vctrs::vec_is_empty(buttons2)) buttons2 <- NA

buttons <- str_flatten(string = c(buttons1, buttons2), collapse = " \n ")

#details
details <- url_html_live |> 
  html_elements("div.metafield-rich_text_field") |> 
  html_text2() |> 
  str_flatten(collapse = "\n")

#bind together
tibble(
  name = name,
  brand = brand,
  category = category,
  price = price,
  buttons = buttons,
  details = details,
) 

