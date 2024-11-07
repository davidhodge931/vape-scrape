library(tidyverse)
library(chromote)
library(rvest)

url <- "https://www.shosha.co.nz/sour-apple-salty-smart-bar-replacement-pod" #works
url <- "https://www.shosha.co.nz/tote-aoturoa-green-grape-freebase-e-liquid-30ml" #works 
url <- "https://www.shosha.co.nz/brands/freebase-e-liquids-brands/tote-aoturoa-e-liquids" #doesn't
url <- "https://www.shosha.co.nz/smok-mag-solo-kit" #works

url <- urls[991]

url |> 
  read_html_live() |> 
  html_elements(".richContent-root-CMO p")  |> 
  html_text2() 

#product name
url |> 
  read_html_live() |> 
  html_nodes("h1.productFullDetail-productName-6ZL")  |> 
  html_text2()

#details list
url |> 
  read_html_live() |> 
  html_nodes(".richContent-root-CMO p")  |> 
  html_text2()

