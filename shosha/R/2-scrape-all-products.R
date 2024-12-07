library(tidyverse)
library(chromote)
library(rvest)
library(xml2)
library(purrr)
library(fs)
library(polite)

# 1: read in URL's to loop through
# 2: read in functions to extract html elements with built in error handling/retries "wait_for_elements", and get all the data with error handling/retries and bind to list "get_html_with_retry"
# 3: loop extract: re-initialise chromote - have a longer sleep after x iterations - save rows to csv as we go - and retry if error occurs in loop

latest_run <- fs::dir_ls(fs::path("shosha", "data")) |>
  as_tibble() |>
  mutate(value = fs::path_sanitize(str_remove(value, fs::path("shosha", "data") ))) |>
  slice_max(value) |>
  pull()

f <- fs::path("shosha", "data", latest_run, "scraped", ext = "csv") # file path to save scraped data into

################################################################################
# save the sitemap locally within this folder - and name as shosha-sitemap
################################################################################
# read_xml("https://www.shosha.co.nz/sitemap_nz.xml") #blocked
sitemap <- read_xml(fs::path("shosha", "data", latest_run, "sitemap_nz", ext = "xml"))

nodes <- sitemap |>
  xml_children() |>
  xml_contents()

urls <- xml_text(nodes) |>
  str_subset("^https") |>
  str_subset("/cache/", negate = TRUE) |> 
  as_tibble() |> 
  rename(url = value) 

#as products are listed as direct children of https://www.shosha.co.nz/
#filter so that only urls with 3 slashs are included, as others do not relate to products 

urls <- urls |>
  mutate(slash_count = str_count(url, pattern = "/")) |>
  mutate(slash_count_3 = slash_count == 3) |>
  filter(slash_count_3) |>
  select(-slash_count, -slash_count_3)

urls <- urls |> pull()

if (test) {
  urls <- c(urls[c(340:400)],
            "https://www.shosha.co.nz/salty-pulse-bar-watermelon-peach-ice-disposable-vape")
}

#show the polite scraping settings for this page - suggests 5 second delay in this case
print(bow("https://www.shosha.co.nz/"))

# --- Function to wait for elements to appear when html_elements() used --------
wait_for_elements <- function(url_html_live, selector, timeout = 3, sleep = 2 * 2) {
  start_time <- Sys.time()
  while (TRUE) { # This sets up an infinite loop that will keep trying to extract the elements. The loop will only exit when the elements are found OR the timeout expires.
    # Try to extract the elements
    elements <- url_html_live %>%
      html_elements(selector)
    
    # If the elements are found or the timeout is reached, return the result
    if (length(elements) > 0 || as.numeric(difftime(Sys.time(), start_time, units = "secs")) > timeout) {
      return(elements)
    }
    
    # Otherwise, wait and we will have another crack
    Sys.sleep(sleep)
    cat("---------- Didn't get html elements for: ", selector, ". It may not exist, but having another go ----------", "\n")
  }
}
# ------------------------------------------------------------------------------

# --- Function to get the data with retry logic - with tryCatch ----------------
# if chromote connection fails and uses wait_for_elements() above to wait 
# until elements are retrieved (since sometimes they are not retrieved 
# on the first go for some reason)
get_html_with_retry <- function(url, retries = 3, delay = 5 * 2) {
  
  attempt <- 1
  while(attempt <= retries) { # loop for number of iterations set in retries 
    
    try({
      # Attempt to scrape the page using read_html_live
      url_html_live <- read_html_live(url)  # Use rvest read_html_live to scrape the JS generated page
      
      Sys.sleep(delay)  # Polite delay as per: print(bow("https://www.shosha.co.nz/"))
      
      #get product name
      name <- url %>% sub(".*/", "",.)
      
      #get product category
      category <- wait_for_elements(url_html_live, "a.breadcrumbs-link-mHX.breadcrumbs-text-lAa") |> 
        html_text2() |> 
        magrittr::extract(2)
      
      #get price
      price <- wait_for_elements(url_html_live, "div.productFullDetail-price-p6T") |> 
        html_text2() 
      
      #get buttons
      buttons <- wait_for_elements(url_html_live, "button.tileNicotine-root-syX span") |> 
        html_text2() |> 
        stringr::str_flatten_comma()
      
      #get prod details
      details1 <- wait_for_elements(url_html_live, ".richContent-root-CMO p") |> 
        html_text2() %>% paste(collapse = "| ")
      #some details are in separate lists so cannot be extracted with ".richContent-root-CMO p" above
      details2 <- wait_for_elements(url_html_live, ".richContent-root-CMO p + ul") |> 
        html_text2() %>% paste(collapse = "| ")
      #and again some details cannot be extracted with above so add this too
      details3 <- wait_for_elements(url_html_live, "div.richContent-root-CMO") |> 
        html_text2() %>% paste(collapse = "| ")
      
      details <- paste(details1,details2,details3,collapse = "| ")
      
      
      return(list(name = name, category = category, price = price, buttons = buttons, details = details))  #return extracted data and end the loop
    }, silent = TRUE) #end of try with error messages suppressed
    
    # If we reach this point, the attempt failed - wait and try again - but if attempt > retries then stop with message
    if (attempt < retries) {
      message(paste("Attempt", attempt, "failed. Retrying in", delay, "seconds..."))
      Sys.sleep(delay)  # Delay before having another go
      attempt <- attempt + 1
    } else {
      stop(paste("All", retries, "retries failed for URL:", url))
    }
  }
}
# ------------------------------------------------------------------------------

# --- loop with imap to keep track of iteration --------------------------------
imap(urls, function(x, i) {
  # Wrap the entire iteration in tryCatch to handle errors anywhere in the loop
  tryCatch({
    # print(paste("Loop number:", i + last_saved_row)) # Adjust loop number based on last saved row - ie if it restarted after error it would be using a last_saved_row > 0
    print(paste("Loop number:", i)) # Adjust loop number based on last saved row - ie if it restarted after error it would be using a last_saved_row > 0
    
    # Clean up any previous connections
    if (exists("url_html_live")) {
      tryCatch({
        url_html_live$session$close()  # Close the chromote session
        message("Chromote session cleared successfully")
      }, error = function(e) {
        message("Error closing the session: ", e$message)
      })
      rm(url_html_live)
    }
    
    # Remove previous objects - prob not neded
    if (exists("name")) {rm(name)}
    if (exists("category")) {rm(category)}
    if (exists("price")) {rm(price)}
    if (exists("buttons")) {rm(buttons)}
    if (exists("details")) {rm(details)}
    
    # Politely sleep longer after every 30th iteration - so once in a while we pause the scraping for longer- we may not need such a long sleep here?
    if (i %% 30 == 0) {
      message("Sleeping for 10 minutes after every 30 iterations.")
      Sys.sleep(600)
    }
    
    page_data <- get_html_with_retry(x) #run get_html_with_retry() which saves data to a list which we extract below
    
    # Extracted data from page_data
    name <- page_data$name
    category <- page_data$category
    price <- page_data$price
    buttons <- page_data$buttons
    details <- page_data$details
    
    print(paste("URL:",x))
    print(paste("Name:", name)) #so we can see how we are tracking
    print(paste("Category:", category)) #so we can see how we are tracking
    print(paste("Price:", price)) #so we can see how we are tracking
    print(paste("Buttons:", buttons)) #so we can see how we are tracking
    print(paste("Details (first only printed):", head(details,1)))
    
    # Create a tibble for the current iteration
    current_data <- tibble(
      name = name,
      category = category,
      price = price,
      buttons = buttons,
      details = details
    )
    
    # Append to CSV (write csv if it's the first loop, otherwise append it's not the first loop - so adds new row to the existing file and re-saves)
    # this is so if there is an error and the loop breaks we have saved all data up to that point and the trycatch will then wait for 10mins and start the loop again
    # but from where we left off
    # if (i + last_saved_row == 1) {
    if (i == 1) {
      readr::write_csv(current_data, file = f)
    } else {
      readr::write_csv(current_data, file = f, append = TRUE, col_names = FALSE)
    }
    
  }, error = function(e) {
    # If any error occurs during the loop then sleep for 10 minutes, and retry
    # message("Error occurred in iteration ", i + last_saved_row, ": ", e$message)
    message("Error occurred in iteration ", i, ": ", e$message)
    message("Sleeping for 10 minutes and will retry this iteration.")
    Sys.sleep(600)
    return(NULL)  # This will retry the current iteration after the 10-minute sleep
  })
  
}, .progress = TRUE)
# ------------------------------------------------------------------------------
