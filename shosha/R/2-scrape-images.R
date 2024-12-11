library(tidyverse)
library(chromote)
library(rvest)
library(xml2)
library(purrr)
library(fs)
library(polite)

latest_run <- fs::dir_ls(fs::path("shosha", "data")) |>
  as_tibble() |>
  mutate(value = fs::path_sanitize(str_remove(value, fs::path("shosha", "data") ))) |>
  slice_max(value) |>
  pull()

sitemap <- read_xml(fs::path("shosha", "data", latest_run, glue::glue("shosha-sitemap-{str_sub(latest_run, 1, 10)}"), ext = "xml"))

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

################################################################################
if (test) {
  urls <- c(urls[c(340:360)],
            "https://www.shosha.co.nz/salty-pulse-bar-watermelon-peach-ice-disposable-vape")
}

folder_path <- fs::path("shosha", "data", latest_run, "images")
fs::dir_create(folder_path)

#show the polite scraping settings for this page - suggests 5 second delay in this case
print(bow("https://www.shosha.co.nz/"))




# --- Function to wait for elements to appear when html_elements() used --------
wait_for_elements <- function(url_html_live, selector, timeout = 3, sleep = 2) {
  start_time <- Sys.time()
  while (TRUE) { # This sets up an infinite loop that will keep trying to extract the elements. The loop will only exit when the elements are found OR the timeout expires.
    # Try to extract the elements
    elements <- url_html_live %>%
      html_elements(selector)
    
    # If the elements are found or the timeout is reached, return the result
    if (length(elements) > 0 || as.numeric(difftime(Sys.time(), start_time, units = "secs")) > timeout) {
      return(elements) #return breaks the loop
    }
    
    # Otherwise, wait and we will have another crack
    Sys.sleep(sleep)
    cat("---------- Didn't get html elements for: ", selector, ". It may not exist, but having another go ----------", "\n")
  }
}
# ------------------------------------------------------------------------------


`


# --- Function to get the data with retry logic - with tryCatch ----------------
# if chromote connection fails and uses wait_for_elements() above to wait 
# until elements are retrieved (since sometimes they are not retrieved 
# on the first go for some reason)
get_html_with_retry <- function(url, retries = 3, delay = 5) {
  
  attempt <- 1
  while(attempt <= retries) { # loop for number of iterations set in retries 
    
    try({
      # Attempt to scrape the page using read_html_live
      url_html_live <- read_html_live(url)  # Use rvest read_html_live to scrape the JS generated page
      
      Sys.sleep(5)  # Polite delay as per: print(bow("https://www.shosha.co.nz/"))
      
      image <- wait_for_elements(url_html_live, "div.carousel2-imageContainer-Xbc img") |> 
        html_attr("src")
      image_url <- image[grepl("^https?://", image)][1] #get the first URL from the extracted html elements
      
      return(list(image_url = image_url))  #return extracted data and end the loop
    }, silent = TRUE) #end of try with error messages suppressed
    
    # If we reach this point, the attempt failed - wait and try again - but if attempt > retries then stop with message
    if (attempt < retries) {
      message(paste("Attempt", attempt, "failed. Retrying in", delay, "seconds..."))
      Sys.sleep(delay)  # Delay before having another go
      attempt <- attempt + 1 #iterate attempt number
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
    
    # Clean up any previous connections - this step may not be needed but it seems a clean way to do it
    if (exists("url_html_live")) {
      tryCatch({
        url_html_live$session$close()  # Close the chromote session
        message("Chromote session cleared successfully")
      }, error = function(e) {
        message("Error closing the session: ", e$message)
      })
      rm(url_html_live)
    }
    
    # Remove previous objects - prob not needed but again, it seems a clean way to do it
    if (exists("image_url")) {rm(image_url)}
    
    # Politely sleep longer after every 30th iteration - so once in a while we pause the scraping for longer- we may not need such a long sleep here?
    if (i %% 30 == 0) {
      message("Sleeping for 10 minutes after every 30 iterations.")
      Sys.sleep(600)
    }
    
    page_data <- get_html_with_retry(x) #run get_html_with_retry() which saves data to a list which we extract below
    
    # Extracted data from page_data
    image_url <- page_data$image_url
    
    print(paste("URL:",x))
    print(paste("Image url:", image_url)) #so we can see how we are tracking
    
    filename <- paste0(folder_path, "/", basename(x), ".png")
    if(!is.na(image_url)){download.file(image_url, destfile = filename, mode = 'wb')}
    
  }, error = function(e) {
    # If any error occurs during the loop then sleep for 10 minutes, and retry
    # message("Error occurred in iteration ", i + last_saved_row, ": ", e$message)
    message("Error occurred in iteration ", i, ": ", e$message)
    message("Sleeping for 10 minutes and will retry this iteration.")
    Sys.sleep(600)
    return(NULL)  # This will retry  the current iteration after the 10-minute sleep
  })
  
}, .progress = TRUE)
# ------------------------------------------------------------------------------



















