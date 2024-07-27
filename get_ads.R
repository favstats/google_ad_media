library(httr)
library(tidyverse)

# Define the URL and parameters
get_ads <- function(ar) {


    beginraw <- lubridate::today()
    endraw <- beginraw - 30
    begin <- beginraw %>%
        as.character() %>%
        str_remove_all("-")
    end <- endraw %>%
        as.character() %>%
        str_remove_all("-")


    url <- "https://adstransparency.google.com/anji/_/rpc/SearchService/SearchCreatives?authuser="
    headers <- c(
        "accept" = "*/*",
        "content-type" = "application/x-www-form-urlencoded"
    )

    # Define the body in plain text
    plain_body <- glue::glue('{"2":40,"3":{"1":">>ar<<","6":>>end<<,"7":>>begin<<,"8":[2840],"12":{"1":"","2":true}},"6":{"1":3,"2":true},"7":{"1":2,"2":0,"3":2276}}', .open = ">>", .close = "<<")

    # Encode the body
    encoded_body <- URLencode(paste("f.req=", plain_body, sep = ""))

    # Perform the POST request
    resp <- POST(
        url,
        add_headers(.headers = headers),
        body = encoded_body,
        encode = "form"
    )

    # Print the response content
    res <- content(resp, "parsed")

    res %>%
        .[[1]] %>%
        bind_rows() %>%
        janitor::clean_names()

}


ar <- "AR10462168114010259457"

kamala <- get_ads(ar)
