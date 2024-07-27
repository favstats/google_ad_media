unlink("node_modules", recursive = T, force = T)
unlink("out", recursive = T, force = T)

# library(tidyverse)
options(timeout=300)

source("get_ads.R")

# source("utils.R")

options(python_init = TRUE)

# cntry_str <- "NL"
# time_preset <- commandArgs(trailingOnly = TRUE)
# time_preset <- "last_90_days"

# install.packages("pacman")
pacman::p_load(
    reticulate,
    vroom,
    progress,
    janitor,
    fs,
    tidyr,
    # appendornot,
    countrycode,
    dplyr,
    stringr,
    lubridate,
    purrr,
    glue,
    rvest,
    cli,
    digest,
    readr,
    piggyback
)


if(!("playwrightr" %in% tibble::as_tibble(installed.packages())$Package)){
    remotes::install_github("benjaminguinaudeau/playwrightr")
}

library(playwrightr)

# releases <- piggyback::pb_releases()

# debugonce(pb_releases)
# Call the function to perform the operation
# full_repos <- read_rds("https://github.com/favstats/meta_ad_reports/releases/download/ReleaseInfo/full_repos.rds")


log_file <- file.path(paste0(digest::digest("YOOo1OOo"), ".txt"), "debug_log.txt")


# options(googledrive_quiet = TRUE)
#
# drive_auth(path = Sys.getenv("GOOGLE_APPLICATION_KEY"))

# conda_install(packages = "fcntl", pip = T)
if(Sys.info()[["sysname"]]=="Windows"){

    pw_init(use_xvfb = F)
} else{

    conda_install(packages = "xvfbwrapper", pip = T)

    print("installed xvfbwrapper")
    conda_install(packages = "playwright", pip = T)
    print("installed playwright")

    pw_init(use_xvfb = T)
    system("playwright install")
}


browser_df <- browser_launch(
    headless = F,
    browser = "chromium",
    user_agent = NULL,
    user_data_dir = "out",
    timeout = 6000*5
)

page_df <- browser_df


tmp_yt_string <-
    paste0(digest::digest("YOOYOOo2222"), ".txt")

tmp_gglvid_string <-
    paste0(digest::digest("YOOYOOo"), ".txt")#  tempfile(fileext = ".txt")

# Format: Video
on <- function(page_df, event, lambda_string) {
    playwrightr:::py_run(glue('{page_df$page_id}.on("{event}", {lambda_string})'))
    return(page_df)
}

off <- function(page_df, event, lambda_string) {
    playwrightr:::py_run(glue(
        '{page_df$page_id}.remove_listener("{event}", {lambda_string})'
    ))
    return(page_df)
}

# Set up the event listener for requests
page_df %>% on(
    "request",
    glue::glue(
        'lambda request: (open(r"{tmp_gglvid_string}", "w").write(str(request.url)) if (request.method == "GET" and "videoplayback" in request.url) else None)'
    )
)

Sys.sleep(2)

page_df %>% on(
    "request",
    glue::glue(
        'lambda request: (open(r"{tmp_yt_string}", "w").write(str(request.url)) if (request.method == "GET" and "ytimg" in request.url) else None)'
    )
)


# get_ggl_media("AR10462168114010259457", "CR15395342536306327553")


ggl_content <- function(ar = "AR12920542145097498625", cr = "CR18234995520227508225", iso = "US") {

    glue::glue("https://adstransparency.google.com/advertiser/{ar}/creative/{cr}?topic=political&region={iso}&hl=en")

}
get_ggl_media <- function(ar = "AR12920542145097498625", cr = "CR18234995520227508225", iso = "US") {
    print("Starting get_ggl_media function")
    print(glue::glue("ar: {ar}, cr: {cr}, iso: {iso}"))

    try({
        print("Navigating to content URL")
        page_df <- page_df %>% playwrightr::goto(ggl_content(ar, cr, iso))
        Sys.sleep(5)

        print("Fetching page content")
        thecontent <- page_df %>% playwrightr::get_content()

        print("Extracting format type")
        which_format <- thecontent %>% html_elements("div.property:nth-child(4)") %>% html_text()
        print(glue::glue("Format type: {which_format}"))

        num <- 1

        if (which_format == "Format: Text" | which_format == "Format: Image") {
            print("Processing Text or Image format")
            img_txts <- thecontent %>%
                html_nodes(".html-container") %>%
                html_children() %>%
                html_attr("src")

            if (which_format == "Format: Image") {
                print("Image format detected")
                img_names <- read_html(img_txts) %>%
                    html_elements(".img_ad") %>%
                    html_attr("src")

                imglinks <- str_remove(img_txts, "index.html") %>%
                    paste0(img_names)



                imglinks %>%
                    walk(~{
                        if (length(img_txts) == 1) {
                            filending <- sub(".*\\.|.*", "", .x, perl = TRUE)
                            print(glue::glue("Downloading single image: {filending}"))
                            download.file(.x, destfile = glue::glue("media/{ar}_{cr}.{filending}"), mode = "wb")
                        } else {
                            filending <- sub(".*\\.|.*", "", .x, perl = TRUE)
                            print(glue::glue("Downloading image {num}: {filending}"))
                            download.file(.x, destfile = glue::glue("media/{ar}_{cr}_{num}.{filending}"), mode = "wb")
                            num <<- num + 1
                        }
                    })

                theres <- tibble(ar, cr, imglink = imglinks, which_format)
                print(theres)
                return(theres)

            } else {
                print("Text format detected")
                theres <- tibble(ar, cr, txlink = img_txts, which_format)
                print(theres)
                return(theres)                # img_txts %>%
                #     walk(~{
                #         if (length(img_txts) == 1) {
                #             print("Taking screenshot of single text ad")
                #             webshot2::webshot(.x, selector = "#single-ad-canvas", file = glue::glue("media/{ar}_{cr}.png"))
                #         } else {
                #             print(glue::glue("Taking screenshot of text ad {num}"))
                #             webshot2::webshot(.x, selector = "#single-ad-canvas", file = glue::glue("media/{ar}_{cr}_{num}.png"))
                #             num <<- num + 1
                #         }
                #     })
            }
        } else {
            print("Processing video format")
            page_df <- page_df %>% playwrightr::goto(ggl_content(ar, cr, iso))
            try({
                print("Reading video URLs")
                read_lines(tmp_gglvid_string) %>%
                    walk(~{
                        if (length(tmp_gglvid_string) == 1) {
                            filending <- sub(".*\\.|.*", "", .x, perl = TRUE)
                            print(glue::glue("Downloading single video: {filending}"))
                            download.file(.x, destfile = glue::glue("media/{ar}_{cr}.mp4"), mode = "wb")
                        } else {
                            filending <- sub(".*\\.|.*", "", .x, perl = TRUE)
                            print(glue::glue("Downloading video {num}: {filending}"))
                            download.file(.x, destfile = glue::glue("media/{ar}_{cr}_{num}.mp4"), mode = "wb")
                            num <<- num + 1
                        }
                    })

                print("Returning result")
                theres <- tibble(ar, cr, vidlink = tmp_gglvid_string, which_format)
                print(theres)
                return(theres)
            })

            try({
                print("Reading YouTube video URLs")
                fin <- read_lines(tmp_yt_string)
            })

            if (!exists("fin")) {
                fin <- "none"
            }

            print("Cleaning up temporary files")
            unlink(tmp_gglvid_string, force = TRUE)
            unlink(tmp_yt_string, force = TRUE)

            print("Returning result")
            theres <- tibble(ar, cr, ytlink = fin, which_format)
            print(theres)
            return(theres)
        }
    })

    print("Completed get_ggl_media function")
}

already_there <- dir("media") %>%
    map_dfr(~{
        thestuff <- str_split(.x, "_") %>% unlist() %>% str_remove_all("\\....")
        tibble(x1 = thestuff[1], x2 = thestuff[2])
        })

doit <- kamala %>%
    anti_join(already_there) %>%
    sample_n(n()) %>%
    slice(1:5) %>%
    split(1:nrow(.)) %>%
    map_dfr(~{
        get_ggl_media(.x$x1, .x$x2)
    }, .progress= T)

playwrightr:::py_run(glue::glue("{page_df$page_id}.close()"))

# saveRDS(doit, file = "doit.rds")
appendornot::save_csv(doit, path = "doit.csv")

doit <- read_csv("doit.csv")

doit %>%
    drop_na(txlink) %>%
    group_by(cr) %>%
    mutate(cr_id = 1:n()) %>%
    ungroup() %>%
    mutate(cr_lab = glue::glue("{ar}_{cr}_{cr_id}.png")) %>%
    split(1:nrow(.))  %>%
        walk(~{
                print("Taking screenshot of single text ad")
                webshot2::webshot(.x$txlink, selector = "#single-ad-canvas", file = glue::glue("media/{.x$cr_lab}"))

        })

#
# get_ggl_media()
# get_ggl_media("AR02110921622201303041",
#               "CR18436650247728922625")
#
# get_ggl_media("AR03090935405692846081",
#               "CR18216615980778389505")
#
# # https://adstransparency.google.com/advertiser/AR03090935405692846081/creative/CR18216615980778389505?authuser=0&region=US&topic=political
#
#
#
#
#
# the_tag <- paste0(cntry_str, "-", tframe)
#
# # cntry_name
#
# if(!(the_tag %in% release_names)){
#     pb_release_create_fr(repo = "favstats/meta_ad_reports",
#                          tag = the_tag,
#                          body = paste0("This release includes ", cntry_name ," '", tframe ,"' Meta ad spending reports."),
#                          releases = full_repos)    # Sys.sleep(5)
# }
#
# file.copy(report_path, paste0(the_date, ".zip"), overwrite = T)
#
# try({
#     # print(paste0(the_date, ".rds"))
#     # print(the_tag)
#     # debugonce(pb_upload_file_fr)
#     # debugonce(pb_upload_file_fr)
#     pb_upload_file_fr(paste0(the_date, ".rds"), repo = "favstats/meta_ad_reports", tag = the_tag, releases = full_repos)
#     pb_upload_file_fr(paste0(the_date, ".zip"), repo = "favstats/meta_ad_reports", tag = the_tag, releases = full_repos)
#
#     lat_dat <- latest_dat %>%
#         filter(country == cntry_str)
#
#     if(nrow(lat_dat) == 0){
#         check_it <- T
#     } else {
#         check_it <- (lubridate::ymd(the_date) >= lat_dat$day)
#     }
#
#     if(length(check_it)!=0){
#         if(check_it){
#             file.rename(paste0(the_date, ".rds"), "latest.rds")
#             # debugonce(pb_upload_file_fr)
#             pb_upload_file_fr("latest.rds", repo = "favstats/meta_ad_reports", tag = the_tag, releases = full_repos)
#
#             file.remove("latest.rds")
#         }
#     }
#
#
#
#
#     file.remove(paste0(the_date, ".rds"))
#     file.remove(paste0(the_date, ".zip"))
# })
#
#
#
# gc()
#
# }
# # # .[1:7] %>%
# # walk_progress( ~ {
# #
# #
# # })
#
# # unzip("report/TN/2023-11-28.zip", exdir = "extracted", overwrite = T)
#
# # unzip(dir(paste0("report/",cntry_str), full.names = T, recursive = T), exdir = "extracted")
#
# print("NL UNZIPPED")
#
#
#
# unlink("node_modules", recursive = T, force = T)
# unlink("out", recursive = T, force = T)
#
# print("################6")
#
# dir() %>%
#     keep( ~ str_detect(.x, ".txt")) %>%
#     discard( ~ str_detect(.x, "n_advertisers.txt")) %>%
#     walk(file.remove)
#
# # all_reports_old <- readRDS("logs/all_reports.rds")
#
# print("################9")
#
# # all_reports <- dir("report", full.names = T, recursive = T)
#
# print("################10")
#
# # all_reports <- all_reports_old %>%
# #   c(all_reports) %>%
# #   unique()
# # print("################11")
# #
# # saveRDS(all_reports, file = paste0("logs/all_reports_", time_preset, ".rds"))
#
# print("################12")
#
# unlink("report", recursive = T, force = T)
# unlink("extracted", recursive = T, force = T)
