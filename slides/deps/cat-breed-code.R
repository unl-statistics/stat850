# Scrape cat breeds

library(rvest)
library(httr)
library(xml2)

base_url <- "https://cattime.com/cat-breeds"
base_html <- read_html(base_url)

breeds <- xml_nodes(base_html, ".group-letter .list-item a") %>%
 xml_attr("href")

cat_info <- function(url) {
  html <- read_html(url)

  list(
    breed = html %>% xml_nodes("h1") %>% xml_text(),
    image = html %>% xml_nodes(".breeds-single-intro > img") %>% xml_attr("src"),
    short_desc = html %>% xml_nodes(".breeds-single-intro > p:first-of-type") %>% xml_text(),
    characteristics = html %>% xml_nodes(".child-characteristic .characteristic-star-block") %>% xml_text() %>%
      as.list() %>%
      set_names(html %>% xml_nodes(".child-characteristic .characteristic-title") %>% xml_text()),
    stats = html %>% xml_nodes(".vital-stat-box") %>% xml_text() %>% stringr::str_remove(".*:") %>% as.list() %>%
      set_names(
        html %>% xml_nodes(".vital-stat-title") %>% xml_text() %>% stringr::str_remove(":")
      )
  )
}

cats <- purrr::map(breeds, cat_info)
save(cats, file = "deps/cat_breeds.Rdata")
