download.file("http://bit.ly/cat-breeds-data", "cat_breeds.Rdata", mode = "wb")
load("cat_breeds.Rdata")

# Create a vector of the first, fifth, and 26th cat breeds in the list

c(cats[[1]]$breed, cats[[5]]$breed, cats[[26]]$breed)

# Stretch goal: Create a vector containing the rating for each
# cat breed above's affection with its family


c(cats[[1]]$characteristics[["Affectionate with Family"]],
  cats[[5]]$characteristics[["Affectionate with Family"]],
  cats[[26]]$characteristics[["Affectionate with Family"]])


# Pull out a character vector of the image URLs for each cat breed

library(purrr)
cat_urls <- map_chr(cats, "image")
library(magick)
ims <- map(cat_urls, image_read) %>% image_join()
image_animate(ims, fps = 2)

# Create a data frame of cat information using the
# single-length items in the cats object.

cat_df <- map_df(cats, magrittr::extract, c("breed", "image", "short_desc"))
cat_df

cat_df <- map_df(cats, magrittr::extract, 1:3)
cat_df


# Creating list columns
fix_characteristics <- function(x)
  tibble(characteristic = names(x), rating = as.numeric(x))

cat_df <- cat_df %>%
  mutate(characteristics = map(cats, "characteristics") %>%
           map(fix_characteristics))


cat_stats <- function(x) {
  tibble(
    min_life_span = str_extract(x[[1]], "^\\d{1,}") %>% parse_number,
    max_life_span = str_extract(x[[1]], "to \\d{1,}") %>% parse_number,
    min_weight = str_extract(x[[3]], "^\\d{1,}") %>% parse_number,
    max_weight = str_extract(x[[3]], "to \\d{1,}") %>% parse_number,
    length = x[[2]],
    origin = x[[4]]
  )
}

cat_df <- cat_df %>%
  mutate(stats = map(cats, "stats") %>% map(cat_stats))
select(cat_df, breed, stats) %>%
  unnest(stats) %>%
  ggplot(aes(x = max_weight, y = max_life_span)) +
  geom_jitter(amount = .125) +
  geom_smooth(method = "lm", se = F)

