library(tidyverse)
library(stringr)

convert_chicken <- function(real_name) {
  # Read names
  nms <- readr::read_lines("chickennames.csv") 
  
  # Chicken name dataset
  chicken_df <- tibble::tibble(chicken_names = str_split(nms, ", ")[[1]])
  
  # New column for first letter of chicken name
  chicken_df$first_letter <- purrr::map_chr(chicken_df$chicken_names, ~str_split(., "")[[1]][1])
  
  # Randomly select letter from name
  letter_key <- sample(str_split(real_name, "")[[1]], 1)
  
  # Subset chicken names by random letter
  chicken_df2 <- chicken_df %>% 
    dplyr::filter(first_letter == toupper(letter_key))
  
  # Randomly select chicken name
  chicken_df2$chicken_names[sample(1:nrow(chicken_df2), 1)] 
}

convert_chicken("Ryan")