
if(!require("pacman")){install.packages("pacman")}

pacman::p_load(dplyr,
               readr,
               stringr)



remaining_laws4 <- c(remaining_laws2, remaining_laws3, remaining_laws6, remaining_laws7, remaining_laws8) |> unique()



#nchar(remaining_laws4)

food_code_snippets <- data.frame(n_chars = nchar(remaining_laws4),
                             text= remaining_laws4) |>
                             arrange(desc(n_chars))


## Here's an idea to build a set of code chunks to find an remove

# build a vector of code fragments to remove
code_fragments <- code_references_dictionary$complete_text |> 
  str_extract_all("(\\(\\d-\\d{3}.\\d{2}\\)).*?(\\(\\d-\\d{3}.\\d{2}\\))") |> 
  unlist() |>
  unique()



# This is an iterative manual mess that I'll use to create a vector of code references

remaining_laws <-     all_violations_df |> 
  pull(violation_text_nolaws) |>
  str_extract_all("\\d+.\\d+.\\d{1,3}.*" ) |>
  unlist() |>
  str_squish( ) |> 
  unique() 

807.110-11

remaining_laws |>
  str_extract_all("\\d+.\\d+.\\d{1,3}" ) |> unlist()  |> table()



remaining_laws |> str_subset("807.120.1") |> head()



