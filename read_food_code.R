# extract text from food code
if(!require("pacman")){install.packages("pacman")}

pacman::p_load(pdftools)




food_code_text <- pdf_text("data/stl_food_code.pdf") 


code_chunks <- food_code_text[29:246] |>  paste(collapse = "") |>
                           str_remove_all("\n") |>
                           str_squish() |>
                           str_extract_all("\\d-\\d{3}\\.\\d{2}.*?\\d-\\d{3}\\.\\d{2}") |>
                           unlist() |>
                           str_remove_all("(\\d-\\d{3}\\.\\d{2})") |>
                           str_squish() |>
                           str_to_lower() |>
                          unique()



for(chunk in code_chunks){
  
  
  all_violations_df <- all_violations_df |> 
                       mutate(violation_text_nolaws = str_squish(str_to_lower(violation_text_nolaws))) |>
    mutate(violation_text_nolaws2 = str_remove_all(violation_text_nolaws,
                                                   stringr::regex(chunk,
                                                                  literal=TRUE) ))
  
  
  
}

