# Data Processing 

if(!require("pacman")){install.packages("pacman")}

pacman::p_load(dplyr,
               readr,
               stringr)

### 01 - process facilities ----------------------------------------------------


all_facilities <- readr::read_delim("data/all_facilities.csv", delim = "\t")

# fix data quality issues with all facilities
all_facilities <- all_facilities |>
  mutate(facility_id = str_extract(link, "(?<=facid\\=).*"),
         
         address = str_squish(str_remove_all(address, "\r\n")))



### 01 - process inspections  ----------------------------------------------------


# combine multiple permit inspections and violations with single permit facilities
all_inspections_df <-read.csv("data/all_inspections.csv")
mp_inspections_df <-read.csv("data/mp_inspections.csv")

all_inspections_df <- all_inspections_df |> 
  bind_rows(mp_inspections_df)

all_violations_df <-read.csv("data/all_violations.csv")
mp_violations_df  <-read.csv("data/mp_violations.csv")

all_violations_df <- all_violations_df |>
  bind_rows(mp_violations_df) |>
  
  # remove spaces in violation text
  mutate(violation_text = str_squish(violation_text))



# add a column for code references
# mutate(code_reference = str_extract(violation_text, "(?<=CODE REFERENCE:).*?(?=CORRECTIVE ACTION:)"))

unique_code_text <- str_extract_all(all_violations_df$violation_text, "(?<=CODE REFERENCE:).*?(?=CORRECTIVE ACTION:|CODE REFERENCE:)") |>
  unlist() |>
  str_squish() |>
  str_subset("^$", negate = TRUE) |>
  unique() |> 
  sort()



# extract out a list of code referneces so that we can remove them
unique_code_numbers <- str_extract_all(all_violations_df$violation_text, 
                                       "\\(\\d-\\d{3}.\\d{2}\\)(\\([A-Z]\\))*" ) |>
  unlist() |> 
  unique() |> 
  sort()



code_references_dictionary <- data.frame(complete_text = unique_code_text, 
                                         code_number = str_extract(unique_code_text,"\\(\\d-\\d{3}.\\d{2}\\)(\\([A-Z]\\))*" ),
                                         just_text = str_remove(unique_code_text,"\\(\\d-\\d{3}.\\d{2}\\)(\\([A-Z]\\))*" )) |>
  distinct()


all_violations_df <- all_violations_df |>
  mutate(violation_text_nolaws = violation_text )


for(i in 1:nrow(code_references_dictionary)){
  
  
  all_violations_df <- all_violations_df |> 
    mutate(violation_text_nolaws = str_remove_all(violation_text_nolaws,
                                                  stringr::regex(code_references_dictionary[i,"complete_text"],
                                                                 literal=TRUE) ))
  
}


# build a vector of code fragments to remove
code_fragments <- code_references_dictionary$complete_text |> 
                   str_extract_all("(\\(\\d-\\d{3}.\\d{2}\\)).*?(\\(\\d-\\d{3}.\\d{2}\\))") |> 
                    unlist() |>
                    unique()




#remaining_laws2 <- remaining_laws
# now we remove the remaining laws stuff from below
for(law in remaining_laws2){
  
  
  all_violations_df <- all_violations_df |> 
    mutate(violation_text_nolaws = str_remove_all(str_squish(violation_text_nolaws),
                                                   stringr::regex(law,
                                                                  literal=TRUE) ))
  
  
  
}

for(law in remaining_laws3){
  
  
  all_violations_df <- all_violations_df |> 
    mutate(violation_text_nolaws = str_remove_all(str_squish(violation_text_nolaws),
                                                   stringr::regex(law,
                                                                  literal=TRUE) ))
  
  
  
}


# This is an iterative manual mess that I'll use to create a vector of code references

remaining_laws <-     all_violations_df |> 
  pull(violation_text_nolaws) |>
  str_extract_all("\\(\\d-\\d{3}.\\d{2}\\).*" ) |>
    unlist() |>
   str_squish( ) |> 
   unique() 


remaining_laws |>
  str_extract_all("\\(\\d-\\d{3}.\\d{2}\\)" ) |> unlist()  |> table()



remaining_laws |> str_subset("2-102.20")

