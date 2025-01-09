# Procesing 

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

all_violations_df <-read.csv("data/all_violations.csv")
mp_violations_df  <-read.csv("data/mp_violations.csv")