if(!require("pacman")){install.packages("pacman")}

pacman::p_load(dplyr,
               RSelenium,
               readr,
               rvest,
               stringr,
               tidyr)


# read in scraped list of facilities
all_facilities <- readr::read_delim("data/all_facilities.csv", delim = "\t")

# fix data quality issues with all facilities
all_facilities <- all_facilities |>
                        mutate(facility_id = str_extract(link, "(?<=facid\\=).*"),
                               
                               address = str_squish(str_remove_all(address, "\r\n")))


# launch selenium web browser
rD <- rsDriver(browser="firefox", 
               port=4553L, 
               chromever = NULL)

remDr <- rD[["client"]]


# define list
inspections_list <- list()
violations_list <- list()

# This list is for facilities, with multiple permits
# If they have more than one permit, we'll record the url and the skip the loop iteration
# and then go back a second time to loop through just those URLS
multiple_permit_urls <- c()

# loop through all facilities
# to get inspections and violations


# 1:10


for(i in 1:nrow(all_facilities)){


# navigate to the facilities page
remDr$navigate(paste0("https://pressagent.envisionconnect.com/",all_facilities$link[i]))


# pull the html
page_html <- remDr$getPageSource()[[1]] |> 
              rvest::read_html()


# extract all the tables from the html
tables <- page_html |> html_table()

# if there's only one column it means there are multiple permits
if(ncol(tables[[2]]) == 1){
  
  
  links <- page_html |> 
           html_nodes("table:nth-child(2)") |>
           html_nodes("a") |> 
           html_attr("href")
  
  
  # record the links for later
  multiple_permit_urls <- c(multiple_permit_urls, links)
  
  # skip the current iteration
  next
}

# the second table is a header with the facility's name and address
name_address <- tables[[2]] |> pull(X2) 

# extract name and address from second table column
name <- name_address[2] |> str_squish()
address1 <- name_address[3] |> str_squish()
address2 <- name_address[4] |> str_squish()


# the third table contains all the inspection data
inspections <- tables[[3]]

# move column names out of first row
colnames(inspections) <- inspections[1,] 
inspections <- inspections[-1,]


# create the inspections data frame

                               # Extract the html tag for all of the inspection's violations
inspections <- inspections |> mutate(inspection_id_tag = str_extract(`Inspection Type`,
                                                                     "(?<=spanId = ).*(?=;)")) |>
  
                              mutate(inspection_id_tag = str_extract(inspection_id_tag,"[A-Z0-9]+")) |>
  
                              # Fill up to add a tag to all rows
                              fill(inspection_id_tag, .direction = "up")  |>
                               
                              # remove blank rows
                              filter(!is.na(Score)) |>
                             
                              mutate(facility_name = name,
                                     address1 = address1,
                                     address2 = address2,
                                     facility_id = all_facilities$facility_id[i]
                                     ) |>
  
                            select(date = Date,
                                   inspection_type = `Inspection Type`,
                                   score = Score,
                                   facility_name,
                                   address1, 
                                   address2, 
                                   facility_id,
                                   inspection_id_tag)
                             
   

# save the inspections into the inspections list
inspections_list[[i]] <- inspections

# extract violations ---------------------------------------------------

# execute javascript to expand the violations table
remDr$executeScript("showAll();", args = list("fugazi"))

# repull the html
page_html <- remDr$getPageSource()[[1]] |> 
  rvest::read_html()


#tables <- page_html |> html_table()

facility_violations <- list()

for (inspection_id in inspections$inspection_id_tag) {

inspection_id_css <- paste0("#", inspection_id)

# critical violations
critical_violations <- page_html |> 
                      html_elements(inspection_id_css) |> 
                       html_nodes( "li[style=' color:#FF0000']") |>
                       html_text() |>
                       str_squish()

# non critical violations
non_critical_violations <- page_html |> 
                            html_elements(inspection_id_css) |>
                            html_nodes( "li[style=' color:#000000']") |>
                            html_text() |>
                            str_squish()


# in cases where there were no violations 
# this won't work, so we'll need to set the violations value equal to NA with a length of 1

if(length(critical_violations) == 0 & length(non_critical_violations) == 0){
  
  violation_type <- NA
  violation_text <- NA
  
} else {
  
  violation_type <- c(critical_violations, non_critical_violations)
  
  violation_text <- page_html |> 
    html_elements(inspection_id_css) |>
    html_nodes( "li[style=' list-style:none;']") |>
    html_text() |>
    str_squish()
  
}








violations_df <- data.frame(facility_name = name,
                            address1 = address1,
                            address2 = address2,
                            facility_id = all_facilities$facility_id[i],
                            inspection_id = inspection_id,
                            violation_type = violation_type,
                            violation_text)




facility_violations[[inspection_id]] <-  violations_df

  
}

facility_violations <- bind_rows(facility_violations)

violations_list[[i]] <- facility_violations

}


# create final data frames from the lists
all_violations_df <- violations_list |> bind_rows()


all_inspections_df <- inspections_list |> bind_rows()
