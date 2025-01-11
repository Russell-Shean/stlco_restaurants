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


all_violations_df <- all_violations_df #|>
                    
                    # add a column for code references
                   # mutate(code_reference = str_extract(violation_text, "(?<=CODE REFERENCE:).*?(?=CORRECTIVE ACTION:)"))

unique_code_text <- str_extract_all(all_violations_df$violation_text, "(?<=CODE REFERENCE:).*?(?=CORRECTIVE ACTION:)") |>
                    unlist() |>
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



unique_law_chunks <- all_violations_df$violation_text |>
                    str_remove("(observed|CORRECTIVE ACTION:|Violation|Observed).*") |> 
                    str_extract_all("\\(\\d-\\d{3}.\\d{2}\\).*?\\(\\d-\\d{3}.\\d{2}\\)") |> 
                    unlist() |> 
                    unique()


for(i in 1:nrow(code_references_dictionary)){
  
  
  all_violations_df <- all_violations_df |> 
                      mutate(violation_text_nolaws = str_remove_all(violation_text_nolaws,
                                                                    stringr::regex(code_references_dictionary[i,"complete_text"],
                                                                               literal=TRUE) ))

}

#remaining_laws2 <- remaining_laws
# now we remove the remaining laws stuff from below
for(law in remaining_laws2){


 all_violations_df <- all_violations_df |> 
   mutate(violation_text_nolaws2 = str_remove_all(str_squish(violation_text_nolaws),
                                        stringr::regex(law,
                                                      literal=TRUE) ))



}






ending_violations <- all_violations_df |> 
     pull(violation_text_nolaws) |>
     str_extract_all("\\(\\d-\\d{3}.\\d{2}\\).*" ) |>
     unlist() |>  
     str_remove_all("(Corrected on site|Pest entry point|CODE REFERENCE:|Mold like|CORRECTIVE ACTION:|Correction:|Tongs stored on|Multiple containers|Walk in|Items observed|Ice machine baffle|Watched employee|Corrected by|No paper towels in|corrected|Observed employee|Back door has|No records of|Cutting boards need|Corrected|HACCP plan needs|Items throughout|Corected by|Bags of|No test strips|Freezer and|Found items|Deceased pests|Debris outside|Floor damage near|Grease still|Bar reach in|Wall dirty below|Multiple items hooked up|Reach in with|Items in the salad|Observed sauces|Gaskets observed|No one with|Utensils stored in food containers|Observed hand washing|Some shellstock tag|Dish wand found|Observed bottle|REPEAT VIOLATION).*") |>
     
    # after removing a bunch of things, I think we have enough complete violation codes 
    # to start excluding any violations that still have inspector comments
     str_subset("test strips|ppm |Drink |touching f|phones| placi|Soa|oap i|s out | out a|ut at|t d |is ou|gloves o|rectin|drinks|luk|cos:|Items not|poor|Found|Dust |turned u|too weak| rice|Rice|bucket |warmer|sani |Wye|hallway|not labeled|in back|strip |build-up|in kitchen|present to|strips|above food|STILL|multiple|food in |still |needs t|damaged on|kitchen l|llow u|in 10|Multiple|Both Violations|prep sink|debris on wall|date marking|Manager still|have servsafe|still does not|3 comp|comp sink|hand soap|hand sink|Needs to be |Pork loin|still dirty|the facility|through out|small hot|hot unit|found on|on walk-in|walk-in|walk-in floor|Gaskets have|substance on them|Gaskets still|still torn|Damaged wall|mold like|Hand sink|Missing ceiling|Comp sink|with gap in|3 Comp|back door|Pest entry possibility|No consumer advisory|not dated|Multiple items|men's restroom|Mop stored|being used|Found multiple items|food debris|underneath ice machine|Floors have food debris|Chef |Needs to be cleaned|Hand sinks|Violation:|will submit|Code Reference:|walk in|Sani buckets|Quat sanitizer|over flowing|shellstock tags|being kept|0ppm quat| quat | Quat |buckets showed|Coconut milk|build up|around ice machine|Debris on|seen in|Needs to reach|to strong|Handwashing sink|inspector|can email|bottles of|Items found|in product|Hand washing sink|Food stored|observed|reach in|COS|Pipe leaking under|No testing device|does not connect to the air gap|need to be|Observed|No one|are still|still at|Reach in|did as well|heavily|Ice machine|multiple pieces of|pieces of equipment|out of date|throughout|Multiple food|food items|Employee drinks|Shelves lined|Some items|date marked|still present|Items in|stored on the floor", negate = TRUE) |>
     str_subset("Bar |ar han|Holes in walls|Hand Washing|No thermometer in facility|need of repair|hand w|d wash|sink di|k did|d not h|t have p|ve pap|zer i|er is t|r is to|o s|s too", negate = TRUE) |>
     str_remove_all("\\.device\\.Hepatitis A records not available on site for all current employees|Scoop in flour was in the flour|Operator is working on current HACCP plan, Has sent over rough draft|Cloths stored in soapy water in sanitizer bucket|Eggplant is being vacuum packed|Please purchase trash cans with lids for restrooms|Linens stored in bin on the floor|Spray nozzle attached to mop sink faucet causing constant source of pressure.|Person in charge is registered for class|In proper cooling|Hose is attached to mop sink causing constant source of pressure") |>
     str_subset("Food found at 122|was attached to mop sink|fruit on bar service well at 57|middle line make table at 60 degrees|Paper towels missing from dispensers|Orange chicken|No items at make table measured|utensils stored in the handwashing sink|Wiping cloths stored on wall|Dishwasher not showing any sanitizer|urger patty, 124F, wrapped, counter top|HVAC vent above the prep arearesidues|temp on items such as cheese sauce|Need to be repaired|display case were missing dates|dwashing signs not available at handsinks|Pipe leaking behind bar|to not use machine until concentration|stored in tepid water|Reach In fridge is|Cooked about noon per owner|Dishwasher at 157|emical sanitizer concentration too high|leaning products stored above soda syrup|No garbages with lids |Stain ceiling tile above|found in the coffee area at 70|Utensils stored in tepid|Kitchen is in need of|Need to wash hands|Salad dressing found at 52|Items at 50 degrees in|Employee not washing hands|Need to be replaced| with a servsafe managers certificate|All employees washed hands|Hot water at 73 degrees|Door on loading dock not|No thermometer provided for|No HACCP plan for|paper towels located in the booth|No handwashing signs|measuring heat of dish machine in BOH|Stocks in hot well|container without the running water on|Hole in the wall|Dates missing on some|Hose attached to mop|Fruit on bar at 50|sent with a current servsafe|Need to be removed|hooked up tot heir|employees did not know how", negate = TRUE) |>
     str_squish() |>
     unique() 




#for(law in ending_violations){
  
  
 # all_violations_df <- all_violations_df |> 
  #  mutate(violation_text_nolaws = str_remove_all(str_squish(violation_text_nolaws),
          #                                        stringr::regex(law,
           #                                                      literal=TRUE) ))
  
  
  
#}
 
remaining_laws <-     all_violations_df |> 
      pull(violation_text_nolaws2) |>
       str_extract_all("\\(\\d-\\d{3}.\\d{2}\\).*" ) |> unlist() |> str_squish( ) |> unique() 


remaining_laws <- remaining_laws |>
                  str_remove("CODE REFERENCE:") |>
                  str_remove("(Employees were observed|Can opener observed|torn gasket|Employee food stored among|Cutting boards damaged|Employee items|Slicer observed dirty|Corrected by|Food not covered|corrected|Pulled from counter|PIC).*") |>
                  str_remove("(Mold like|Cutting boards need|Corected by|Found a bag|Servers had food|Food observed|Employees observed|Employees are smo|Employee food obse|Employee drink|CORRECTIVE ACTION:|Employee food|__________|Employees had items).*") |>
                  
                  str_squish() |>
                  str_remove("(Single use cups stored|Utensils stored in tep|Utensils stored on line|Raw meat being|Chip containers observed|chlorine at 0ppm|Potatoes being|Ice cream machine observed|Microwaves in kitchen|Ice machine was|Food items being|Ice machine|Dry food goods stored|\\.residues\\.|Ice machine on|Ice machine has|Soda coole|Ice machine observed|Soda nozzles|Slicer observed|Utensils stored in room|Corrected|COS:|Linens stored in bin).*") |>
                  
                  str_squish() |>
                   str_remove("(Hep A|Lights are burnt|Walk in freezer floor is|Light bulbs out in|Multiple light bulb out|Lights need to be replaced|Light bulbs missing from|Multiple light bulbs|Lightbulbs still missing|Lights need to be replaced in hood system|Ventilation observed inadequate|Mop stored on|Follow up will be conducted|Debris on wall by|Chef will submit|Tongs stored on lip|Ice scoop stored on top of|Box of utensils was observed|Gaskets on reach|Small cutting boards need|Cutting boards on line|Grate on soda fountain|Line cutting boards still need|Cutting boards on line need|Dust and debris accumulation on|Cutting board on steam|Green cutting board in|Cutting board/block needs|Violation:|No drainboards available|Test kits not available|Facility has HACCP plan on hand|ROP Products found|Meat found at|Establishment needs to|No HACCP plan on file|Establishment is making|Facility needs to|Facility HACCP plan not|End of bread|Food being stored|Observed|Found soup from|Hepatitis A records not available on site |Records form need|Hepatitis A records not kept).*") |>
                  str_squish() |>
                  str_remove("(Door on loading dock|No thermometer provided for|chemical sanitizer concentration too high|Stocks in hot well at 91 degrees|Utensils stored in tepid water|Serving utensils stored in the handwashing|Dates missing on some items|No HACCP plan|No thermometer for|Hose was attached to mop sink|Employees are using gloves in between |Salad dressing found at|Pipe leaking behind bar|No handwashing signs in|Reach In fridge is leaking|Utensils stored in tepid water|Handwashing signs not available|must have dispensed liquid soap and disposable paper towels|Employee not washing hands properly|Fruit on bar at 50 degrees|Hamburger patty, 124F, wrapped|Yogurt cups in lobby display case|Hot water at 73 degrees|Dishwasher not showing any|Utensils stored in tepid water|Orange chicken|Still no one present|Kitchen is in need of|No thermometers provide for|Inspector recorded 165 or above|Instructed owner to instruct employee proper times|Hose attached to mop|Ramekins and bowls used as serving utensils|Food in middle line make|Cooked about noon per|Food found at 122 degrees|dust accumulation on the HVAC vent|Hole in the wall below|No garbages with lids|Paper towels missing from|Dishwasher at 157 degrees|Lighter fluid and glass cleaning products stored|Chlorine not showing on).*") |>
                  str_squish() |>
                   unique()

remaining_laws |>
  str_extract_all("\\(\\d-\\d{3}.\\d{2}\\)" ) |> unlist()  |> table()


remaining_laws |> nchar() |> table()

remaining_laws |> str_subset("6-301.14")


n_chars_ending_laws <- ending_violations |> nchar()

ending_laws_substrates <- c()


for(i in seq_along(ending_violations)){
  
  
 ith_substrate <-  ending_violations[i] |> substr(start = n_chars_ending_laws[i] - 40, stop = n_chars_ending_laws[i]) 
  
  ending_laws_substrates <- c(ending_laws_substrates, ith_substrate)
}

ending_laws_substrates |> unique() |> sort()


ending_violations |> 
  str_subset("Food found at 122|was attached to mop sink|fruit on bar service well at 57|middle line make table at 60 degrees|Paper towels missing from dispensers|Orange chicken|No items at make table measured|utensils stored in the handwashing sink|Wiping cloths stored on wall|Dishwasher not showing any sanitizer|urger patty, 124F, wrapped, counter top|HVAC vent above the prep arearesidues|temp on items such as cheese sauce|Need to be repaired|display case were missing dates|dwashing signs not available at handsinks|Pipe leaking behind bar|to not use machine until concentration|stored in tepid water|Reach In fridge is|Cooked about noon per owner|Dishwasher at 157|emical sanitizer concentration too high|leaning products stored above soda syrup|No garbages with lids |Stain ceiling tile above|found in the coffee area at 70|Utensils stored in tepid|Kitchen is in need of|Need to wash hands|Salad dressing found at 52|Items at 50 degrees in|Employee not washing hands|Need to be replaced| with a servsafe managers certificate|All employees washed hands|Hot water at 73 degrees|Door on loading dock not|No thermometer provided for|No HACCP plan for|paper towels located in the booth|No handwashing signs|measuring heat of dish machine in BOH|Stocks in hot well|container without the running water on|Hole in the wall|Dates missing on some|Hose attached to mop|Fruit on bar at 50|sent with a current servsafe|Need to be removed|hooked up tot heir|employees did not know how")

