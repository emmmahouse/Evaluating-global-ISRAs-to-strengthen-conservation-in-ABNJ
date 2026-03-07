# 04/02/2026
# ISRA: data checks

#Import dataset 
library(readxl)
isra <- read_excel("C:/Users/emmak/OneDrive - University of Edinburgh/DISS/data/finished datasets/isra_collapsed_final_no_criteria.xlsx")
View(isra)

#Coding Club tutorial one: checks
head(isra)                # Displays the first few rows
tail(isra)                # Displays the last rows
str(isra)                 # Tells you whether the variables are continuous, integers, categorical or characters
head(isra$Region)         # Displays the first few rows of this column only

#Checks for blanks/NA in evidence types (should all return FALSE)
anyNA(isra$`Landing site surveys`)
anyNA(isra$`Demersal gillnets and hand net surveys`)
anyNA(isra$`Artisanal surveys`)
anyNA(isra$`Interviews with fishers`)
anyNA(isra$`Catch and bycatch data`)
anyNA(isra$`Longline surveys`)
anyNA(isra$`Local Ecological Knowledge (LEK)`)
anyNA(isra$`Citizen science reports and sightings`)
anyNA(isra$`Citizen science dives`)
anyNA(isra$`Scientific dives`)
anyNA(isra$`Aerial surveys`)
anyNA(isra$`Underwater visual census (UVC)`)
anyNA(isra$`Tourism observations`)
anyNA(isra$`Mark and recapture`)
anyNA(isra$`Acoustic tagging`)
anyNA(isra$`Satellite tagging`)
anyNA(isra$`Baited remote underwater videos (BRUV)`)
anyNA(isra$`Contactless ultrasound scanning`)
anyNA(isra$`Underwater photos and videos`)
anyNA(isra$`Stable isotope analysis`)
anyNA(isra$`Environmental DNA (eDNA)`)
anyNA(isra$`Social media`)
anyNA(isra$`Boat surveys`)
anyNA(isra$`Commercial data`)
anyNA(isra$`Stomach analysis`)
anyNA(isra$`Oil and gas platform observations`)
anyNA(isra$`Photographic identification`)
anyNA(isra$`Bottom trawling`)
anyNA(isra$`ABNJ`)

#Check there are 817 ISRAs
sum(!is.na(isra$`ISRA location`))

#Check how many areas there are and how many ISRA are in each region
table(isra$Region)

## DATA HAS BEEN CHECKED ##



