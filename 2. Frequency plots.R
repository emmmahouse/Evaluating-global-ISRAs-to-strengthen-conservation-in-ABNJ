#05/02/2026
#ISRA: Frequency plots (RQ1)

#Import dataset ----
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
isra <- read_excel("C:/Users/emmak/OneDrive - University of Edinburgh/DISS/data/finished datasets/isra_collapsed_final_no_criteria.xlsx")
View(isra)

#Changing the names of the columns to make life easier----
isra <- rename(isra, location = `ISRA location`, area = `Area (km2)`, region = `Region`)     # changes the names of the columns (getting rid of capital letters) and overwriting our data frame

#Summing all evidence types used ----
#Editing the dataset so we just have the evidence types
ET <- dplyr::select(isra, 
                                `Landing site surveys`, 
                                `Demersal gillnets and hand net surveys`, 
                                `Artisanal surveys`, 
                                `Interviews with fishers`, 
                                `Catch and bycatch data`, 
                                `Longline surveys`, 
                                `Local Ecological Knowledge (LEK)`, 
                                `Citizen science reports and sightings`,
                                `Citizen science dives`,
                                `Scientific dives`,
                                `Aerial surveys`, 
                                `Underwater visual census (UVC)`,
                                `Tourism observations`,
                                `Mark and recapture`, 
                                `Acoustic tagging`,
                                `Satellite tagging`,
                                `Baited remote underwater videos (BRUV)`,
                                `Contactless ultrasound scanning`,
                                `Underwater photos and videos`,
                                `Stable isotope analysis`,
                                `Environmental DNA (eDNA)`,
                                `Social media`,
                                `Boat surveys`,
                                `Electric fishing`,
                                `Sport fishing`,
                                `Commercial data`,
                                `Stomach analysis`,
                                `Oil and gas platform observations`,
                                `Photographic identification`,
                                `Bottom trawling`)

#Summing up the data in a new data frame and making a new column name 'total'
ET_sum <- ET %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
mutate(across(where(is.character), ~ "Total"))

# Bind it to the original dataframe
ET_with_sum <- bind_rows(ET, ET_sum)

# Pivot_longer to transpose the data
ET_sum <- ET %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "EvidenceType", values_to = "Total")


#Bar chart of the evidence types
ggplot(ET_sum, aes(x = reorder(EvidenceType, -Total), y = Total)) +
  geom_col(fill = "steelblue") +
  labs(title = "Total Evidence Types across global dataset",
       x = "Evidence Type",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# making the plot look nicer
fig_1 <- ggplot(ET_sum, aes(x = reorder(EvidenceType, -Total), y = Total)) +
  geom_col(fill = "steelblue") +
  labs(x = "Evidence Type",
       y = "Frequency") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(face ="bold", angle = 70, hjust = 1, colour = "black"),
        axis.text.y = element_text(face="bold", colour ="black"),
        axis.title = element_text(face = "bold",colour = "black"))

print(fig_1)

#saving the graph in higher quality 
tiff('effect.tiff', units = 'in', width=12, height=7, res=300)
print (fig_1)
dev.off()


#remove ABNJ data from evidence types----
#Which rows have their ISRA present in the ABNJ? 
which(isra$ABNJ == 1)

# ABNJ data : rows 121, 704, 774 and 779

#Create a dataframe without the ABNJ rows in it
isra_no_ABNJ <- isra[-c(121, 704, 774, 779), ]

#Check this new dataset has no ABNJ
(isra_no_ABNJ$ABNJ == "1") 

#for all evidence types (minus the ABNJ data)
ET_no_ABNJ <- dplyr::select(isra_no_ABNJ, 
                    `Landing site surveys`, 
                    `Demersal gillnets and hand net surveys`, 
                    `Artisanal surveys`, 
                    `Interviews with fishers`, 
                    `Catch and bycatch data`, 
                    `Longline surveys`, 
                    `Local Ecological Knowledge (LEK)`, 
                    `Citizen science reports and sightings`,
                    `Citizen science dives`,
                    `Scientific dives`,
                    `Aerial surveys`, 
                    `Underwater visual census (UVC)`,
                    `Tourism observations`,
                    `Mark and recapture`, 
                    `Acoustic tagging`,
                    `Satellite tagging`,
                    `Baited remote underwater videos (BRUV)`,
                    `Contactless ultrasound scanning`,
                    `Underwater photos and videos`,
                    `Stable isotope analysis`,
                    `Environmental DNA (eDNA)`,
                    `Social media`,
                    `Boat surveys`,
                    `Electric fishing`,
                    `Sport fishing`,
                    `Commercial data`,
                    `Stomach analysis`,
                    `Oil and gas platform observations`,
                    `Photographic identification`,
                    `Bottom trawling`)

#Summing up the data in a new dataframe (without ABNJ)
ET_no_ABNJ_sum <- ET_no_ABNJ %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "EvidenceType", values_to = "Total")

#making the bar graph
ggplot(ET_no_ABNJ_sum, aes(x = reorder(EvidenceType, -Total), y = Total)) +
  geom_col(fill = "darkgreen") +
  labs(title = "Evidence type (No ABNJ)",
       x = "Evidence Type",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#nicer plot 
fig_2 <- ggplot(ET_no_ABNJ_sum, aes(x = reorder(EvidenceType, -Total), y = Total)) +
  geom_col(fill = "darkgreen") +
  labs(x = "Evidence Type",
       y = "Frequency") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(face ="bold", angle = 70, hjust = 1, colour = "black"),
        axis.text.y = element_text(face="bold", colour ="black"),
        axis.title = element_text(face = "bold",colour = "black"))

print (fig_2)

#save graph
tiff('effect.tiff', units = 'in', width=12, height=7, res=300)
print (fig_2)
dev.off()


#MAKING ABNJ EVIDENCE TYPE DATA----

#Assigning specific ABNJ ISRAs rows to a new dataset for only ABNJ 
ABNJ_data <- isra[c(121, 704,774, 779), ]

# Making a bar chart of the summed up ET
# Sum up the ET
ABNJ_ET <-dplyr::select(ABNJ_data, 
                        `Landing site surveys`, 
                        `Demersal gillnets and hand net surveys`, 
                        `Artisanal surveys`, 
                        `Interviews with fishers`, 
                        `Catch and bycatch data`, 
                        `Longline surveys`, 
                        `Local Ecological Knowledge (LEK)`, 
                        `Citizen science reports and sightings`,
                        `Citizen science dives`,
                        `Scientific dives`,
                        `Aerial surveys`, 
                        `Underwater visual census (UVC)`,
                        `Tourism observations`,
                        `Mark and recapture`, 
                        `Acoustic tagging`,
                        `Satellite tagging`,
                        `Baited remote underwater videos (BRUV)`,
                        `Contactless ultrasound scanning`,
                        `Underwater photos and videos`,
                        `Stable isotope analysis`,
                        `Environmental DNA (eDNA)`,
                        `Social media`,
                        `Boat surveys`,
                        `Electric fishing`,
                        `Sport fishing`,
                        `Commercial data`,
                        `Stomach analysis`,
                        `Oil and gas platform observations`,
                        `Photographic identification`,
                        `Bottom trawling`)

#Summing up the data in a new dataframe (without ABNJ)
ABNJ_ET_sum <- ABNJ_ET %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "EvidenceType", values_to = "Total")

#Make a graph of these sums
ggplot(ABNJ_ET_sum, aes(x = reorder(EvidenceType, -Total), y = Total)) +
  geom_col(fill = "gold") +
  labs(title = "Evidence types used to identify ISRAs in ABNJ only",
       x = "Evidence Type",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Nicer plot and removal of the other evidence types that are not present 
ABNJ_ET_sum_clean <- ABNJ_ET_sum %>%
  filter(EvidenceType %in% c("Satellite tagging", "Catch and bycatch data", "Longline surveys", "Commerical data", "Interviews with fishers", "Social media", "Stomach analysis"))

fig_3 <- ggplot(ABNJ_ET_sum_clean, aes(x = reorder(EvidenceType, -Total), y = Total)) +
  geom_col(fill = "gold") +
  labs(x = "Evidence Type",
       y = "Frequency") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(face ="bold", angle = 50, hjust = 1, colour = "black"),
        axis.text.y = element_text(face="bold", colour ="black"),
        axis.title = element_text(face = "bold",colour = "black"))

print(fig_3)

#save graph
tiff('effect.tiff', units = 'in', width=12, height=7, res=300)
print (fig_3)
dev.off()

