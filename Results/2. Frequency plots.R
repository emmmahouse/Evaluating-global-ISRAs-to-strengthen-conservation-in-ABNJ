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
  filter(EvidenceType %in% c("Satellite tagging", "Catch and bycatch data", "Longline surveys", "Commercial data", "Interviews with fishers", "Social media", "Stomach analysis"))

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


# plotting graphs per themes
# figure 4
# assigning the themes to the different evidence types
themes <- c(`Landing site surveys` = "Fisheries-dependent data" , 
            `Demersal gillnets and hand net surveys` = "Fisheries-dependent data", 
            `Artisanal surveys` = "Fisheries-dependent data", 
            `Interviews with fishers`= "Fisheries-dependent data", 
            `Catch and bycatch data` = "Fisheries-dependent data", 
            `Longline surveys` = "Fisheries-dependent data", 
            `Local Ecological Knowledge (LEK)` = "Local knowledge, citizen science and public observations", 
            `Citizen science reports and sightings` = "Local knowledge, citizen science and public observations" ,
            `Citizen science dives`= "Local knowledge, citizen science and public observations" ,
            `Scientific dives` = "Fisheries-independent data",
            `Aerial surveys`= "Fisheries-independent data", 
            `Underwater visual census (UVC)`= "Fisheries-independent data" ,
            `Tourism observations`= "Local knowledge, citizen science and public observations",
            `Mark and recapture` = "Tagging techniques", 
            `Acoustic tagging` = "Tagging techniques",
            `Satellite tagging` = "Tagging techniques",
            `Baited remote underwater videos (BRUV)`= "Fisheries-independent data",
            `Contactless ultrasound scanning`= "Fisheries-independent data",
            `Underwater photos and videos`= "Fisheries-independent data",
            `Stable isotope analysis`= "Fisheries-independent data",
            `Environmental DNA (eDNA)`= "Fisheries-independent data",
            `Social media`= "Local knowledge, citizen science and public observations",
            `Boat surveys`= "Fisheries-independent data",
            `Electric fishing` = "Fisheries-dependent data",
            `Sport fishing`= "Fisheries-dependent data" ,
            `Commercial data`= "Fisheries-dependent data",
            `Stomach analysis`= "Fisheries-independent data",
            `Oil and gas platform observations`= "Local knowledge, citizen science and public observations",
            `Photographic identification`= "Fisheries-independent data",
            `Bottom trawling` = "Fisheries-dependent data")

ABNJ_ET_sum_clean <- ABNJ_ET_sum %>%
  filter(EvidenceType %in% c("Satellite tagging", "Catch and bycatch data", "Longline surveys", "Commercial data", "Interviews with fishers", "Social media", "Stomach analysis"))

#adding themes into the dataset
ABNJ_ET_sum_clean$Theme <- themes[ABNJ_ET_sum_clean$EvidenceType]

fig_4 <- ggplot(ABNJ_ET_sum_clean, aes(x = reorder(EvidenceType, -Total), y = Total, fill = Theme)) +
  geom_col() +
  labs(x = "Evidence Type",
       y = "Frequency") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(face ="bold", angle = 60, hjust = 1, colour = "black"),
        axis.text.y = element_text(face="bold", colour ="black"),
        axis.title = element_text(face = "bold",colour = "black")) +
  scale_fill_manual(values = c(
    "Fisheries-dependent data" = "#E69F00",
    "Fisheries-independent data" = "#56B4E9",
    "Tagging techniques" = "#009E73",
    "Local knowledge, citizen science and public observations" = "#CC79A7"))

print(fig_4)

#save graph
tiff('effect.tiff', units = 'in', width=12, height=7, res=300)
print (fig_4)
dev.off()


## frequency bar chart of all evidence types across all regions






#Adding a themes column to the data for all regions
ET_sum$Theme <- themes[ET_sum$EvidenceType]

#plotting the data again with the themes coloured
fig_7 <- ggplot(ET_sum, aes(x = reorder(EvidenceType, -Total), y = Total, fill = Theme)) +
  geom_col()+
  labs(x = "Evidence Type",
       y = "Frequency") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(face ="bold", angle = 70, hjust = 1, colour = "black"),
        axis.text.y = element_text(face="bold", colour ="black"),
        axis.title = element_text(face = "bold",colour = "black"))+
          scale_fill_manual(values = c(
            "Fisheries-dependent data" = "#E69F00",
            "Fisheries-independent data" = "#56B4E9",
            "Tagging techniques" = "#009E73",
            "Local knowledge, citizen science and public observations" = "#CC79A7"))

print(fig_7)


#saving the graph in higher quality 
tiff('effect.tiff', units = 'in', width=12, height=7, res=300)
print (fig_7)
dev.off()


