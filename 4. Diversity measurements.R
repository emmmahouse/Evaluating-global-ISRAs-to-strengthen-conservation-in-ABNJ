#ISRA: Diversity measurements
#19/02/2026

#Libraries 
library(ggplot2)
library(readxl)
library(vegan)
library(dplyr)
library(tidyr)

#Importing data
isra <- read_excel("C:/Users/emmak/OneDrive - University of Edinburgh/DISS/data/finished datasets/isra_collapsed_final_no_criteria.xlsx")
View(isra)

#Editing dataset
isra <- dplyr::select(isra,
                            `Region`,
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

# Exploring "vegan" package ----
browseVignettes("vegan")

#Practicing diversity with dataset found in vegan package
data(BCI)

#Shannon index (finds diversity indices for all sites)
H <- diversity(BCI)

#Vegan does not have indices for evenness but most common is Pielou's eveness which it can do
#specnumber is a simple vegan function to find the numbers of species 
J <- H/log(specnumber(BCI))

#alpha diversity: within region diversity
alpha <- fisher.alpha(BCI)




# edit the dataset so it is an abundance matrix per region---- 
isra_matrix <- isra %>%
  group_by(Region) %>%
  summarise(across(c(`Landing site surveys`, 
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
                     `Bottom trawling`), sum, na.rm = TRUE ))



# ensure numeric matrix (remove region column)
isra_matrix_numeric <- isra_matrix %>%
  select(-Region)

#shannon diversity per region
H <- diversity(isra_matrix_numeric, index = "shannon")

#Turning shannon into a dataset to plug into a plot 
regions <- c("Asia", 
             "Central and South American Pacific",
             "European Atlantic",
             "Mediterranean and Black Seas",
             "New Zealand and Pacific Islands",
             "Polar Waters",
             "South American Atlantic",
             "South American Inland Waters",
             "Western Indian Ocean")

#avoiding mismatch of regions to the shannon values
shannon <- isra_matrix %>%
  mutate(Shannon = diversity(select(., -Region), index = "shannon"))

#plotting shannon onto a bar chart 
fig_1 <- (ggplot(shannon, aes(x = Region, y = Shannon))+
         geom_bar(position = position_dodge(), stat = "identity", colour = "black", fill = "#00868B") +
         theme_bw()+
         ylab("Shannon Diversity Index (H')")+
         xlab("Region")+
         theme(axis.text.x = element_text(size=20, angle=45,vjust=1, hjust=1),
               panel.grid = element_blank(),
               plot.margin = unit(c(1,1,1,1), units = , "cm"),
               text = element_text(size = 22)))

print(fig_1)

#saving plot 
tiff('shannon1.tiff', units = 'in', width=12, height=7, res=300)
print (fig_1)
dev.off()


#shannon with abnj as a region ----
#ABNJ rows
abnj_rows <- c(121, 704, 774, 779)

#new column for ABNJ or NON-ABNJ in dataset
isra <- isra%>%
  mutate(Region = ifelse(row_number() %in% abnj_rows, "ABNJ", Region))

#sort dataset so abnj is a region 
isra_matrix <- isra %>%
  group_by(Region) %>%
  summarise(across(c(`Landing site surveys`, 
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
                     `Bottom trawling`), sum, na.rm = TRUE ))


#Ensure numeric matrix (remove region column)
isra_matrix_numeric <- isra_matrix %>%
  select(-Region)

#shannon diversity per region = gamma diversity 
H <- diversity(isra_matrix_numeric, index = "shannon")

#Turning shannon into a dataset to plug into a plot 
regions <- c("Asia", 
             "Central and South American Pacific",
             "European Atlantic",
             "Mediterranean and Black Seas",
             "New Zealand and Pacific Islands",
             "Polar Waters",
             "South American Atlantic",
             "South American Inland Waters",
             "Western Indian Ocean",
             "ABNJ")

#avoiding mismatch of regions to the shannon values
shannon <- isra_matrix %>%
  mutate(Shannon = diversity(select(., -Region), index = "shannon"))

#plotting shannon onto a bar chart 
fig_2 <- (ggplot(shannon, aes(x = Region, y = Shannon))+
  geom_bar(position = position_dodge(), stat = "identity", colour = "black", fill = "#00868B") +
  theme_bw()+
  ylab("Shannon Diversity Index (H')")+
  xlab("Region")+
  theme(axis.text.x = element_text(size=20, angle=45,vjust=1, hjust=1),
        panel.grid = element_blank(),
        plot.margin = unit(c(1,1,1,1), units = , "cm"),
        text = element_text(size = 22)))

print (fig_2)

#saving plot 
tiff('shannon2.tiff', units = 'in', width=12, height=7, res=300)
print (fig_2)
dev.off()


# Richness ----

#count the different evidence types used per ISRA but make new dataset first
#editing the dataset
isra_r <- dplyr::select(isra,
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
View(isra_r)


#adding the new richness column onto the end 
isra$richness <- rowSums (isra_r, na.rm = TRUE)

#richness per region as boxplot 
ggplot(isra, aes(x=Region, y=richness))+
  geom_boxplot()+
  theme_bw()+
  ylab("Richness (number of evidence types)")+
  xlab("Region")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#plotting bar chart with error bars
summary_df <- isra %>%
  group_by(Region)%>%
  summarise(
    mean=mean(richness),
    sd = sd(richness),
    n=n(),
    se=sd/sqrt(n))

fig_3 <- (ggplot(summary_df, aes(x=Region, y = mean))+
  geom_col(fill = "#99458A")+
  geom_errorbar(aes(ymin = mean - se, ymax = mean +se), width = 0.2, size = 1) +
  theme_bw() +
  ylab("Mean richness (± SE)")+
         theme(axis.text.x = element_text(size=20, angle=45,vjust=1, hjust=1),
                                            panel.grid = element_blank(),
                                            plot.margin = unit(c(1,1,1,1), units = , "cm"),
                                            text = element_text(size = 22)))

print(fig_3)

#saving the plot 
tiff('richness.tiff', units = 'in', width=12, height=7, res=300)
print (fig_3)
dev.off()


#Calculating this with ABNJ as a bar----
isra <- read_excel("C:/Users/emmak/OneDrive - University of Edinburgh/DISS/data/finished datasets/isra_collapsed_final_no_criteria.xlsx")

#ABNJ rows
abnj_rows <- c(121, 704, 774, 779)

#new column for ABNJ or NON-ABNJ in dataset
isra <- isra%>%
  mutate(Region = ifelse(row_number() %in% abnj_rows, "ABNJ", Region))

#editing the dataset
isra_r <- dplyr::select(isra,
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
View(isra_r)


#adding the new richness column onto the end 
isra$richness <- rowSums (isra_r, na.rm = TRUE)

#richness per region
ggplot(isra, aes(x=Region, y=richness))+
  geom_boxplot()+
  theme_bw()+
  ylab("Richness (number of evidence types)")+
  xlab("Region")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#plotting bar chart with error bars
summary_df <- isra %>%
  group_by(Region)%>%
  summarise(
    mean=mean(richness),
    sd = sd(richness),
    n=n(),
    se=sd/sqrt(n))

fig_4 <- (ggplot(summary_df, aes(x=Region, y = mean))+
  geom_col(fill = "#99458A")+
  geom_errorbar(aes(ymin = mean - se, ymax = mean +se), width = 0.2, size = 1) +
  theme_bw() +
  ylab("Mean richness (± SE)")+
  theme(axis.text.x = element_text(size=20, angle=45,vjust=1, hjust=1),
        panel.grid = element_blank(),
        plot.margin = unit(c(1,1,1,1), units = , "cm"),
        text = element_text(size = 22)))

print(fig_4)

#saving the plot 
tiff('richness2.tiff', units = 'in', width=12, height=7, res=300)
print (fig_4)
dev.off()


#richness per ABNJ vs NON-ABNJ ----
rich <- read_excel("C:/Users/emmak/OneDrive - University of Edinburgh/DISS/data/finished datasets/isra_collapsed_final_no_criteria.xlsx")
View(rich)

#removing the specific columns and keeping only evidence types
richer = dplyr::select(rich, 
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

#new column for richness
richer$richness <- rowSums (richer, na.rm = TRUE)

#ABNJ rows
abnj_rows <- c(121, 704, 774, 779)

#new column for ABNJ or NON-ABNJ in dataset
richer$Zone <- "NON-ABNJ"
richer$Zone[abnj_rows] <- "ABNJ"

#average for the non-abnj and abnj columns
ggplot(richer, aes(x=Zone, y=richness))+
  geom_boxplot()+
  theme_bw()+
  ylab("Richness (number of evidence types)")+
  xlab("Region")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# error bars with se
summary_df <- richer %>%
  group_by(Zone)%>%
  summarise(
    mean=mean(richness),
    sd = sd(richness),
    n=n(),
    se=sd/sqrt(n))

#box plot with error bars
fig_5 <- (ggplot(summary_df, aes(x=Zone, y = mean))+
  geom_col(fill = "lightblue")+
  geom_errorbar(aes(ymin = mean - se, ymax = mean +se), width = 0.2, size = 1) +
  theme_bw() +
  ylab("Mean richness (± SE)")+
  xlab("Region")+
  theme(axis.text.x = element_text(size=20,angle = 30, vjust=1, hjust=1),
        panel.grid = element_blank(),
        plot.margin = unit(c(1,1,1,1), units = , "cm"),
        text = element_text(size = 22)))

print(fig_5)

#saving plot 
tiff('richness3.tiff', units = 'in', width=12, height=7, res=300)
print (fig_5)
dev.off()

#box plot with richness for ABNJ vs NON-ABNJ and all points included 
fig_6 <- (ggplot(richer, aes(x=Zone, y = richness))+
  geom_boxplot(fill = "pink")+
  geom_jitter(width = 0.2)+
  theme_bw() +
  ylab("Mean richness (± SE)")+
  xlab("Region")+
  theme(axis.text.x = element_text(size=20,angle = 30, vjust=1, hjust=1),
        panel.grid = element_blank(),
        plot.margin = unit(c(1,1,1,1), units = , "cm"),
        text = element_text(size = 22)))

print(fig_6)

#saving plot 
tiff('richness4.tiff', units = 'in', width=12, height=7, res=300)
print (fig_6)
dev.off()



# exploring pielou's evenness ----
#specnumber is a vegan function that can find the number of species
J <- H/log(specnumber(isra_matrix_numeric))
plot(J)

pielou <- data.frame(
  Region = regions, 
  Pielou = J)

#plotting the pielou values across all regions 
ggplot(pielou, aes(x = Region, y = J))+
  geom_bar(position = position_dodge(), stat = "identity", colour = "black", fill = "#00812B") +
  theme_bw()+
  ylab("Pielou evenness Index (J')")+
  xlab("Region")+
  theme(axis.text.x = element_text(size=20, angle=45,vjust=1, hjust=1),
        panel.grid = element_blank(),
        plot.margin = unit(c(1,1,1,1), units = , "cm"),
        text = element_text(size = 22))




#richness: how many evidence types were used across each region ----
richness <- specnumber(isra_matrix_numeric)
richness

#abundance heat map----
#transpose data (so it is in long format for heatmap specifically)
isra_long <- pivot_longer(data = isra_matrix,
                          cols = -c(Region),
                          names_to = "Evidence type",
                          values_to = "Abundance")


#heatmap of regions and evidence types
fig_5 <- ggplot (data = isra_long, mapping = aes(x = Region,
  y = `Evidence type`,
  fill = Abundance)) +
  geom_tile()+
  xlab(label = "Region")+
  ylab(label = "Evidence types")+
  theme(axis.text.x = element_text(angle = 44, hjust = 1),
          text = element_text(size = 22))

fig_5

#saving the heatmap
tiff('heatmap.tiff', units = 'in', width=12, height=7, res=300)
print (fig_5)
dev.off()





