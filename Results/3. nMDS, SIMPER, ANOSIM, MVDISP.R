#05/02/2026
#ISRA: NMDS plots

#Practicing using the coding club (ignore)----

#Install packages
install.packages("vegan")
install.packages("ape")
install.packages("dplyr")

library(vegan)
library(ape)
library(dplyr)

# First step is to calculate a distance matrix. See PCOA for more information about the distance measures
# Here we use bray-curtis distance, which is recommended for abundance data
data (varespec)
head(varespec)
dist <- vegdist(varespec,  method = "bray")

# In this part, we define a function NMDS.scree() that automatically 
# performs a NMDS for 1-10 dimensions and plots the nr of dimensions vs the stress
NMDS.scree <- function(x) { #where x is the name of the data frame variable
  plot(rep(1, 10), replicate(10, metaMDS(x, autotransform = F, k = 1)$stress), xlim = c(1, 10),ylim = c(0, 0.30), xlab = "# of Dimensions", ylab = "Stress", main = "NMDS stress plot")
  for (i in 1:10) {
    points(rep(i + 1,10),replicate(10, metaMDS(x, autotransform = F, k = i + 1)$stress)) }}

# Use the function that we just defined to choose the optimal nr of dimensions
NMDS.scree(dist)

# Because the final result depends on the initial random placement of the points 
# we`ll set a seed to make the results reproducible
set.seed(2)

# Here, we perform the final analysis and check the result
NMDS1 <- metaMDS(dist, k = 2, trymax = 100, trace = F)
# Do you know what the trymax = 100 and trace = F means?
# Let's check the results
NMDS1

# If you don`t provide a dissimilarity matrix, metaMDS automatically applies Bray-Curtis. So in our case, the results would have to be the same
NMDS2 <- metaMDS(varespec, k = 2, trymax = 100, trace = F)
NMDS2
stressplot(NMDS1)
plot(NMDS1, type = "t")

#giving metaMDS the original community matrix as input and specifying the distance measure
NMDS3 <- metaMDS(varespec, k = 2, trymax = 100, trace = F, autotransform = FALSE, distance="bray")
plot(NMDS3)
plot(NMDS3, display = "sites", type = "n")
points(NMDS3, display = "sites", col = "red", cex = 1.25)
text(NMDS3, display ="species")

# Alternatively, you can use the functions ordiplot and orditorp
ordiplot(NMDS3, type = "n")
orditorp(NMDS3, display = "species", col = "red", air = 0.01)
orditorp(NMDS3, display = "sites", cex = 1.1, air = 0.01)

# Load the second dataset
data(varechem)

# The function envfit will add the environmental variables as vectors to the ordination plot
ef <- envfit(NMDS3, varechem, permu = 999)
ef

# The two last columns are of interest: the squared correlation coefficient and the associated p-value
# Plot the vectors of the significant correlations and interpret the plot
plot(NMDS3, type = "t", display = "sites")
plot(ef, p.max = 0.05)

# Define a group variable (first 12 samples belong to group 1, last 12 samples to group 2)
group = c(rep("Group1", 12), rep("Group2", 12))

# Create a vector of color values with same length as the vector of group values
colors = c(rep("red", 12), rep("blue", 12))

# Plot convex hulls with colors based on the group identity
ordiplot(NMDS3, type = "n")
for(i in unique(group)) {
  ordihull(NMDS3$point[grep(i, group),], draw="polygon",
           groups = group[group == i],col = colors[grep(i,group)],label=F) } 

orditorp(NMDS3, display = "species", col = "red", air = 0.01)
orditorp(NMDS3, display = "sites", col = c(rep("red",12),
                                           rep("blue", 12)), air = 0.01, cex = 1.25)




#NMDS for all data including ABNJ ----
install.packages("vegan")
library(vegan)
library(ggplot2)
library(readxl)

pc <- read_excel("C:/Users/emmak/OneDrive - University of Edinburgh/DISS/data/finished datasets/isra_collapsed_final_no_criteria.xlsx")
View(pc)

#make community matrix - extract columns with abundance information
com = dplyr::select(pc, 
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


#make a matrix from the data
m_com = as.matrix(com)

#creating the bray curtis matrix
dist_mat <- vegdist(m_com,method="bray", binary=TRUE)

#ensuring region is a grouping variable for future ggplot (vector)
region <- pc[rownames(m_com), "Region"][[1]]


#nmds using the bray-curtis matrix
set.seed(123)
nmds = metaMDS(dist_mat)
nmds
plot(nmds)

#plot nmds to see if it looks different 
#extract NMDS scores (x and y coordinates)
data.scores <- as.data.frame(scores(nmds))
region <- pc$Region
data.scores$Region <- as.factor(region)

#choosing colours for each region
region_colours <- c("Asia"="red", 
                    "Central and South American Pacific"="black", 
                    "European Atlantic"="gold",
                    "Mediterranean and Black Seas"="hotpink", 
                    "New Zealand & Pacific Islands"="blue", 
                    "Polar Waters" ="red", 
                    "South American Atlantic"="turquoise", 
                    "South American Inland Waters"= "navy",
                    "Western Indian Ocean" ="green")

#choosing shapes for each region
region_shapes <- c("Asia"=4, 
                   "Central and South American Pacific"=3, 
                   "European Atlantic"=17,
                   "Mediterranean and Black Seas"=19, 
                   "New Zealand & Pacific Islands"=7, 
                   "Polar Waters" =5, 
                   "South American Atlantic"= 15, 
                   "South American Inland Waters"= 25,
                   "Western Indian Ocean"= 17)

#plotting this onto a nmds plot 
fig_5 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2, color = Region, shape = Region)) +
  geom_point(size = 3) +
  scale_colour_manual(values = region_colours)+
  scale_shape_manual(values = region_shapes)+
  theme_minimal()

print(fig_5)

7#Saving the plot in higher quality
tiff('effect.tiff', units = 'in', width=12, height=7, res=300)
print (fig_5)
dev.off()

# Bestness of fit
stressplot(nmds)
#r2 = 0.98 and 0.91 (really good fit, 1 = the best)

#Kruskal's stress test
nmds$stress
#=0.14 = fair representation, in the middle


#MVDISP----
mvd <-betadisper(dist_mat, group = region)

mvd <- betadisper(m_com, group= region) 

#ANOVA for the MVDISP
anova(mvd)

#Plotting the dispersion of every region on a boxplot
boxplot(mvd)

#creating a datafram for ggplot from the mvd
distances <- mvd$distances
df <- data.frame(
  Distance = distances,
  Group = mvd$group)

#creating the boxplot
fig_7 <- ggplot(df, aes(x = Group, y = Distance)) +
  geom_boxplot(notch = TRUE) +
  geom_jitter(width = 0.1, alpha = 0.6, color = "black") + # add individual points
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Region", y = "Distance to Centroid")

print(fig_7)

#save graph
tiff('effect.tiff', units = 'in', width=12, height=7, res=300)
print (fig_7)
dev.off()


#The ouput for the mvp per region
tapply(mvd$distances, mvd$group, mean)
#changing the order so it is from smallest to biggest 
sort(tapply(mvd$distances, mvd$group, mean))



# Next step: ANOSIM ----

#code for anosim using the bray matrix from earlier
#Setting the seed to increase reproducability 
set.seed(123)
anosim_result <- anosim(dist_mat, group = region)

summary(anosim_result)
plot(anosim_result) 


# Next step: SIMPER ----
#Using the community matrix, not the BC matrix
#species = evidence types (not regions)

simper_result <- simper(m_com, group = region)
summary(simper_result)

#PERMANOVA ----
region <- pc[rownames(m_com), "Region"]
region <- pc$Region
dist_mat <- vegdist(m_com, method = "bray")
set.seed(123)  # for reproducibility
permanova_result <- adonis2(dist_mat ~ region, data = com, permutations = 999)

print(permanova_result)



R.Version()
