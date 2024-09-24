## Load libraries ##

library(tidyverse)
library(here)
library(librarian)

## Joan's code chunk part 1 ##
# setting a theme? #
theme_set(
  theme_bw(base_size = 17)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)

if (!require("librarian")) install.packages("librarian")
librarian::shelf(tidyverse, readxl, viridis, patchwork)


## Michelle set working directory ##
setwd("/Users/michellemohr/Desktop/YOSE-SEKI-SugarPine")
files <- list.files()
outdir <- "/Users/michellemohr/Desktop/YOSE-SEKI-SugarPine"

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#PILA data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## reading in PILA data
folders <- list.dirs(outdir)[-c(1,4)]

pila_list <- data.frame()

for (folder in folders) {
  newfiles = list.files(folder, pattern = "PILAdata")
  for(file in newfiles) {
    xlsfile <- read_excel(paste0(folder,"/",file), na = "NA") %>%
      mutate_if(is.numeric, as.numeric) %>%
      dplyr::select(plotID, plot_type, treeNum,DBH_cm, pitchTubes, exitHoles,
                    activeBranchCanker, inactiveBranchCanker,
                    activeBoleCanker, inactiveBoleCanker,
                    notes, plot_elevation_ft, trans_length, width,
                    percentLive, ribes_50m, ribes_100m, ribes_150m, ribes_200m)
    pila_list <- rbind(pila_list, xlsfile)
  }
}

### NOTE: picking up 1 NA row (will drop later in code - so only 3 plots have NAs as placeholders
### (NO PILA in these plots 7, 8, and 29)).
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#ASSOC data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## reading in ASSOC TREE data
folders <- list.dirs(outdir)[-c(1,4)]

assoc_list <- data.frame()

for (folder in folders) {
  newfiles = list.files(folder, pattern = "YPE_Treedata")
  for(file in newfiles) {
    xlsfile <- read_excel(paste0(folder,"/",file), na = "NA") %>%
      mutate_if(is.numeric, as.numeric) %>%
      dplyr::select(plot, treeNum, DBH_cm, height_m, species, notes, percentLive)
    assoc_list <- rbind(assoc_list, xlsfile)
  }
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#VEG data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## reading in VEG data
folders <- list.dirs(outdir)[-c(1,4)]

veg_list <- data.frame()

for (folder in folders) {
  newfiles = list.files(folder, pattern = "YPE_Understory")
  for(file in newfiles) {
    xlsfile <- read_excel(paste0(folder,"/",file), na = "NA") %>%
      mutate_if(is.numeric, as.numeric) %>%
      dplyr::select(plotID, species1, species2, species3, species4,
                    species5, species6, species7, species8, species9,
                    species10,
                    assoc1, assoc2, assoc3, assoc4, assoc5,
                    assoc6, assoc7, assoc8, assoc9, assoc10)
    veg_list <- rbind(veg_list, xlsfile)
  }
}

## NOTE: Jenny provided code to add in the rest of the veg data columns.

#species11, species12, species13, species14,
#species15, species16, species17, species18, species19,
#species20, species21, species22, species23, species24,
#species25, species26, species27, species28, species29,

#assoc11, assoc12,assoc13, assoc14, assoc15, assoc16, assoc17, assoc18,
#assoc19, assoc20, assoc21, assoc22, assoc23, assoc24,
#assoc25, assoc26, assoc27, assoc28,
#assoc29, assoc30, assoc31, assoc32, assoc33


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#FIRE data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

firedata <- read_csv(here("FIREdata.csv"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#CLIMATE data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

climatedata <- read_csv(here("updatedPRISMdata.csv"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#Changing plot numbers to have 1-59
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# create plot_num column which will change the plots to numerical order#

pila_list <- pila_list %>%
  mutate(plotNum = dense_rank(plotID))

assoc_list <- assoc_list %>%
  mutate(plotNum = dense_rank(plot))

veg_list <- veg_list %>%
  mutate(plotNum = dense_rank(plotID))

climatedata <- climatedata %>%
  mutate(plotNum = dense_rank(PlotID))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#DROPPING CERTAIN ROWS
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Drop rows where plotID column has NA values
pila_list <- pila_list[!is.na(pila_list$plotID), ]

# Drop specific row for plotNum 48 treeNum 11
pila_list <- pila_list %>%
  filter(!(plotNum == 48 & treeNum == 11))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#ALIVE STATUS using pila_list
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#add new column to pila_list dataframe called "alive_status"
pila_list <- pila_list %>%
  mutate(alive_status = ifelse(percentLive == 0, "Dead", "Alive"))

#create new dataframe called 'pila_counts'
pila_counts <- pila_list %>%
  group_by(alive_status) %>%
  summarize(count = n())

# There are 1730 pila trees (+3 NAs)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#ALIVE STATUS using assoc_list
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#add new column to pila_list dataframe called "alive_status"
assoc_list <- assoc_list %>%
  mutate(alive_status = ifelse(percentLive == 0, "Dead", "Alive"))

#create new dataframe called 'assoc_counts'
assoc_counts <- assoc_list %>%
  group_by(alive_status) %>%
  summarize(count = n())

# There are 3310 assoc trees (+1 NA)


######## IMPORTANT STATS #############
# Looks like we have three plots without PILA 7, 8, and 25 (old YPE 29)
# Total trees says 1734 but really is probably 1730 ( 4 NAs are placeholders for plots 7, 8, and 25 without PILA data. 1 row is NAs being picked up)

# Figure out how many trees are alive vs dead for each plot
# Create new dataframe pilaplots_list with selected columns from pila_list
pilaplots_list <- pila_list[, c("plotNum", "treeNum", "alive_status")]

# Create new columns Alive and Dead based on alive_status
pilaplots_list <- pilaplots_list %>%
  mutate(Alive = ifelse(alive_status == "Alive", 1, 0),
         Dead = ifelse(alive_status == "Alive", 0, 1)) %>%
  select(-alive_status)  # Remove the original alive_status column if needed

# Create new column plot_alive with the sum of alive trees per plotNum
pilaplots_list <- pilaplots_list %>%
  group_by(plotNum) %>%
  mutate(plot_alive = sum(Alive))

# Create new column plot_dead with the sum of dead trees per plotNum
pilaplots_list <- pilaplots_list %>%
  group_by(plotNum) %>%
  mutate(plot_dead = sum(Dead))

####
pilaplots <- pilaplots_list %>%
  distinct(plotNum, plot_alive, plot_dead)


# Create a bar plot of alive vs dead individuals per plot
pilaalive_plot <- ggplot(pilaplots_list, aes(x = as.factor(plotNum))) +
  geom_bar(aes(y = plot_alive, fill = "Alive"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = plot_dead, fill = "Dead"), stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Alive" = "blue", "Dead" = "red")) +
  labs(x = "Plot Number", y = "Number of Individuals", fill = "Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Display the plot
print(pilaalive_plot)


### IMPORTANT ########
# 3 plots no PILA
#(plotNum 7, 8, and 25)
# 4 plots with only DEAD PILA
#(plotNum 5: 1 dead tree, 32: 1 dead tree, and 44: 6 dead trees)
### Question: why is this plot showing that I have more than 4 plots with only dead PILA??
#### Answer: redo the plot to show dead and alive side by side instead of stacked

# Assume pilaplots_list is already defined as per your previous steps
# Calculate the number of alive and dead trees per plot
plot_summary <- pilaplots_list %>%
  group_by(plotNum) %>%
  summarise(plot_alive = sum(Alive),
            plot_dead = sum(Dead)) %>%
  pivot_longer(cols = c(plot_alive, plot_dead), names_to = "status", values_to = "count")

# Create a bar plot of alive vs dead individuals per plot
pilaalive_plot <- ggplot(plot_summary, aes(x = as.factor(plotNum), y = count, fill = status)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("plot_alive" = "blue", "plot_dead" = "red"), labels = c("Alive", "Dead")) +
  labs(x = "Plot Number", y = "Number of Individuals", fill = "Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Print the plot
print(pilaalive_plot)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#WPBR PILA infection
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# Create a new dataframe with selected columns from pila_list
pilaWPBR <- pila_list[, c("plotNum", "treeNum", "DBH_cm", "activeBoleCanker", "activeBranchCanker", "inactiveBoleCanker", "inactiveBranchCanker")]


# Drop rows with any NA values in the selected columns
pilaWPBR <- na.omit(pilaWPBR)

# Create the new column WPBRinfected
pilaWPBR$WPBRinfected <- as.integer(rowSums(pilaWPBR[, c("activeBoleCanker", "activeBranchCanker", "inactiveBoleCanker", "inactiveBranchCanker")]) > 0)

# Group by plotNum and calculate the sum of WPBRinfected per plot
pilaWPBR$plotWPBRinfected <- ave(pilaWPBR$WPBRinfected, pilaWPBR$plotNum, FUN = sum)

# Create new column with the number of unique treeNum per plot
pilaWPBR <- pilaWPBR %>%
  group_by(plotNum) %>%
  mutate(trees_plot = n_distinct(treeNum))

# Create new column with the number of trees in the whole study
pilaWPBR <- pilaWPBR %>%
  mutate(total_trees = nrow(.))

#### work with new pilaWPBRclean
# Define the plots to exclude
plots_to_exclude <- c(5, 7, 8, 25, 32, 44)

# Create a new dataframe excluding the specified plots
pilaWPBRclean <- pilaWPBR %>%
  filter(!plotNum %in% plots_to_exclude)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#Incidence (Percent) =
  ### number of individuals in the plot with infections/number of trees in the plot * 100
#Extent =
  ### number of plots with at least one infection/total number of plots surveyed
#Severity =
  ### FORMULA ### cs = (25 - DBH) / 5
#Infection Rate =
  ### number of trees in plot with WPBR/number of PILA in the plot
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

##### Incidence Percent ######

# Create new column called incidencepercent using the number of indiv. infected within a plot (plotWPBRinfected)
# divided by the total number of PILA in the plot (trees_plot)
# and multiply it by 100 to get percent

# Create new column incidencepercent
pilaWPBRclean <- pilaWPBRclean %>%
  mutate(incidencepercent = (plotWPBRinfected / trees_plot) * 100)


##### Extent BEGIN ######
# ### number of plots with at least one infection/total number of plots surveyed

# Create the WPBRone column indicating if a tree has at least one infection
pilaWPBRclean <- pilaWPBRclean %>%
  mutate(WPBRone = ifelse(activeBoleCanker > 0 |
                            activeBranchCanker > 0 |
                            inactiveBoleCanker > 0 |
                            inactiveBranchCanker > 0,
                          1, 0))

# Now summarize to determine if any tree in the plot is infected
plotWPBRone <- pilaWPBRclean %>%
  group_by(plotNum) %>%
  summarize(plotWPBRone = as.integer(any(WPBRone == 1)), .groups = "drop")

# View the resulting summary
print(plotWPBRone)

# Sum the values in the plotWPBRone column
total_WPBRone <- sum(plotWPBRone$plotWPBRone, na.rm = TRUE)

# Print the result
print(total_WPBRone)

# Count the number of unique plots in the plotNum column
num_unique_plots <- n_distinct(plotWPBRone$plotNum)

# Print the result
print(num_unique_plots)


###### IMPORTANT EXTENT NOTES ########
# 27 plots have at least one infection.
# 26 plots do not have any infections.
# There are a total of 53 plots
# (original plot total of 59, but plots 5, 7, 8, 25, 32, and 44 either have no PILA or only dead PILA)

# EXTENT =
#### 27/53 plots with infection = 50.94%

# Step 1: Summarize by plotNum to calculate if each plot has any WPBR infection (1 = infected, 0 = not infected)
plot_extent_summary <- plotWPBRone %>%
  group_by(plotNum) %>%
  summarise(WPBR_infection = ifelse(sum(plotWPBRone) > 0, 1, 0))

# Step 2: Count the number of infected vs. non-infected plots
plot_extent_counts <- plot_extent_summary %>%
  group_by(WPBR_infection) %>%
  summarise(count = n())

# Step 3: Create the bar plot showing WPBR infection extent

#visual 1
ggplot(plot_extent_counts, aes(x = factor(WPBR_infection, labels = c("Not Infected", "Infected")), y = count, fill = factor(WPBR_infection))) +
  geom_bar(stat = "identity") +
  labs(x = "WPBR Infection Status", y = "Number of Plots", title = "Extent of WPBR Infection Across Plots", fill = "WPBR Infection") +
  scale_fill_manual(values = c("Not Infected" = "lightblue", "Infected" = "red")) +
  theme_minimal()

#visual 2
ggplot(plot_extent_counts, aes(x = factor(WPBR_infection, labels = c("Not Infected", "Infected")), y = count, fill = factor(WPBR_infection))) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +  # Add black border, adjust bar width
  labs(x = "WPBR Infection Status", y = "Number of Plots", title = "Extent of WPBR Infection Across Plots", fill = "WPBR Infection") +
  scale_fill_manual(values = c("Not Infected" = "lightblue", "Infected" = "red")) +
  scale_y_continuous(breaks = seq(0, max(plot_extent_counts$count), by = 5)) +  # Add more y-axis ticks
  theme_minimal()

#visual 3
ggplot(plot_extent_counts, aes(x = factor(WPBR_infection, labels = c("Not Infected", "Infected")), y = count)) +
  geom_bar(stat = "identity", aes(fill = factor(WPBR_infection)), color = "black", width = 0.7) +  # Use aes for fill inside geom_bar
  labs(x = "WPBR Infection Status", y = "Number of Plots", title = "Extent of WPBR Infection Across Plots", fill = "WPBR Infection") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "red"), labels = c("Not Infected", "Infected")) +  # Custom colors for bars
  scale_y_continuous(breaks = seq(0, max(plot_extent_counts$count), by = 5)) +  # More y-axis ticks
  theme_minimal()
##### Extent END ######


##### Severity BEGIN ######
# ### severity = cs = (25 - DBH) / 5
## cs =
# 0 -> 0 branch cankers
# 1 -> 1 -3 branch cankers
# 2 -> 4 - 9 branch cankers
# 3 -> 10 - 25 branch cankers
# 4 -> 25 or more branch cankers
# 5 -> bole canker
###
pilaWPBRclean <- pilaWPBRclean %>%
  mutate(
    # Calculate total branch cankers
    total_branch_cankers = activeBranchCanker + inactiveBranchCanker,
#### Canker Severity
    # Assign canker severity based on the Duriscoe 2002 criteria
    cankerseverity = case_when(
      activeBoleCanker == TRUE | inactiveBoleCanker == TRUE ~ 5,  # Presence of bole canker takes priority
      total_branch_cankers == 0 ~ 0,  # No cankers
      total_branch_cankers >= 1 & total_branch_cankers <= 3 ~ 1,  # 1-3 branch cankers
      total_branch_cankers >= 4 & total_branch_cankers <= 9 ~ 2,  # 4-9 branch cankers
      total_branch_cankers >= 10 & total_branch_cankers <= 25 ~ 3,  # 10-25 branch cankers
      total_branch_cankers > 25 ~ 4,  # More than 25 branch cankers
      TRUE ~ NA_real_  # In case no conditions are met, return NA
    )
  )
#### Severity Formula

pilaWPBRclean <- pilaWPBRclean %>%
  mutate(
    severity = if_else(cankerseverity == 0, 0, (cankerseverity + (25 - DBH_cm)) / 5)
  )



##### Infection Rate BEGIN ######
# ### number of trees in plot with WPBR/number of PILA in the plot
pilaWPBRclean <- pilaWPBRclean %>%
  mutate(infectionrate = (plotWPBRinfected / trees_plot))


