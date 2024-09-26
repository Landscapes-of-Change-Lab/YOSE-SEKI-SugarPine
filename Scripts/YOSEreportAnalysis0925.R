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
###########       WPBR PILA infection     #################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# Create a new dataframe with selected columns from pila_list
pilaWPBR <- pila_list[, c("plotNum", "treeNum", "plot_type", "DBH_cm", "activeBoleCanker", "activeBranchCanker", "inactiveBoleCanker", "inactiveBranchCanker")]


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
############# INCIDENCE ####### EXTENT ######### SEVERITY ############ INFECTION RATE ######################
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

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############# Extent BEGIN ###################
# ### number of plots with at least one infection/total number of plots surveyed
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
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
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############## Extent END ####################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############## Severity BEGIN ################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ### severity = cs = (25 - DBH) / 5
## cs =
# 0 -> 0 branch cankers
# 1 -> 1 -3 branch cankers
# 2 -> 4 - 9 branch cankers
# 3 -> 10 - 25 branch cankers
# 4 -> 25 or more branch cankers
# 5 -> bole canker

# NOTE: ask Joan about formula -->
# * if tree diameter is greater than 25 inches, then DBH = 25 *
# * unless cs = 0, then S = 0 *
### note: I am going to have to change the DBH column from cm -> inches

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
### note: I am going to have to change the DBH column from cm -> inches

pilaWPBRclean <- pilaWPBRclean %>%
  mutate(
    severity = if_else(cankerseverity == 0, 0, (cankerseverity + (25 - DBH_cm)) / 5)
  )
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################ Severity END ################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
########## Infection Rate BEGIN ##############
# ### number of trees in plot with WPBR/number of PILA in the plot
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

pilaWPBRclean <- pilaWPBRclean %>%
  mutate(infectionrate = (plotWPBRinfected / trees_plot))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############ Infection Rate END #############
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

###### Let's separate random and high severity plots ##########
###### to see how infection rates change ########

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#$$$$$$$$$$$$$$$$$$$$$$#
      # EXTENT #
#$$$$$$$$$$$$$$$$$$$$$$#
# Step 1: Summarize the number of plots with WPBR infections by plot type
plottype_summary <- pilaWPBRclean %>%
  group_by(plotNum, plot_type) %>%
  summarize(wpbr_present = sum(WPBRinfected > 0)) %>%  # 1 if WPBR is present, 0 otherwise
  ungroup() %>%
  group_by(plot_type) %>%
  summarize(
    total_plots = n_distinct(plotNum),      # Count total plots
    plots_with_wpbr = sum(wpbr_present > 0) # Count plots with WPBR presence
  )

# View the summary table
print(plottype_summary)

#### IMPORTANT STATS:

# 52 total plots??? why is one missing

# random -> 36 plots total (22 of the random plots have infections)
## random 22/36 = 61.1%

# high severity -> 16 plots total (5 of the high severity plots have infections)
## high severity 5/16 = 31.25%

### both random and high severity = 27/52 = 51.9%

#$$$$$$$$$$$$$$$$$$$$$$#
   # INFECTION RATE #
#$$$$$$$$$$$$$$$$$$$$$$#
# Step 1: Group by plotNum and plot_type, calculate infection rate for each plot
plottypeWPBRtreesummary <- pilaWPBRclean %>%
  group_by(plotNum, plot_type) %>%
  summarize(
    trees_plot = n(),                             # Total number of trees in the plot
    plotWPBRinfected = sum(WPBRinfected > 0)      # Number of infected trees in the plot
  ) %>%
  mutate(
    infectionrate = plotWPBRinfected / trees_plot  # Calculate infection rate
  ) %>%
  ungroup()

# View the result
print(plottypeWPBRtreesummary)

# OR #

# Step 2: Calculate average infection rate per plot type
infectionrate_summary <- pilaWPBRclean %>%
  group_by(plot_type) %>%
  summarize(
    total_plots = n_distinct(plotNum),            # Total number of plots in each plot type
    avg_infectionrate = mean(infectionrate, na.rm = TRUE)  # Average infection rate per plot type
  )

# View the infection rate summary
print(infectionrate_summary)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
###########         PLOT CHARACTERISTICS        ############
# objective: create a table that has the following
### DBH
### height
### presence/absence of Ribes
### number of standing dead trees
### number of trees with signs of DEPO/DEVA/weevils
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#RIBES pila_list
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#### work with new pila_list_clean
# Define the plots to exclude
plots_to_exclude <- c(5, 7, 8, 25, 32, 44)

# Create a new dataframe excluding the specified plots
pila_list_clean <- pila_list %>%
  filter(!plotNum %in% plots_to_exclude)

# create a ribes A/P (0/1) column #
pila_list_clean <- pila_list_clean %>%
  mutate(ribes = ifelse(
    !is.na(ribes_50m) & !(ribes_50m %in% c("N", "None", "NONE")) |
      !is.na(ribes_100m) & !(ribes_100m %in% c("N", "None", "NONE")) |
      !is.na(ribes_150m) & !(ribes_150m %in% c("N", "None", "NONE")) |
      !is.na(ribes_200m) & !(ribes_200m %in% c("N", "None", "NONE")),
    1, 0))


#create new dataframe called 'ribespila_counts'
ribespila_counts <- pila_list_clean %>%
  group_by(plotNum) %>%
  summarize(ribes_plot = ifelse(any(ribes == 1), 1, 0))

# Find the sum of ribes_plot
ribes_plot_sum <- sum(ribespila_counts$ribes_plot)


### 36 out of 53 plots have ribes = 67.92% ###

#### Create visuals #####
# Create a summary dataframe for plotting
ribes_plot_summary <- ribespila_counts %>%
  group_by(ribes_plot) %>%
  summarize(count = n(), .groups = "drop")  # Count the number of plots for each presence/absence category

# Create the bar plot
ggplot(ribes_plot_summary, aes(x = factor(ribes_plot, labels = c("Absent", "Present")), y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of Plots with Ribes", x = "Ribes Presence", y = "Number of Plots") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        title = element_text(size = 14, face = "bold"))

###### VISUAL OF RIBES AND WPBR #######
# Assuming `pila_list_clean` includes a column for WPBR status (e.g., wpbr_present)
# Modify this based on your actual WPBR column name

# Create wpbr_present column in pila_list_clean
pila_list_clean <- pila_list_clean %>%
  mutate(wpbr_present = ifelse(
    activeBoleCanker > 0 |
      activeBranchCanker > 0 |
      inactiveBoleCanker > 0 |
      inactiveBranchCanker > 0,
    1, 0
  ))

# Create a new dataframe to combine Ribes and WPBR information
ribes_wpbr_combined <- pila_list_clean %>%
  mutate(ribes_presence = ifelse(ribes == 1, "Present", "Absent"),
         wpbr_presence = ifelse(wpbr_present == TRUE, "Present", "Absent")) %>%
  group_by(plotNum) %>%
  summarize(ribes_status = first(ribes_presence),
            wpbr_status = first(wpbr_presence), .groups = "drop")

# Create a summary table for plotting
plot_summary <- ribes_wpbr_combined %>%
  count(ribes_status, wpbr_status) %>%
  mutate(status = paste(ribes_status, wpbr_status))

### Visual 1 ###
# Create the visual using ggplot2
ggplot(plot_summary, aes(x = status, y = n, fill = status)) +
  geom_bar(stat = "identity") +
  labs(title = "Plots with Ribes and WPBR Status", x = "Ribes and WPBR Status", y = "Number of Plots") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        title = element_text(size = 14, face = "bold"))

##### NOTE: Not sure this is correctly counting how many plots have both ribes and wpbr
####### SIDE NOTE: it would be cool to make a
######### tmap showing which plots have ribes and which plots have wpbr
########## to show where infections are occuring spatially and the interaction with ribes

############## PILA PLOT CHARACTERISTICS DATAFRAME #######################
## Need to create a table
### ribes_plot (present in plot?)
### wpbr_plot (present in plot?)
### beetles_plot (present in plot?)
### alive_plot (how many trees are alive in plot)
### dead_plot (how many trees are dead in plot)

pila_plotcharacteristics <- pila_list %>%
  select(plotNum, treeNum, DBH_cm, alive_status, )
