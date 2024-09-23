# Load required libraries
librarian::shelf(sf, ggplot2, sp, readr, dplyr, tidyverse, tmap, here, RColorBrewer)


# Step 1: Load Data and Ensure Valid Format
waypoints <- read_csv(here("pila_datum.csv")) %>%
  mutate(
    plot_beg_UTM_E = as.numeric(plot_beg_UTM_E),
    plot_beg_UTM_N = as.numeric(plot_beg_UTM_N),
    zone = as.numeric(zone)
  ) %>%
  drop_na(plot_beg_UTM_E, plot_beg_UTM_N, zone)  # Remove rows with NA values

problems(waypoints)

# Step 2: Convert UTM to Lat/Lon
convert_utm_to_latlon <- function(easting, northing, zone) {
  proj_string <- paste0("+proj=utm +zone=", zone, " +datum=WGS84")
  coords <- SpatialPoints(cbind(easting, northing), proj4string = CRS(proj_string))
  latlon <- spTransform(coords, CRS("+proj=longlat +datum=WGS84"))
  return(data.frame(lat = latlon@coords[, 2], lon = latlon@coords[, 1]))
}

latlon_coords <- do.call(rbind, mapply(function(easting, northing, zone) {
  tryCatch({
    convert_utm_to_latlon(easting, northing, zone)
  }, error = function(e) {
    return(data.frame(lat = NA, lon = NA))  # Handle conversion errors
  })
}, waypoints$plot_beg_UTM_E, waypoints$plot_beg_UTM_N, waypoints$zone, SIMPLIFY = FALSE))

waypoints <- cbind(waypoints, latlon_coords) %>%
  drop_na(lat, lon)  # Remove rows with invalid lat/lon

# Step 3: Create sf Object and Reproject CRS
waypoints_sf <- st_as_sf(waypoints, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = 32611)  # Change CRS to your specific UTM zone

# Step 4: Calculate Convex Hull and Area for Each Plot
plot_areas <- waypoints_sf %>%
  group_by(plotID) %>%
  summarise(
    convex_hull = st_convex_hull(st_union(geometry)),
    area_sq_m = st_area(convex_hull)
  ) %>%
  mutate(area_sq_km = as.numeric(area_sq_m) / 1e6) %>%  # Convert to square kilometers
  st_set_geometry(NULL)  # Remove geometry column for merging

# Step 5: Join Areas Back to Original Data
waypoints_with_area <- waypoints %>%
  left_join(plot_areas, by = "plotID")

# Step 6: Create Interactive Map with tmap
waypoints_sf_with_area <- st_as_sf(waypoints_with_area, coords = c("lon", "lat"), crs = 4326)
waypoints_sf_with_area$plotID = as.factor(waypoints_sf_with_area$plotID)


# Original Set1 colors
set1_colors <- brewer.pal(9, "Set1")

# Function to generate variations of a color
generate_variations <- function(color, n_variations = 6) {
  variations <- colorRampPalette(c(adjustcolor(color, alpha.f = 0.8), color))(n_variations)
  return(variations)
}

# Generate variations for each of the 9 original Set1 colors
expanded_palette <- unlist(lapply(set1_colors, generate_variations))

tmap_mode("view")  # Set tmap to interactive mode

# Create a map showing all waypoints
tm_shape(waypoints_sf_with_area) +
  tm_dots(col = "treeNum", title = "Plot ID", size = .7, palette = expanded_palette ) +
  #tm_text("plotID", just = "center", size = 0.7, color.scale = expanded_palette) +
  tm_layout(legend.position = c("left", "top"))

## NM — tmap has it's own max color value, boo!

# Step 7: Visualize Results with ggplot
ggplot() +
  geom_sf(data = waypoints_sf_with_area, aes(color = factor(plotID))) +
  geom_sf(data = st_as_sf(plot_areas, crs = 4326), aes(fill = area_sq_km), color = "blue", alpha = 0.2) +
  ggtitle("Waypoints with Convex Hull by PlotID") +
  theme(legend.position = "right")

# Optional: Save the Updated Data
write_csv(waypoints_with_area, here("Data", "pila_datum_with_area.csv"))

######### MORE ################
#issue with calculating areas and projections of plots in wrong spots

# Read the CSV file containing the waypoints data
waypoints <- read.csv(here("Data","pila_datum.csv"))

## these waypoints are off
plot30 <- filter(waypoints, plotID==30)

# Convert the UTM columns to numeric (this will introduce NAs for any non-numeric values)
waypoints$PILA_UTM_E <- as.numeric(waypoints$PILA_UTM_E)
waypoints$PILA_UTM_N <- as.numeric(waypoints$PILA_UTM_N)

# Remove rows with missing or invalid UTM coordinates
waypoints_clean <- waypoints %>%
  filter(!is.na(PILA_UTM_E) & !is.na(PILA_UTM_N))


# Convert the cleaned waypoints to an sf object (assuming UTM zone 11N, EPSG: 32611)
waypoints_sf <- st_as_sf(waypoints_clean, coords = c("PILA_UTM_E", "PILA_UTM_N"), crs = 32611)
waypoints_sf$treeNum = as.factor(waypoints_sf$treeNum)
waypoints_sf$plotID = as.factor(waypoints_sf$plotID)

unique_palette <- rainbow(60)

tm_shape(waypoints_sf) +
  tm_bubbles(size = 0.5, col = "treeNum", palette = unique_palette, title.col = "Tree Number") +
  tm_text("plotID", just = "center", size = 0.8, col = "black") +
  tm_layout(legend.position = c("left", "top"))

# Group by plotID, calculate convex hull, and compute area in square meters
plot_areas <- waypoints_sf %>%
  group_by(plotID) %>%
  summarise(
    convex_hull = st_convex_hull(st_union(geometry)),  # Calculate the convex hull of the plot
    area_sq_m = st_area(convex_hull)  # Calculate the area of the convex hull in square meters
  )


# Optional: Visualize the plot and convex hull
ggplot() +
  geom_sf(data = waypoints_sf, aes(color = factor(plotID))) +  # Plot the tree waypoints
  geom_sf(data = plot_areas, aes(fill = factor(plotID)), alpha = 0.2) +  # Plot the convex hulls
  ggtitle("Plot Convex Hulls and Tree Waypoints") +
  theme_minimal()


## use this code to visualize the polygon of each plot
## plots: 1 6 9 10 12 26 30 31 32 37 41 43 44 48 60 68 73 75


plot_areas%>%
  filter(plotID%in%c(30)) %>%
  ggplot()+
  geom_sf(aes(fill = as.factor(plotID)), alpha = 0.5, color = 'black') +
  geom_sf(color = 'red', shape = 21) +   # Plot waypoints
  scale_fill_discrete(name = "Plot ID")

plot_areas%>%
  filter(plotID%in%c(75)) %>%
  ggplot()+
  geom_sf(aes(fill = as.factor(plotID)), alpha = 0.5, color = 'black') +
  geom_sf(color = 'red', shape = 21) +   # Plot waypoints
  scale_fill_discrete(name = "Plot ID")

plot_areas%>%
  filter(plotID%in%c(43)) %>%
  ggplot()+
  geom_sf(aes(fill = as.factor(plotID)), alpha = 0.5, color = 'black') +
  geom_sf(color = 'red', shape = 21) +   # Plot waypoints
  scale_fill_discrete(name = "Plot ID")


################################
trees_to_drop <- data.frame(
  plotID = c(43, 44, 12),
  treeNum = c(15, 8, 40)
)


# Ensure plotID and treeNum have the same type across both datasets
waypoints_clean$plotID <- as.factor(waypoints_clean$plotID)
waypoints_clean$treeNum <- as.numeric(as.character(waypoints_clean$treeNum))

trees_to_drop$plotID <- as.factor(trees_to_drop$plotID)
trees_to_drop$treeNum <- as.numeric(as.character(trees_to_drop$treeNum))

# Perform the anti_join to remove specific waypoints
waypoints_clean <- waypoints_clean %>%
  anti_join(trees_to_drop, by = c("plotID", "treeNum"))



# AREA CALCULATION #
# Ensure the UTM coordinates are numeric
waypoints_clean$PILA_UTM_E <- as.numeric(waypoints_clean$PILA_UTM_E)
waypoints_clean$PILA_UTM_N <- as.numeric(waypoints_clean$PILA_UTM_N)

# Remove any rows with NA coordinates
waypoints_clean <- waypoints_clean %>%
  filter(!is.na(PILA_UTM_E) & !is.na(PILA_UTM_N))

# Ensure the 'waypoints_clean_sf' object is an sf object with the correct CRS
waypoints_clean_sf <- st_as_sf(waypoints_clean, coords = c("PILA_UTM_E", "PILA_UTM_N"), crs = 32611)

# Convert the plot_beg_UTM_E and plot_beg_UTM_N into individual points for each plot
# This vectorizes the st_point() function to work row-wise
waypoints_clean_sf <- waypoints_clean_sf %>%
  rowwise() %>%
  mutate(waypoint_beg = st_sfc(st_point(c(plot_beg_UTM_E, plot_beg_UTM_N)), crs = 32611)) %>%
  ungroup()  # Ungroup to remove rowwise grouping

# Group by plotID and calculate length, width, and area for each plot
plot_areas <- waypoints_clean_sf %>%
  group_by(plotID) %>%
  summarise(
    # Use the 'plot_beg_UTM_E' and 'plot_beg_UTM_N' as the starting waypoint (already calculated above)
    waypoint_beg = first(waypoint_beg),

    # Calculate the length (maximum distance between waypoints in the same plot)
    length_m = max(st_distance(st_geometry(.))),

    # Calculate the width as the maximum distance between waypoints orthogonal to the length
    width_m = max(st_distance(st_geometry(.), st_geometry(.)[which.max(st_distance(st_geometry(.)))]))
  ) %>%
  mutate(
    # Compute the area as length * width
    area_sq_m = as.numeric(length_m * width_m)
  )

# View the result
print(plot_areas)


#### Area code fixed?? ###
# Ensure the 'waypoints_clean_sf' object is an sf object with the correct CRS
waypoints_clean_sf <- st_as_sf(waypoints_clean, coords = c("PILA_UTM_E", "PILA_UTM_N"), crs = 32611)

# Convert the plot_beg_UTM_E and plot_beg_UTM_N into individual points for each plot
# This vectorizes the st_point() function to work row-wise
waypoints_clean_sf <- waypoints_clean_sf %>%
  rowwise() %>%
  mutate(waypoint_beg = st_sfc(st_point(c(plot_beg_UTM_E, plot_beg_UTM_N)), crs = 32611)) %>%
  ungroup()  # Ungroup to remove rowwise grouping

# Function to calculate max distance between two points in a plot
calculate_max_distances <- function(geometry) {
  distance_matrix <- st_distance(geometry)  # Compute distance between all waypoints
  max_length <- max(distance_matrix)  # Maximum length is the furthest two points
  return(max_length)
}

# Group by plotID and calculate length, width, and area for each plot
plot_areas <- waypoints_clean_sf %>%
  group_by(plotID) %>%
  summarise(
    # Use the 'plot_beg_UTM_E' and 'plot_beg_UTM_N' as the starting waypoint (already calculated above)
    waypoint_beg = first(waypoint_beg),

    # Calculate the length as the max distance from waypoint_beg to any other point
    length_m = max(st_distance(waypoint_beg, st_geometry(.))),

    # Calculate the max distance between any two waypoints for width
    width_m = calculate_max_distances(st_geometry(.))
  ) %>%
  mutate(
    # Compute the area as length * width
    area_sq_m = as.numeric(length_m * width_m)
  )

# View the result
print(plot_areas)
