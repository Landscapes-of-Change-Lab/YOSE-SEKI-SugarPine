

########################################
# Load required libraries
library(sf)
library(dplyr)

# Ensure waypoints_clean is an sf object with the correct CRS
waypoints_clean_sf <- st_as_sf(waypoints_clean, coords = c("PILA_UTM_E", "PILA_UTM_N"), crs = 32611)

# Step 1: Define the starting waypoint (waypoint_beg) for each plot
waypoints_clean_sf <- waypoints_clean_sf %>%
  group_by(plotID) %>%
  mutate(
    waypoint_beg = st_sfc(st_point(c(first(plot_beg_UTM_E), first(plot_beg_UTM_N))), crs = 32611)
  ) %>%
  ungroup()

# Step 2: Function to calculate the maximum distance within a plotID
calculate_distances_within_plot <- function(geometry, waypoint_beg) {
  # Calculate distances from the starting waypoint to all other waypoints in the same plot
  dist_from_beg <- st_distance(waypoint_beg, geometry)  # Distance from the starting waypoint
  furthest_distance <- max(dist_from_beg)  # Maximum distance from waypoint_beg within the same plot
  return(furthest_distance)
}

# Step 3: Group by plotID and calculate length, width, and area for each plot
plot_areas <- waypoints_clean_sf %>%
  group_by(plotID) %>%
  summarise(
    waypoint_beg = first(waypoint_beg),  # Store the starting waypoint
    length_m = calculate_distances_within_plot(geometry, first(waypoint_beg)),  # Maximum distance for length within plot
    # Calculate the maximum width between the two furthest waypoints within the same plot
    width_m = max(as.numeric(st_distance(geometry)), na.rm = TRUE),  # Maximum distance between waypoints within plot
    area_sq_m = length_m * width_m,  # Compute area as length * width
    area_sq_km = area_sq_m / 1e6  # Convert to square kilometers
  ) %>%
  ungroup()  # Ungroup after summarization

# View results
print(plot_areas)

