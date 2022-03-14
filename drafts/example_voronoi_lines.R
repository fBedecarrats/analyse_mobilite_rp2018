library(sf)
library(stplanr)
library(dplyr)
library(purrr)

# data
osm_net_example <- st_transform(osm_net_example, 27700) 
l1 <- osm_net_example[1, ]
example <- osm_net_example[l1, ] %>% 
  mutate(my_ID = as.character(seq_len(nrow(.)))) %>% 
  st_set_agr("constant")

# change example data to points
example_points <- st_cast(example, "POINT")
example_unique_points <- example_points %>% 
  filter(!duplicated(geometry))
# I don't need the points shared between two or more lines and, most important,
# I need a unique ID for each point later
example_unique_multipoints <- example_unique_points %>% 
  group_by(my_ID) %>% 
  summarise()

voronoi_polygons <- st_voronoi(
  # https://github.com/r-spatial/sf/issues/824
  x = do.call("c", st_geometry(example_unique_multipoints))
) %>% 
  st_collection_extract() %>% 
  st_set_crs(27700)

voronoi_polygons_for_lines <- voronoi_polygons %>% 
  st_set_crs(27700) %>% 
  # now I need to merge the polygons associated with the same line
  # https://github.com/r-spatial/sf/issues/1030
  st_cast("MULTIPOLYGON", ids = unlist(st_intersects(voronoi_polygons, example_unique_multipoints))) %>% 
  st_union(by_feature = TRUE)

par(mar = rep(0, 4))
plot(voronoi_polygons_for_lines, col = sf.colors(3), reset = FALSE)
plot(st_geometry(example), lwd = 4, add = TRUE, col = "black")

# Now calculate the buffers
example_buffer <- st_buffer(example, dist = 20)

# Now I'd like to take the intersection of each buffer with the corresponding
# voronoi polygon. The problem is that if I run
st_intersection(st_geometry(example_buffer), voronoi_polygons_for_lines)

# I'm not sure how to solve that. For the moment I use map2
my_solution <- map2(
  .x = st_geometry(st_buffer(example, dist = 20)), 
  .y = voronoi_polygons_for_lines, 
  .f = st_intersection
  ) %>% 
  st_sfc()

# this is the result
plot(my_solution, col = sf.colors(length(my_solution), alpha = 0.5))
