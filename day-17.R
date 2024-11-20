library(tidyverse)
library(tidycensus)
library(sf)
library(ggtext)
library(glue)
library(basemaps)
library(ggimage)
library(magick)
library(cropcircles)
library(patchwork)

?crop_circle
?geometry_area
############## 1. Import Data ####################################################

NYC_CRS = "EPSG:32118"

# Get NJ data from 2020 census
NJ_counties <- get_decennial(year = 2020,
                      geography = "county",
                      state = c("New Jersey"),
                      variables = c("H3_001N", "H3_003N"),
                      geometry = T,
                      sumfile = "dhc",
                      output="wide") %>%
  st_transform(NYC_CRS) %>%
  rename(total_housing = "H3_001N", 
         vacant = "H3_003N")

# Read NYC neighborhoods
nyc_neighborhoods <- st_read("data/Neighborhoods Boundries.geojson") %>%
  st_transform(crs = NYC_CRS)

# Manually enter the places
food_spots_matrix <- matrix(data = c(40.64190691623345, -74.00295993041696, "Hainan Chicken House",
                                     40.760697917518925, -73.98220580734326, "Urban hawker",
                                     40.737450025718324, -73.99114771713131, "Laut",
                                     40.71388934487965, -73.99102859832436, "Kopitiam",
                                     40.74038859494913, -73.94648330481917, "Lemak Kitchen"
                              ), ncol = 3, byrow = T)

############## 2. Process Data ##################################################


# Convert to sf object
food_spots <- data.frame(lat = food_spots_matrix[,1],
                   lon = food_spots_matrix[,2],
                   name = food_spots_matrix[,3]) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = NYC_CRS)

# Create normal df
food_img = as.data.frame(st_coordinates(food_spots))
food_img$img = c("data/hainan.jpg",
                 "data/hawker.jpg",
                 "data/kopitiam.jpg",
                 "data/laut.jpg",
                 "data/lemak.png")

food_img$img <- crop_circle(food_img$img)

############## 3. Create Map ##################################################


title_text = glue("Restaurants to grab <br><b><span style = 'color:#EF3340;'>Singaporean</span></b> food in NYC and Long Island<br>")

p <- ggplot() + 
  xlim(291000, 315000) + 
  ylim(50000, 71000) + 
  geom_sf(data = NJ_counties, fill = "lightgrey", colour = "white") + 
  geom_sf(data = nyc_neighborhoods, fill = "lightgrey", colour = "white") + 
  geom_sf(data = food_spots, size = 3, colour = "#EF3340") + 
  labs(title = title_text) + 
  theme_void() + 
  theme(plot.title = element_markdown(size=20, family = "Lato")) + 
  

  geom_segment(aes(x = 299749, xend = 299749 + 1000, y = 52773, yend = 52773 + 2000), colour = "#EF3340") + 
  geom_segment(aes(x = 299749 + 1000, xend = 299749 + 6000, y = 52773 + 2000, yend = 52773 + 2000), colour = "#EF3340") + 
  annotate("text", x = 299749 + 1000, y = 52773 + 3000, label = "Hainan\nChicken House", color = "#EF3340", size = 4, fontface = "bold", hjust = 0, family = "Lato") + 
  geom_image(data = food_img[1,], aes(image = img, x = 299749 + 7000, y = 52773 + 3000), size = 0.1) + 

  geom_segment(aes(x = 300747, xend = 300747 - 1000, y = 63383, yend = 63383 + 2000), colour = "#EF3340") + 
  geom_segment(aes(x = 300747 - 1000, xend = 300747 - 5000, y = 63383 + 2000, yend = 63383 + 2000), colour = "#EF3340") + 
  annotate("text", x = 300747 - 2500, y = 63383 + 2500, label = "Laut", color = "#EF3340", size = 4, fontface = "bold", hjust = 0, family = "Lato") + 
  geom_image(data = food_img[4,], aes(image = img, x = 300747 - 6000, y = 63383 + 3000), size = 0.1) + 
  
  geom_segment(aes(x = 301502, xend = 301502 + 1000, y = 65964, yend = 65964 + 2000), colour = "#EF3340") + 
  geom_segment(aes(x = 301502 + 1000, xend = 301502 + 5000, y = 65964 + 2000, yend = 65964 + 2000), colour = "#EF3340") + 
  annotate("text", x = 301502 + 1000, y = 65964 + 3000, label = "Urban\nHawker", color = "#EF3340", size = 4, fontface = "bold", hjust = 0, family = "Lato") + 
  geom_image(data = food_img[2,], aes(image = img, x = 301502 + 6000, y = 65964 + 3000), size = 0.1) +  
  
  geom_segment(aes(x = 304520, xend = 304520 + 1000, y = 63710, yend = 63710 - 2000), colour = "#EF3340") + 
  geom_segment(aes(x = 304520 + 1000, xend = 304520 + 5000, y = 63710 - 2000, yend = 63710 - 2000), colour = "#EF3340") + 
  annotate("text", x = 304520 + 1000, y = 63710 - 1000, label = "Lemak\nKitchen", color = "#EF3340", size = 4, fontface = "bold", hjust = 0, family = "Lato") + 
  geom_image(data = food_img[5,], aes(image = img, x = 304520 + 6000, y = 63710 - 1000), size = 0.1) +  
  
  geom_segment(aes(x = 300758, xend = 300758 - 1000, y = 60766, yend = 60766 - 2000), colour = "#EF3340") + 
  geom_segment(aes(x = 300758 - 1000, xend = 300758 - 5000, y = 58766, yend = 58766), colour = "#EF3340") + 
  annotate("text", x = 300758 - 3700, y = 58766 - 500, label = "Kopitiam", color = "#EF3340", size = 4, fontface = "bold", hjust = 0, family = "Lato") + 
  geom_image(data = food_img[5,], aes(image = img, x = 300758 - 6000, y = 58766 - 1000), size = 0.1)
  


ggsave("output/day-19-half.jpg", p, bg = "white", width = 8, height = 8)

############## 3A. Byron Map ##################################################
poi <- st_read("./sg-data/nypizzas.geojson")
river <- st_read("./sg-data/focus-rivers-clipped.geojson")
land <- st_read("./sg-data/focus-area-clipped.geojson")
sea <- st_read("./sg-data/focus-sea.geojson")


title_text_sg = glue("Restaurants to grab <br><b><span style = 'color:#003fb4;'>New York</span></b> pizzas in Singapore")

q <- ggplot() +
  geom_sf(data = sea, fill = "#ffffff", color = NA) +
  geom_sf(data = land, fill = "#d3d3d3", color = NA) +
  geom_sf(data = river, fill = "#ffffff", color = NA) +
  geom_sf(data = poi, shape = 15, color = "#003fb4", size = 2) +
  geom_sf_text(data = poi, aes(label = Restaurant), color = "#003fb4", size = 3.5, family = "Lato", fontface = "bold", vjust = -0.8) +
  ggtext::geom_textbox(
    aes(x = 103.877, y = 1.233, 
        label = "Collaborative Map"), 
    family = "Lato", color = "black", 
    box.color = NA, fill = NA, size = 4, hjust = 0.5, vjust = 0.5,
    width = unit(3, "in"), 
    halign = 1, valign = 0, lineheight = 1.2
  ) +
  ggtext::geom_textbox(
    aes(x = 103.877, y = 1.225, 
        label = "30 day map challenge<br>Day #17<br>Culinary Crossovers<br>New York & Singapore<br>Sean & Byron"), 
    family = "Lato", color = "black", 
    box.color = NA, fill = NA, size = 3, hjust = 0.5, vjust = 0.5,
    width = unit(3, "in"), 
    halign = 1, valign = 0, lineheight = 1.2
  ) +
  ggimage::geom_image(
    aes(x = 103.846, y = 1.274, image = "./sg-data/Blue.png"),
    size = 0.07
  ) +
  ggimage::geom_image(
    aes(x = 103.805, y = 1.28, image = "./sg-data/Johns.png"),
    size = 0.07
  ) +
  ggimage::geom_image(
    aes(x = 103.822, y = 1.248, image = "./sg-data/Louis.png"),
    size = 0.07
  ) +
  ggimage::geom_image(
    aes(x = 103.870, y = 1.287, image = "./sg-data/Sonny.png"),
    size = 0.07
  ) +
  ggimage::geom_image(
    aes(x = 103.856, y = 1.302, image = "./sg-data/Yellow.png"),
    size = 0.07
  ) +
  labs(title = title_text_sg) + 
  theme_void() + 
  theme(plot.title = element_markdown(size=20, family = "Lato"),
        text = element_text(family = "Lato")) 



############## 4. Combine and GPT ##################################################

# Combine the plots into a list
plots <- list(p, q)

# Use wrap_plots to arrange the plots
combined_plot <- wrap_plots(plots, ncol = 2)


# Display the combined plot
# combined_plot

# Save the combined plot
ggsave("output/17-Sean-Byron-Collab.png", combined_plot, bg = "white", width = 16, height = 8)

