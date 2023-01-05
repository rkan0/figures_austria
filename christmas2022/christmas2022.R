library(osmdata)
library(tidyverse)
library(sf)
library(ggplot2)
library(gganimate)
library(ggtext)
library(showtext)

font_add("wien_norm", "~/Library/Fonts/wiener_norm.otf")
font_add_google("Merriweather", "merriweather")
showtext_auto()
sf_extSoftVersion()

# specify area to be loaded from osm
getbb("Vienna Austria")
bb <- getbb("Josefstadt", featuretype = "settlement", format_out = "sf_polygon") 

# load specified features
# didnt include footpath for cleaner map
streets <- getbb("Josefstadt", featuretype = "settlement") %>% 
  opq() %>% 
  add_osm_feature(key = "highway", value = c("residential", "living_street", "unclassified", "pedestrian", "road")) %>% 
  osmdata_sf() %>% 
  trim_osmdata(bb)
st_crs(streets$osm_lines) = st_crs(4326)

roads <- getbb("Josefstadt", featuretype = "settlement")  %>% opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "secondary", "tertiary")) %>%
  osmdata_sf()
st_crs(roads$osm_lines) = st_crs(4326)


# load border of Josefstadt
boundary <- getbb("Josefstadt", featuretype = "settlement") %>% 
  opq() %>% 
  add_osm_feature(key = "admin_level", value = "9") %>% 
  osmdata_sf() %>% 
  trim_osmdata(bb)
st_crs(boundary$osm_multipolygons) = st_crs(4326)
# check downloaded data
# View(boundary$osm_multipolygons)
# head(boundary$osm_multipolygons[1,])

# some of the streets are not in 8.
View(roads$osm_lines)

# load neighbouring districts to cover them up 
alsergrund <- getbb("Alsergrund", featuretype = "settlement") %>%   opq() %>% 
  add_osm_feature(key = "admin_level", value = "9") %>% 
  osmdata_sf()
st_crs(alsergrund$osm_multipolygons) = st_crs(4326)

#### GIF ####

# get data of lights
# weeks coded as 1-6; 7 = didn't have any decorations
anim_data <- data.frame(
  id = 1:10,
  christmas = sample(1:7, 10, replace = TRUE)
)

anim_data <- anim_data %>%
  # recode later to make reshapes easier
  mutate(christmas = ifelse (is.na(christmas), 7, christmas)) %>%
  mutate(time1 = if_else(christmas <= 1, "have", "without")) %>%
  mutate(time2 = if_else(christmas <= 2, "have", "without")) %>%
  mutate(time3 = if_else(christmas <= 3, "have", "without")) %>%
  mutate(time4 = if_else(christmas <= 4, "have", "without")) %>%
  mutate(time5 = if_else(christmas <= 5, "have", "without")) %>%
  mutate(time6 = if_else(christmas <= 6, "have", "without")) %>%
  mutate(time7 = if_else(christmas < 7, "have", "without"))


# coordinates of shops
points <- data.frame(
  id = 1:10,
  x = runif(n = 10, min = 16.344, max = 16.355),
  y = runif(n = 10, min = 48.209, max = 48.215)
)
points_long <- points %>% st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  st_transform(crs = st_crs(4326))
points_long <- dplyr::inner_join(points_long, anim_data, by = "id") %>%
  pivot_longer(time1:time7, names_to = "time_point", values_to = "status") %>%
  mutate(time_point = recode(time_point, "time1" = "6", "time2" = "5", "time3" = "4", "time4" = "3", "time5" = "2", "time6" = "1", "time7" = "0")) %>% 
  mutate(time_point = factor(time_point, levels = c("6", "5", "4", "3", "2", "1", "0")))


anim_title<-"<span style = 'color: white;font-family =wien_norm'> WEEKS TO CHRISTMAS  {closest_state}</span>"
anim_subtitle<-"<span style = 'color: white;font-family =merriweather'>When do the decorations go up in shops in the 8th district? </span>"
caption<-"<p style='color:white;font-family =merriweather'>Map: {osmdata}<br>Data &#38; figure: &#64;rkan&#64;fosstodon.org"


sf_use_s2(FALSE)
anim <- ggplot() +
  geom_sf(data = streets$osm_lines, # plot streets
          inherit.aes = FALSE,
          color = "#626161") +
  geom_sf(data = roads$osm_lines, # plot roads
          inherit.aes = FALSE,
          color = "#626161") +
  geom_sf(data = points_long$geometry, # plot shops
          size = 3, #alpha = 0.7,
          aes(
            color = as.factor(points_long$status), 
            shape = as.factor(points_long$status))) +
  scale_colour_manual(values = c( "#FFFF57", "#cccccc"))+
  scale_shape_manual(values = c(18, 4)) +
  geom_sf(data = alsergrund$osm_multipolygons[1,], #outline 7.
          inherit.aes = FALSE,
          color = "black", fill = "black") +
  geom_sf(data = alsergrund$osm_multipolygons[2,], #outline 1.
          inherit.aes = FALSE,
          color = "black", fill = "black") +
  geom_sf(data = boundary$osm_multipolygons[1,], #outline 8.
          inherit.aes = FALSE,
          color = "#626161", fill = NA) +
  coord_sf(xlim = c(16.34300, 16.35700),
           ylim = c(48.20850, 48.21600), 
           expand = FALSE) +
  theme_void() +
  labs(title = anim_title,
       subtitle = anim_subtitle,
       caption = caption) +
  theme(plot.margin = margin(l = 20, r = 0, b = 10, t = 20),
        plot.title = element_textbox_simple(size = 20, family = "wien_norm"),
        plot.subtitle = element_textbox_simple(size = 15), 
        plot.caption = element_textbox_simple(size = 10, margin = margin(b = 5)),
        plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_blank(),
        legend.position = "none"
  )   +
  transition_states(
    points_long$time_point, 
    transition_length = 0, 
    state_length = 1
  )

animate(anim, nframes = 300, fps = 30, end_pause = 100,renderer = gifski_renderer("anim.gif"))


#### PNG ####

# coordinates of shops
points <- data.frame(
  id = 1:10,
  x = runif(n = 10, min = 16.344, max = 16.355),
  y = runif(n = 10, min = 48.209, max = 48.215)
)
points_st <- points %>% st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  st_transform(crs = st_crs(4326))

# get data of lights
# weeks coded as 1-6; 7 = didn't have any decorations
data <- data.frame(
  id = 1:10,
  christmas = sample(1:7, 10, replace = TRUE)
)

data %>%
  group_by(christmas)%>%
  dplyr::summarise(count = n())

# join points to data
length(unique(points[["id"]]))
length(unique(data[["id"]]))
points_st <- dplyr::inner_join(data, points_st, by = "id")
length(unique(points_st[["id"]]))

title<-"<span style = 'color: white;font-family =wien_norm'>WHEN DOES THE CHRISTMAS SEASON BEGIN?</span>"
subtitle<-"<span style = 'color: white;font-family =merriweather'><br>Most shops (98%) in the 8th district of Vienna from Josefstädter Straße to Skodagasse that put up Christmas decorations in their windows did so by the end of the Second Week of Advent. </span>"
caption<-"<p style='color:white;font-family =merriweather'>Map: {osmdata}<br>Data &#38; figure: &#64;rkan&#64;fosstodon.org"


sf_use_s2(FALSE)
ggplot() +
  geom_sf(data = streets$osm_lines, # plot streets
          inherit.aes = FALSE,
          color = "#626161") +
  geom_sf(data = roads$osm_lines, # plot roads
          inherit.aes = FALSE,
          color = "#626161") +
  geom_sf(data = points_st$geometry, # plot shops
          aes(color = as.factor(points_st$christmas), 
              shape = as.factor(points_st$christmas)), 
          size = 2, alpha = 0.7, show.legend = TRUE) +
  geom_sf(data = points_st$geometry, # add 'glowing' effect
          aes(color = as.factor(points_st$christmas), 
              shape = as.factor(points_st$christmas)), 
          size = 3, alpha = 0.6, show.legend = TRUE) +
  scale_colour_manual(name = "Decorations were put up in the",
                      labels = c("2nd week before Advent", "Week before Advent", "1st Week of Advent", "2nd Week of Advent", "3rd Week of Advent", "4th Week of Advent", "(none)"),
                      values = c( "#FFFF57", "#CBF247", "#59D63C", "#29A7AA", "#2a5B96", "#3130Ea", "#626161")) +
  scale_shape_manual(name = "Decorations were put up in the",
                     labels = c("2nd week before Advent", "Week before Advent", "1st Week of Advent", "2nd Week of Advent", "3rd Week of Advent", "4th Week of Advent", "(none)"),
                     values = c(18, 18, 18, 18, 18, 18, 4)) +
  geom_sf(data = alsergrund$osm_multipolygons[1,], #outline 7.
          inherit.aes = FALSE,
          color = "black", fill = "black") +
  geom_sf(data = alsergrund$osm_multipolygons[2,], #outline 1.
          inherit.aes = FALSE,
          color = "black", fill = "black") +
  geom_sf(data = boundary$osm_multipolygons[1,], #outline 8.
          inherit.aes = FALSE,
          color = "#626161", fill = NA) +
  coord_sf(xlim = c(16.34300, 16.35700),
           ylim = c(48.20850, 48.21600), 
           expand = FALSE) +
  theme_void() +
  labs(
    title = title,
   subtitle = subtitle,
    caption = caption, 
  ) +
   theme(plot.margin = margin(l = 20, r = 0, b = 10, t = 20),
         plot.title = element_textbox_simple(size = 15, family = "wien_norm"),
         plot.subtitle = element_textbox_simple(size = 10), 
         plot.caption = element_textbox_simple(size = 7, margin = margin(b = 5)),
         plot.background = element_rect(fill = "black", color = "black"),
         panel.background = element_blank(),
         legend.title = element_text(color = "white", size = 8),
         legend.text = element_text(color = "white", size = 8), # using element_markdown ruins spacing; changing font also looks strange
         legend.margin = margin(t = 4, r = 0, b = 0, l = 0, unit = "cm")
   ) 



# notes
# scale_*_manual for legend needs to be wrapped in aes as factor
# use fill = to change multipolygon fill colour

