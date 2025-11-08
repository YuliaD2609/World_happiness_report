#install.packages("tmap", "dplyr")
#install.packages(c("leaflet", "rnaturalearth", "rnaturalearthdata", "sf"))

library(dplyr)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(tmap)

#Calcolo della media di felicità per paese


library(dplyr)

happiness_mean <- df %>%
  mutate(country = case_when(
    country == "Bosnia and Herzegovina" ~ "Bosnia and Herz.",
    country == "Central African Republic" ~ "Central African Rep.",
    country == "Congo (Brazzaville)" ~ "Congo",
    country == "Congo (Kinshasa)" ~ "Dem. Rep. Congo",
    country == "Dominican Republic" ~ "Dominican Rep.",
    country == "Eswatini" ~ "eSwatini",
    country == "Hong Kong S.A.R. of China" ~ "Hong Kong",
    country == "Ivory Coast" ~ "Côte d'Ivoire",
    country == "State of Palestine" ~ "Palestine",
    country == "Taiwan Province of China" ~ "Taiwan",
    country == "Turkiye" ~ "Turkey",
    country == "United States" ~ "United States of America",
    TRUE ~ country
  ))

happiness_mean <- happiness_mean %>%
  group_by(country) %>%
  summarise(mean_happiness = mean(happiness_score, na.rm = TRUE))


world <- ne_countries(scale = "medium", returnclass = "sf")

# Join tra la geometria e la media calcolata
world_happiness <- world %>%
  left_join(happiness_mean, by = c("name" = "country"))

setdiff(happiness_mean$country, world$name)


#mappa interattiva
tmap_mode("view")  # modalità interattiva

map_dynamic <- tm_shape(world_happiness) +
  tm_polygons(
    col = "mean_happiness",
    palette = "YlGnBu",
    title = "Media Felicità",
    alpha = 0.85
  ) +
  tm_layout(
    legend.title.size = 0.7,  # legenda più piccola anche in view
    legend.text.size = 0.55
  )

map_dynamic

#mappa statica
tmap_mode("plot") 
map_static <- tm_shape(world_happiness) +
  tm_polygons(
    col = "mean_happiness",
    palette = "YlGnBu",
    title = "Media Felicità",
    colorNA = "lightgray",
    textNA = "Nessun dato"
  ) +
  tm_layout(
    legend.title.size = 0.7,
    legend.text.size = 0.55,
    frame = FALSE,
    legend.position = c("left", "center")
  )

map_static 

