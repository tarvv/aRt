library(tidyverse)
library(tidycensus)
library(sf)
library(ipumsr)
library(wesanderson)
library(gridExtra)
options(tigris_use_cache = TRUE)
# Census key
readRenviron("~/.Renviron")

#function for generating samples
genSamples <- function(data)
  st_sample(data, size = round(data$value / 10))

# get 2020 decennial census race data from census API
data2020 <- get_decennial("tract", table = "P1", cache_table = TRUE, year = 2020,
                      geometry = TRUE, state = "55", county = "079",
                      output = "tidy")

# get 1960 decennial census race data stored locally
# source: Steven Manson, Jonathan Schroeder, David Van Riper, Tracy Kugler, and Steven Ruggles. IPUMS National Historical Geographic Information System: Version 17.0 [dataset]. Minneapolis, MN: IPUMS. 2022. http://doi.org/10.18128/D050.V17.0
shp_zip_loc60 <- "data/nhgis0001_shape.zip"
dat_loc60 <-  "data/nhgis0001_ds92_1960_tract.csv"

data2020 <- data2020 %>%
  filter(variable == "P1_003N" | variable == "P1_004N") %>%   # filter so only rows with Black and White counts remain
  mutate(race = case_when(          # Change variable names for clarity 
    variable == "P1_003N" ~ "White",
    variable == "P1_004N" ~ "Black",
  )) %>%
  group_by(GEOID, race) %>%         # group data to work with summarize()
   summarize(value = value) #%>%   
   filter(value > 50) %>%            # filter to census tracts with > 50 people
   split(.$race)       # converts dataframe into a list of dataframes, 1 for each race

pts2020 <- map(data2020, genSamples) %>%
  imap(~st_sf(tibble(race = rep(.y, length(.x))), geometry = .x))
pts2020 <- do.call(rbind, pts2020)

pts2020 <- pts2020 %>%
  group_by(race) %>%
  summarize() %>%
  mutate(race = factor(race, levels = c("White", "Black")))

data1960 <- read_nhgis_sf(dat_loc60, shp_zip_loc60, verbose = FALSE) %>%
  st_transform(4269) %>%
  filter(STATEA == "55" & COUNTYA == "079") %>%
  select(GEOID = GISJOIN2, geometry, White = B7B001, Black = B7B002) %>%
  gather("race", "value", -GEOID, -geometry) %>%
  group_by(GEOID, race) %>%
  summarize(value = sum(value)) %>%
  filter(value > 50) %>%
  split(.$race)

pts1960 <- map(data1960, genSamples) %>%
  imap(~st_sf(data_frame(race = rep(.y, length(.x))), geometry = .x))
pts1960 <- do.call(rbind, pts1960)
pts1960 <- pts1960 %>% group_by(race) %>% summarise() %>%
  mutate(race = factor(race, levels = c("White", "Black")))

# setting theme options
theme_set(theme_minimal() +
            theme(panel.grid.major = element_line(size = 0),
                  plot.margin = unit(c(-1,-1,-1,-1), "lines"),
                  panel.spacing = unit(0, "lines"),
                  plot.background = element_rect(fill = "antiquewhite", colour = NA),
                  panel.background = element_rect(fill = "antiquewhite", colour = NA),
                  axis.title = element_blank(),
                  axis.text = element_blank(),
                  legend.position = "none"))

#PLOT DEC 2020
plot20 <- ggplot() +
  geom_sf(data = pts2020, aes(colour = race, fill = race), size = .75, stroke = 0) +
  scale_color_manual(values = wes_palette(n=2, name="Royal1")) +
  scale_alpha(range = c(.1,1)) +
  #bridge east line
  annotate("path",
           x = c(-87.931129, -87.931929, -87.932046, -87.931246),
           y = c(43.037156, 43.036156, 43.024232, 43.023232),
           size = 1, color = "black", lineend = "round") +
  #bridge west line
  annotate("path",
           x = c(-87.934729, -87.933929, -87.934046, -87.934846),
           y = c(43.037156, 43.036156, 43.024232, 43.023232),
           size = 1, color = "black", lineend = "round")

#PLOT DEC 1960
plot60 <- ggplot() +
  geom_sf(data = pts1960, aes(colour = race, fill = race), size = .5, stroke = 0) +
  scale_color_manual(values = wes_palette(n=2, name="Royal1")) +
  #bridge east line
  annotate("path",
           x = c(-87.931129, -87.931929, -87.932046, -87.931246),
           y = c(43.037156, 43.036156, 43.024232, 43.023232),
           size = 1, color = "black", lineend = "round") +
  #bridge west line
  annotate("path",
           x = c(-87.934729, -87.933929, -87.934046, -87.934846),
           y = c(43.037156, 43.036156, 43.024232, 43.023232),
           size = 1, color = "black", lineend = "round")

censusPlot <- grid.arrange(plot60, plot20, nrow = 1)

ggsave("longestBridge.png", plot = censusPlot, width = 12, height = 14)