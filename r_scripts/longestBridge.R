library(sf)
library(tidyverse)
library(ipumsr)
library(tidycensus)
library(wesanderson)
library(gridExtra)

# ACS5 2019 DATA
# Census key
# census_api_key("YOUR API KEY GOES HERE")
readRenviron("~/.Renviron")

#Search for data
# query <- load_variables(2019, "acs5", cache = TRUE)
# query %>%
#   mutate(table = str_extract(name, "^.+_")) %>%
#   filter(str_detect(concept, "RACE")) %>%
#   select(table, concept) %>% distinct %>% print(n = Inf)

#get 5 year census data 2019
acs19 <- get_acs("tract", table = "B02001", cache_table = TRUE, geometry = TRUE,
               state = "55", county = "079", output = "tidy")

acs19 <- acs19 %>%
  mutate(id = str_extract(variable, "[0-9]{3}$") %>% as.integer) %>%
  filter(id == 2 | id == 3) %>%
  mutate(race = case_when(
    # id == 1 ~ "Sum All",
    id == 2 ~ "White",
    id == 3 ~ "Black",
    # id == 4 ~ "Native American",
    # id == 5 ~ "Asian",
    # id == 6 ~ "Pacific Islander",
    # id == 7 ~ "Some Other",
    # id > 7 ~ "Two or More"
  )) %>%
  group_by(GEOID, race) %>%
  summarize(estimate = sum(estimate)) %>%
  filter(estimate > 50) %>%
  split(.$race)

generate_samples <- function(data)
  suppressMessages(st_sample(data, size = round(data$estimate / 10)))

pts19 <- map(acs19, generate_samples) %>%
  imap(~st_sf(data_frame(race = rep(.y, length(.x))), geometry = .x))

pts19 <- do.call(rbind, pts19)

pts19 <- pts19 %>% group_by(race) %>%
  summarise() %>%
  mutate(race = factor(race, levels = c("White", "Black")))


# DECCENIAL 1960'S DATA
shp_zip_loc60 <- paste0(getwd(), "/misc/nhgis0001_shape.zip")
dat_loc60 <- paste0(getwd(), "/data/nhgis0001_ds92_1960_tract.csv")

dec60 <- read_nhgis_sf(dat_loc60, shp_zip_loc60, verbose = FALSE) %>%
  st_transform(4269) %>%
  filter(STATEA == "55" & COUNTYA == "079") %>%
  select(GISJOIN2, geometry, B7B001, B7B002) %>%
  rename("GEOID" = GISJOIN2, "White" = B7B001, "Black" = B7B002) %>%
  gather("race", "estimate", -GEOID, -geometry) %>%
  group_by(GEOID, race) %>%
  summarize(estimate = sum(estimate)) %>%
  filter(estimate > 50) %>%
  split(.$race)

generate_samples60 <- function(data)
  suppressMessages(st_sample(data, size = round(data$estimate / 10)))

pts60 <- map(dec60, generate_samples60) %>%
  imap(~st_sf(data_frame(race = rep(.y, length(.x))), geometry = .x))
pts60 <- do.call(rbind, pts60)
pts60 <- pts60 %>% group_by(race) %>% summarise() %>%
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

#PLOT ACS5 2019
p19 <- ggplot() +
  geom_sf(data = pts19, aes(colour = race, fill = race), size = .5, stroke = 0) +
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
           size = 1, color = "black", lineend = "round") #+
  #Freedom house
  # annotate("point", y=43.049870142373045, x=-87.93149064420359, color="black") +
  # coord_sf(xlim = c(-88, -87.82713), ylim = c(42.96, 43.1))


#PLOT DEC 1960'S
p60 <- ggplot() +
  geom_sf(data = pts60, aes(colour = race, fill = race), size = .5, stroke = 0) +
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
           size = 1, color = "black", lineend = "round") #+
  #Freedom house
  # annotate("point", y=43.049870142373045, x=-87.93149064420359, color="black") +
  # coord_sf(xlim = c(-88, -87.82713), ylim = c(42.96, 43.1))

pmain <- grid.arrange(p60, p19, nrow = 1)

ggsave("fin/longestBridge.png", plot = pmain, width = 14, height = 14)