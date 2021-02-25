library(tidyverse)
library(tidygeocoder)
library(tigris)
library(ggtext)
library(extrafont)
library(sf)
library(lubridate)
library(scales)
library(here)

dur_dat <- read_rds(here("data/clean-citations.rds"))

us <- states()
us_small <- us %>%
  filter(!(NAME %in% c("Alaska", "American Samoa", "Commonwealth of the Northern Mariana Islands", "Guam", "Puerto Rico", "United States Virgin Islands", "Hawaii")))

durham <- counties(state = 37) %>%
  filter(NAME == "Durham")

durham_city <- places(state = 37) %>%
  filter(NAME == "Durham")

dur_dat_geocoded <- dur_dat %>%
  filter(address1 != "NULL") %>%
  mutate(clean_address = str_remove_all(string = address1, pattern = "[:punct:]")) %>%
  mutate(full_add = paste(clean_address, city, state, zip_code)) %>%
  mutate(full_add = str_trim(full_add))

batches <- dur_dat_geocoded$full_add
batches[ ] <- 1:553

test <- dur_dat_geocoded %>%
  mutate(batch = batches) %>%
  group_by(batch) %>%
  nest() %>%
  mutate(data = map(.x = data,
                    .f = ~geocode(.tbl = .x, address = full_add, lat = "lat", long = "long", return_addresses = TRUE, method = "census")))
  
final <- test %>%
  unnest(cols = data)

write_rds(x = final, file = here("data/geocoded.rds"))

final %>%
  filter(!(is.na(lat))) %>%
  filter(long > -130) %>%
  ggplot() +
  geom_sf(data = us_small, fill = "#00467f", color = "#ffffff") +
  geom_point(aes(long, lat), fill = "#ffd24f", color = "black", shape = 21) +
  labs(title = "Durham Parking Citations", subtitle = "Vehicle geocoding",
       caption = "Note: One Alaska citation excluded for the purpose of map size") +
  cowplot::theme_map() +
  theme(plot.title = element_text(family = "Roboto Condensed", size = 20, face = "bold"),
        plot.subtitle = element_text(family = "Roboto Condensed", size = 14),
        plot.caption = element_text(family = "Roboto Condensed", size = 10, hjust = 0))

ggsave(filename = "citations.png", device = "png", path = here("plots/"),
       width = 12, height = 8, dpi = "retina")

dur_cits <- final %>%
  ungroup() %>%
  filter(!(is.na(lat))) %>%
  st_as_sf(coords = c("long", "lat"), crs = st_crs(durham)) %>%
  st_intersection(durham)

ggplot() +
  geom_sf(data = durham, fill = "#00467f", color = "#ffffff") +
  geom_sf(data = dur_cits, fill = "#ffd24f", color = "black", shape = 21) +
  labs(title = "Durham Parking Citations", subtitle = "Vehicle geocoding, Durham only",
       caption = "Note: One Alaska citation excluded for the purpose of map size") +
  cowplot::theme_map() +
  theme(plot.title = element_text(family = "Roboto Condensed", size = 20, face = "bold"),
        plot.subtitle = element_text(family = "Roboto Condensed", size = 14),
        plot.caption = element_text(family = "Roboto Condensed", size = 10, hjust = 0))

ggsave(filename = "dur-citations.png", device = "png", path = here("plots/"),
       width = 5, height = 12, dpi = "retina")

dur_cits %>%
  as_tibble() %>%
  select(-geometry) %>%
  mutate(fiscal_year = ifelse(month(issue_date_time) >= 6, year(issue_date_time) + 1, year(issue_date_time))) %>%
  group_by(fiscal_year) %>%
  count()

## Citation geocoding
dur_dat_geocoded <- dur_dat %>%
  filter(citation_location != "NULL") %>%
  mutate(clean_address = str_remove_all(string = address1, pattern = "[:punct:]")) %>%
  mutate(full_add = paste(citation_block, str_to_title(citation_location), "Durham, NC")) %>%
  mutate(full_add = str_trim(full_add))

batches <- dur_dat_geocoded$full_add
batches[ ] <- 1:553

test <- dur_dat_geocoded %>%
  mutate(batch = batches) %>%
  group_by(batch) %>%
  nest() %>%
  mutate(data = map(.x = data,
                    .f = ~geocode(.tbl = .x, address = full_add, lat = "lat", long = "long", return_addresses = TRUE, method = "census")))

final <- test %>%
  unnest(cols = data) %>%
  ungroup()

write_rds(x = final, file = here("data/cit-geocoded.rds"))

dur_cits <- final %>%
  ungroup() %>%
  filter(!(is.na(lat))) %>%
  st_as_sf(coords = c("long", "lat"), crs = st_crs(durham_city))

hex_bin <- dur_cits %>%
  st_coordinates() %>%
  as_tibble()

ggplot() +
  geom_sf(data = durham_city, color = "#00467f", fill = NA) +
  #geom_sf(data = dur_cits, fill = "#ffd24f", color = "black", shape = 21, alpha = 0.6) +
  geom_hex(data = hex_bin, aes(X, Y), color = "#ffffff") +
  scale_fill_viridis_c("# of Citations", labels = comma_format()) +
  labs(title = "Durham Parking Citations", subtitle = "Citations geocoding, hexagonal bins",
       caption = "Note: One Alaska citation excluded for the purpose of map size") +
  cowplot::theme_map() +
  theme(plot.title = element_text(family = "Roboto Condensed", size = 20, face = "bold"),
        plot.subtitle = element_text(family = "Roboto Condensed", size = 14),
        plot.caption = element_text(family = "Roboto Condensed", size = 10, hjust = 0))

ggsave(filename = "cit-locations.png", device = "png", path = here("plots/"),
       width = 7, height = 10, dpi = "retina")
