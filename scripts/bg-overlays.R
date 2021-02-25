library(tidyverse)
library(tidycensus)
library(tigris)
library(ggtext)
library(extrafont)
library(sf)
library(lubridate)
library(scales)
library(here)
library(janitor)
library(patchwork)
library(glue)

# Load logos
d_logo <- png::readPNG(source = here("logo/durham.png"), native = TRUE)
n_logo <- png::readPNG(source = here("logo/black-logo-long.png"), native = TRUE)

# Census API Key
source(here("scripts/api-key.R"))

# Load data
dat <- read_rds(here("data/geocoded.rds"))

# Grab block groups
us_bg <- block_groups(state = 37)
dur_bg <- block_groups(state = 37, county = 63)

# Block group overlay
check <- dat %>%
  filter(!(is.na(long))) %>%
  st_as_sf(coords = c("long", "lat"), crs = st_crs(us_bg)) %>%
  st_intersection(us_bg) %>%
  clean_names()

# Durham block group overlay
check_dur <- dat %>%
  filter(!(is.na(long))) %>%
  st_as_sf(coords = c("long", "lat"), crs = st_crs(dur_bg)) %>%
  st_intersection(dur_bg) %>%
  clean_names()

# Getting rid of geometry to make it a smaller tibble object
check_clean <- check %>%
  as_tibble() %>%
  select(-geometry)

check_clean %>%
  write_csv(here("data/bg-overlay.csv"))

# Load census variables
vars <- load_variables(year = 2019, dataset = "acs5")

# Fetching white alone data from Census
white_alone <- get_acs(geography = "block group", variables = "B02001_002",
                       state = 37, survey = "acs5", key = api_key, summary_var = "B02001_001")

# Cleaning census data
white_alone %>%
  clean_names() %>%
  filter(summary_est != 0) %>%
  mutate(pct = estimate / summary_est) %>%
  mutate(high = estimate + moe) %>%
  mutate(low = estimate - moe) %>%
  mutate(high_pct = high / summary_est) %>%
  mutate(low_pct = ifelse(low <= 0, 0, low / summary_est)) %>%
  mutate(majority_minority = ifelse(pct < .50, TRUE, FALSE))-> white_alone_clean

# Creating plot 1
p1 <- white_alone_clean %>%
  tabyl(majority_minority) %>%
  ggplot(aes(majority_minority, percent)) +
  geom_col(fill = "#ee3a43") +
  geom_text(aes(y = percent - 0.08, label = paste0(percent(percent), "\n", comma(n))), color = "#ffffff", size = 7, family = "Roboto Condensed") +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0,0.04))) +
  scale_x_discrete(labels = c("IS NOT Majority-Minority", "IS Majority-Minority")) +
  labs(x = NULL, y = NULL, title = "How many majority-minority block groups are there in NC?") +
  theme_minimal() +
  theme(axis.text = element_text(family = "Roboto Condensed", size = 10),
        axis.text.x = element_text(face = "bold"),
        axis.title = element_text(family = "Roboto Condensed", size = 12),
        plot.title = element_text(family = "Roboto Condensed", face = "bold", size = 18, color = "#00467f"),
        plot.subtitle = element_text(family = "Roboto Condensed", color = "#00467f"),
        axis.line = element_line(color = "black", size = 0.02))

# Merging data for block groups
test <- check_clean %>%
  clean_names() %>%
  left_join(white_alone_clean) %>%
  filter(!(is.na(majority_minority))) %>%
  tabyl(majority_minority)

# Creating second plot for stacking
p2 <- test %>%
  ggplot(aes(majority_minority, percent)) +
  geom_col(fill = "#ee3a43") +
  geom_text(aes(y = percent - 0.07, label = paste0(percent(percent), "\n", comma(n))), color = "#ffffff", size = 7, family = "Roboto Condensed") +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0,0.04))) +
  scale_x_discrete(labels = c("IS NOT Majority-Minority", "IS Majority-Minority")) +
  labs(x = NULL, y = NULL, title = "How many citations were issued to vehicles from\nmajority-minority block groups?") +
  theme_minimal() +
  theme(axis.text = element_text(family = "Roboto Condensed", size = 10),
        axis.text.x = element_text(face = "bold"),
        axis.title = element_text(family = "Roboto Condensed", size = 12),
        plot.title = element_text(family = "Roboto Condensed", face = "bold", size = 18, color = "#00467f"),
        plot.subtitle = element_text(family = "Roboto Condensed", color = "#00467f"),
        axis.line = element_line(color = "black", size = 0.02))

# Creating a saving stacked plot
p1 / p2 &
  theme(plot.caption = element_text(hjust = 0, family = "Roboto Condensed"), 
        plot.margin = unit(x = c(1, 1, 3.5, 1), units = "lines"))

ggsave(filename = "maj-min.png", device = "png", path = here("plots/"), height = 12, width = 8, dpi = "retina")

# Fetching per capita income
pci <- get_acs(geography = "block group", variables = "B19301_001",
                   state = 37, survey = "acs5", key = api_key)

# Cleaning census data
pci %>%
  clean_names() %>%
  filter(estimate != 0) %>%
  mutate(high = estimate + moe) %>%
  mutate(low = estimate - moe) %>%
  mutate(cats = cut_interval(estimate, n = 10)) -> pci_clean

test <- pci_clean %>%
  filter(!(is.na(estimate))) %>%
  arrange(cats) %>%
  tabyl(cats) %>%
  mutate(upper = str_extract(cats, "[^(]*(?=,)")) %>%
  mutate(lower = str_extract(cats, "(?<=,)[^\\]]*")) %>%
  mutate(upper = str_remove(upper, "\\[")) %>%
  mutate(label = glue("{dollar(as.numeric(upper))} to {dollar(as.numeric(lower))}")) %>%
  mutate(label = str_wrap(label, width = 10))

p1 <- test %>%
  ggplot(aes(cats, percent)) +
  geom_col(fill = "#ee3a43") +
  geom_text(data = filter(test, percent > 0.1), aes(y = percent - 0.06, label = paste0(percent(percent, accuracy = 0.01), "\n", comma(n, accuracy = 1))), color = "#ffffff", size = 5, family = "Roboto Condensed") +
  geom_text(data = filter(test, percent < 0.1), aes(y = percent + 0.06, label = paste0(percent(percent, accuracy = 0.01), "\n", comma(n, accuracy = 1))), color = "#00467f", size = 5, family = "Roboto Condensed") +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0,0.04))) +
  scale_x_discrete(labels = test$label) +
  labs(x = NULL, y = NULL, title = "Block Group Per Capita Income for NC", 
       subtitle = "Per Capita Income block groups separated into 10 equal interval categories") +
  theme_minimal() +
  theme(axis.text = element_text(family = "Roboto Condensed", size = 10),
        axis.text.x = element_text(face = "bold"),
        axis.title = element_text(family = "Roboto Condensed", size = 12),
        plot.title = element_text(family = "Roboto Condensed", face = "bold", size = 24, color = "#00467f"),
        plot.subtitle = element_text(family = "Roboto Condensed", color = "#00467f"),
        axis.line = element_line(color = "black", size = 0.02))

test <- check_clean %>%
  clean_names() %>%
  left_join(pci_clean) %>%
  filter(!(is.na(estimate))) %>%
  arrange(cats) %>%
  tabyl(cats) %>%
  mutate(upper = str_extract(cats, "[^(]*(?=,)")) %>%
  mutate(lower = str_extract(cats, "(?<=,)[^\\]]*")) %>%
  mutate(upper = str_remove(upper, "\\[")) %>%
  mutate(label = glue("{dollar(as.numeric(upper))} to {dollar(as.numeric(lower))}")) %>%
  mutate(label = str_wrap(label, width = 10))

p2 <- test %>%
  ggplot(aes(cats, percent)) +
  geom_col(fill = "#ee3a43") +
  geom_text(data = filter(test, percent > 0.1), aes(y = percent - 0.04, label = paste0(percent(percent, accuracy = 0.01), "\n", comma(n, accuracy = 1))), color = "#ffffff", size = 5, family = "Roboto Condensed") +
  geom_text(data = filter(test, percent < 0.1), aes(y = percent + 0.04, label = paste0(percent(percent, accuracy = 0.01), "\n", comma(n, accuracy = 1))), color = "#00467f", size = 5, family = "Roboto Condensed") +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0,0.04))) +
  scale_x_discrete(labels = test$label) +
  labs(x = NULL, y = NULL, title = "Citations and Block Group Per Capita Income for NC", 
       subtitle = "Per Capita Income block groups separated into 10 equal interval categories") +
  theme_minimal() +
  theme(axis.text = element_text(family = "Roboto Condensed", size = 10),
        axis.text.x = element_text(face = "bold"),
        axis.title = element_text(family = "Roboto Condensed", size = 12),
        plot.title = element_text(family = "Roboto Condensed", face = "bold", size = 24, color = "#00467f"),
        plot.subtitle = element_text(family = "Roboto Condensed", color = "#00467f"),
        axis.line = element_line(color = "black", size = 0.02))

p1 / p2 &
  theme(plot.caption = element_text(hjust = 0, family = "Roboto Condensed"), 
        plot.margin = unit(x = c(1, 1, 3.5, 1), units = "lines"))

ggsave(filename = "pci.png", device = "png", path = here("plots/"), height = 12, width = 10, dpi = "retina")


######## Again but for Durham only
p1 <- white_alone_clean %>%
  filter(str_detect(name, "Durham")) %>%
  tabyl(majority_minority) %>%
  ggplot(aes(majority_minority, percent)) +
  geom_col(fill = "#ee3a43") +
  geom_text(aes(y = percent - 0.08, label = paste0(percent(percent), "\n", comma(n))), color = "#ffffff", size = 7, family = "Roboto Condensed") +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0,0.04))) +
  scale_x_discrete(labels = c("IS NOT Majority-Minority", "IS Majority-Minority")) +
  labs(x = NULL, y = NULL, title = "How many majority-minority block groups are there in Durham County?") +
  theme_minimal() +
  theme(axis.text = element_text(family = "Roboto Condensed", size = 10),
        axis.text.x = element_text(face = "bold"),
        axis.title = element_text(family = "Roboto Condensed", size = 12),
        plot.title = element_text(family = "Roboto Condensed", face = "bold", size = 18, color = "#00467f"),
        plot.subtitle = element_text(family = "Roboto Condensed", color = "#00467f"),
        axis.line = element_line(color = "black", size = 0.02))

# Merging data for block groups
test <- check_clean %>%
  clean_names() %>%
  left_join(filter(white_alone_clean, str_detect(name, "Durham County"))) %>%
  filter(!(is.na(majority_minority))) %>%
  tabyl(majority_minority)

# Creating second plot for stacking
p2 <- test %>%
  ggplot(aes(majority_minority, percent)) +
  geom_col(fill = "#ee3a43") +
  geom_text(aes(y = percent - 0.07, label = paste0(percent(percent), "\n", comma(n))), color = "#ffffff", size = 7, family = "Roboto Condensed") +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0,0.04))) +
  scale_x_discrete(labels = c("IS NOT Majority-Minority", "IS Majority-Minority")) +
  labs(x = NULL, y = NULL, title = "How many citations were issued to vehicles from majority-minority\nblock groups in Durham County?") +
  theme_minimal() +
  theme(axis.text = element_text(family = "Roboto Condensed", size = 10),
        axis.text.x = element_text(face = "bold"),
        axis.title = element_text(family = "Roboto Condensed", size = 12),
        plot.title = element_text(family = "Roboto Condensed", face = "bold", size = 18, color = "#00467f"),
        plot.subtitle = element_text(family = "Roboto Condensed", color = "#00467f"),
        axis.line = element_line(color = "black", size = 0.02))

# Creating a saving stacked plot
p1 / p2 &
  theme(plot.caption = element_text(hjust = 0, family = "Roboto Condensed"), 
        plot.margin = unit(x = c(1, 1, 3.5, 1), units = "lines"))

ggsave(filename = "dur-maj-min.png", device = "png", path = here("plots/"), height = 12, width = 8, dpi = "retina")

test <- pci_clean %>%
  filter(!(is.na(estimate))) %>%
  filter(str_detect(name, "Durham")) %>%
  arrange(cats) %>%
  tabyl(cats) %>%
  mutate(upper = str_extract(cats, "[^(]*(?=,)")) %>%
  mutate(lower = str_extract(cats, "(?<=,)[^\\]]*")) %>%
  mutate(upper = str_remove(upper, "\\[")) %>%
  mutate(label = glue("{dollar(as.numeric(upper))} to {dollar(as.numeric(lower))}")) %>%
  mutate(label = str_wrap(label, width = 10))

p1 <- test %>%
  ggplot(aes(cats, percent)) +
  geom_col(fill = "#ee3a43") +
  geom_text(data = filter(test, percent > 0.1), aes(y = percent - 0.05, label = paste0(percent(percent, accuracy = 0.01), "\n", comma(n, accuracy = 1))), color = "#ffffff", size = 5, family = "Roboto Condensed") +
  geom_text(data = filter(test, percent < 0.1), aes(y = percent + 0.05, label = paste0(percent(percent, accuracy = 0.01), "\n", comma(n, accuracy = 1))), color = "#00467f", size = 5, family = "Roboto Condensed") +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0,0.04))) +
  scale_x_discrete(labels = test$label) +
  labs(x = NULL, y = NULL, title = "Citations and Block Group Per Capita Income for Durham County", 
       subtitle = "Per Capita Income block groups separated into 10 equal interval categories") +
  theme_minimal() +
  theme(axis.text = element_text(family = "Roboto Condensed", size = 10),
        axis.text.x = element_text(face = "bold"),
        axis.title = element_text(family = "Roboto Condensed", size = 12),
        plot.title = element_text(family = "Roboto Condensed", face = "bold", size = 24, color = "#00467f"),
        plot.subtitle = element_text(family = "Roboto Condensed", color = "#00467f"),
        axis.line = element_line(color = "black", size = 0.02))

test <- check_clean %>%
  clean_names() %>%
  left_join(filter(pci_clean, str_detect(name, "Durham"))) %>%
  filter(!(is.na(estimate))) %>%
  arrange(cats) %>%
  tabyl(cats) %>%
  mutate(upper = str_extract(cats, "[^(]*(?=,)")) %>%
  mutate(lower = str_extract(cats, "(?<=,)[^\\]]*")) %>%
  mutate(upper = str_remove(upper, "\\[")) %>%
  mutate(label = glue("{dollar(as.numeric(upper))} to {dollar(as.numeric(lower))}")) %>%
  mutate(label = str_wrap(label, width = 10))

p2 <- test %>%
  ggplot(aes(cats, percent)) +
  geom_col(fill = "#ee3a43") +
  geom_text(data = filter(test, percent > 0.1), aes(y = percent - 0.04, label = paste0(percent(percent, accuracy = 0.01), "\n", comma(n, accuracy = 1))), color = "#ffffff", size = 5, family = "Roboto Condensed") +
  geom_text(data = filter(test, percent < 0.1), aes(y = percent + 0.04, label = paste0(percent(percent, accuracy = 0.01), "\n", comma(n, accuracy = 1))), color = "#00467f", size = 5, family = "Roboto Condensed") +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0,0.04))) +
  scale_x_discrete(labels = test$label) +
  labs(x = NULL, y = NULL, title = "Block Group Per Capita Income for Durham County", 
       subtitle = "Per Capita Income block groups separated into 10 equal interval categories") +
  theme_minimal() +
  theme(axis.text = element_text(family = "Roboto Condensed", size = 10),
        axis.text.x = element_text(face = "bold"),
        axis.title = element_text(family = "Roboto Condensed", size = 12),
        plot.title = element_text(family = "Roboto Condensed", face = "bold", size = 24, color = "#00467f"),
        plot.subtitle = element_text(family = "Roboto Condensed", color = "#00467f"),
        axis.line = element_line(color = "black", size = 0.02))

p1 / p2 &
  theme(plot.caption = element_text(hjust = 0, family = "Roboto Condensed"), 
        plot.margin = unit(x = c(1, 1, 3.5, 1), units = "lines"))

ggsave(filename = "dur-pci.png", device = "png", path = here("plots/"), height = 12, width = 10, dpi = "retina")
