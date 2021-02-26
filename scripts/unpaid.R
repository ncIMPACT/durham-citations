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

# Census API Key
source(here("scripts/api-key.R"))

dur_bg <- block_groups(state = 37, county = 63)
dur_bg <- dur_bg %>%
  as_tibble() %>%
  select(-geometry) %>%
  clean_names()

dat <- read_csv(here("data/bg-overlay.csv"), col_types = cols(zip_code = col_character(),
                                                              geoid = col_character()))

# Fetching white alone data from Census
white_alone <- get_acs(geography = "block group", variables = "B02001_002",
                       state = 37, survey = "acs5", key = api_key, summary_var = "B02001_001")

# Cleaning census data
white_alone %>%
  clean_names() %>%
  filter(summary_est != 0) %>%
  mutate(pct = estimate / summary_est) %>%
  mutate(majority_minority = ifelse(pct < .50, TRUE, FALSE)) %>%
  select(geoid, majority_minority) -> white_alone_clean

# Fetching per capita income
pci <- get_acs(geography = "block group", variables = "B19301_001",
               state = 37, survey = "acs5", key = api_key)

# Cleaning census data
pci %>%
  clean_names() %>%
  filter(estimate != 0) %>%
  mutate(cats = cut_interval(estimate, n = 10)) %>%
  mutate(upper = str_extract(cats, "[^(]*(?=,)")) %>%
  mutate(lower = str_extract(cats, "(?<=,)[^\\]]*")) %>%
  mutate(upper = str_remove(upper, "\\[")) %>%
  mutate(label = glue("{dollar(as.numeric(upper))} to {dollar(as.numeric(lower))}")) %>%
  mutate(label = str_wrap(label, width = 10)) %>%
  select(geoid, cats, label) %>%
  rename(pci_cats = cats, pci_label = label, ) -> pci_clean

# Join the census data
dat %>%
  filter(is.na(date_paid)) %>%
  left_join(pci_clean) %>%
  left_join(white_alone_clean) -> dat_merged

# Plot maj-min
p1 <- dur_bg %>%
  left_join(select(distinct(white_alone_clean, geoid, .keep_all = TRUE), geoid, majority_minority)) %>%
  filter(!(is.na(majority_minority))) %>%
  tabyl(majority_minority) %>%
  ggplot(aes(majority_minority, percent)) +
  geom_col(fill = "#ee3a43") +
  geom_text(aes(y = percent - 0.07, label = paste0(percent(percent), "\n", comma(n))), color = "#ffffff", size = 7, family = "Roboto Condensed") +
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

p2 <- dat_merged %>%
  filter(geoid %in% dur_bg$geoid) %>%
  tabyl(majority_minority) %>%
  ggplot(aes(majority_minority, percent)) +
  geom_col(fill = "#ee3a43") +
  geom_text(aes(y = percent - 0.07, label = paste0(percent(percent), "\n", comma(n))), color = "#ffffff", size = 7, family = "Roboto Condensed") +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0,0.04))) +
  scale_x_discrete(labels = c("IS NOT Majority-Minority", "IS Majority-Minority")) +
  labs(x = NULL, y = NULL, title = "How many unpaid citations were issued to vehicles from\nmajority-minority block groups in Durham County?") +
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

ggsave(filename = "dur-maj-min-unpaid.png", device = "png", path = here("plots/"), height = 12, width = 8, dpi = "retina")

# Plot pci
dur_bg %>%
  left_join(distinct(pci_clean, geoid, .keep_all = TRUE)) %>%
  filter(!(is.na(pci_label))) %>%
  tabyl(pci_cats) %>%
  mutate(upper = str_extract(pci_cats, "[^(]*(?=,)")) %>%
  mutate(lower = str_extract(pci_cats, "(?<=,)[^\\]]*")) %>%
  mutate(upper = str_remove(upper, "\\[")) %>%
  mutate(label = glue("{dollar(as.numeric(upper))} to {dollar(as.numeric(lower))}")) %>%
  mutate(label = str_wrap(label, width = 10)) -> test

p1 <- test %>%
  ggplot(aes(pci_cats, percent)) +
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

dat_merged %>%
  filter(geoid %in% dur_bg$geoid) %>%
  tabyl(pci_cats) %>%
  mutate(upper = str_extract(pci_cats, "[^(]*(?=,)")) %>%
  mutate(lower = str_extract(pci_cats, "(?<=,)[^\\]]*")) %>%
  mutate(upper = str_remove(upper, "\\[")) %>%
  mutate(label = glue("{dollar(as.numeric(upper))} to {dollar(as.numeric(lower))}")) %>%
  mutate(label = str_wrap(label, width = 10)) -> test

p2 <- test %>%
  ggplot(aes(pci_cats, percent)) +
  geom_col(fill = "#ee3a43") +
  geom_text(data = filter(test, percent > 0.1), aes(y = percent - 0.05, label = paste0(percent(percent, accuracy = 0.01), "\n", comma(n, accuracy = 1))), color = "#ffffff", size = 5, family = "Roboto Condensed") +
  geom_text(data = filter(test, percent < 0.1), aes(y = percent + 0.05, label = paste0(percent(percent, accuracy = 0.01), "\n", comma(n, accuracy = 1))), color = "#00467f", size = 5, family = "Roboto Condensed") +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0,0.04))) +
  scale_x_discrete(labels = test$label) +
  labs(x = NULL, y = NULL, title = "Unpaid Citations and Block Group Per Capita Income\nfor Durham County", 
       subtitle = "Per Capita Income block groups separated into 10 equal interval categories") +
  theme_minimal() +
  theme(axis.text = element_text(family = "Roboto Condensed", size = 10),
        axis.text.x = element_text(face = "bold"),
        axis.title = element_text(family = "Roboto Condensed", size = 12),
        plot.title = element_text(family = "Roboto Condensed", face = "bold", size = 24, color = "#00467f"),
        plot.subtitle = element_text(family = "Roboto Condensed", color = "#00467f"),
        axis.line = element_line(color = "black", size = 0.02))

# Creating a saving stacked plot
p1 / p2 &
  theme(plot.caption = element_text(hjust = 0, family = "Roboto Condensed"), 
        plot.margin = unit(x = c(1, 1, 3.5, 1), units = "lines"))

ggsave(filename = "dur-pci-unpaid.png", device = "png", path = here("plots/"), height = 12, width = 10, dpi = "retina")
