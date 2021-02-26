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

# Load data
dat <- read_rds(here("data/geocoded.rds"))

# Grab block groups
nc_bg <- block_groups(state = 37)
dur_bg <- block_groups(state = 37, county = 63)

# Block group overlay
check <- dat %>%
  filter(!(is.na(long))) %>%
  st_as_sf(coords = c("long", "lat"), crs = st_crs(nc_bg)) %>%
  st_intersection(nc_bg) %>%
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

check_dur_clean <- check_dur %>%
  as_tibble() %>%
  select(-geometry)

check_clean %>%
  write_csv(here("data/bg-overlay.csv"))

check_dur_clean %>%
  write_csv(here("data/dur-bg-overlay.csv"))

# Load census variables
vars <- load_variables(year = 2019, dataset = "acs5")

# Fetching white alone data from Census
race <- get_acs(geography = "block group", table = "B02001",
                state = 37, survey = "acs5", key = api_key, summary_var = "B02001_001")

race %>%
  clean_names() %>%
  left_join(vars, by = c("variable" = "name")) %>%
  mutate(pct = estimate / summary_est) %>%
  mutate(label = str_remove(label, "Estimate")) %>%
  mutate(label = str_replace_all(label, ":", " ")) %>%
  mutate(label = str_replace_all(label, "!!", " ")) %>%
  mutate(label = str_replace_all(label, "  ", " ")) %>%
  mutate(label = str_trim(label)) %>%
  filter(!(str_sub(variable, -3, -1) %in% c("007", "008", "009", "010"))) %>%
  mutate(label = ifelse(str_ends(variable, "001"), "Total Population", str_remove(label, "Total "))) %>%
  select(geoid, label, summary_est, pct) %>%
  pivot_wider(names_from = label, values_from = pct) %>%
  clean_names() %>%
  select(1:2, 4:8) %>%
  rename(total_pop = summary_est) -> race_clean

# Fetching per capita income
pci <- get_acs(geography = "block group", variables = "B19301_001",
               state = 37, survey = "acs5", key = api_key)

pci %>%
  clean_names() %>%
  mutate(label = "per_capita_income") %>%
  select(geoid, estimate) %>%
  rename(per_capita_income = estimate) -> pci_clean

# Fetching age data
age <- get_acs(geography = "block group", table = "B01001",
               state = 37, survey = "acs5", summary_var = "B01001_001", key = api_key)

age %>%
  clean_names() %>%
  left_join(vars, by = c("variable" = "name")) %>%
  mutate(pct = estimate / summary_est) %>%
  mutate(label = str_remove(label, "Estimate")) %>%
  mutate(label = str_replace_all(label, ":", " ")) %>%
  mutate(label = str_replace_all(label, "!!", " ")) %>%
  mutate(label = str_replace_all(label, "  ", " ")) %>%
  mutate(label = str_trim(label)) %>% 
  mutate(check = str_sub(variable, 8, 10)) %>%
  mutate(check = as.integer(check)) %>%
  mutate(age_group = case_when(
    check %in% c(3:6, 27:30) ~ "0 to 17",
    check %in% c(7:12, 31:36) ~ "18 to 34",
    check %in% c(13:16, 37:40) ~ "35 to 54",
    check %in% c(17:22, 41:46) ~ "55 to 74",
    check %in% c(23:25, 47:49) ~ "75_plus",
    check == 1 ~ "Total",
    check == 2 ~ "Total Male",
    check == 26 ~ "Total Female"
  )) %>%
  filter(age_group != "Total") %>%
  mutate(check = str_detect(label, "Male")) %>%
  mutate(gender = ifelse(check, "Male", "Female")) %>%
  group_by(geoid, age_group) %>%
  summarise(estimate = sum(estimate)) %>%
  ungroup() %>%
  left_join(select(clean_names(distinct(age, GEOID, .keep_all = TRUE)), geoid, summary_est)) %>%
  mutate(pct = estimate / summary_est) %>%
  select(geoid, age_group, pct) %>%
  pivot_wider(names_from = age_group, values_from = pct) %>%
  clean_names() -> age_clean


race_clean %>%
  left_join(age_clean) %>%
  left_join(pci_clean) %>%
  right_join(check_clean) -> all_dat_wide

race_clean %>%
  left_join(age_clean) %>%
  left_join(pci_clean) %>%
  right_join(check_dur_clean) -> dur_dat_wide

write_csv(all_dat_wide, here("data/citation-demographics.csv"))
write_csv(dur_dat_wide, here("data/dur-citation-demographis.csv"))



