library(tidyverse)
library(readxl)
library(here)
library(janitor)
library(lubridate)
library(scales)
library(extrafont)
library(patchwork)

d_logo <- png::readPNG(source = here("logo/durham.png"), native = TRUE)
n_logo <- png::readPNG(source = here("logo/black-logo-long.png"), native = TRUE)

# Load the data
dat <- read_xlsx(path = here("data/durham-citations.xlsx"))

# Clean variable names and get dates in correct format
clean_dat <- dat %>%
  clean_names() %>%
  mutate(first_escalation_date = as.POSIXct(first_escalation_date, format = "%Y-%m-%d %H:%M:%OS")) %>%
  mutate(second_escalation_date = as.POSIXct(second_escalation_date, format = "%Y-%m-%d %H:%M:%OS")) %>%
  mutate(date_sent_to_collections = as.POSIXct(date_sent_to_collections, format = "%Y-%m-%d %H:%M:%OS")) %>%
  mutate(date_paid = as.POSIXct(date_paid, format = "%Y-%m-%d %H:%M:%OS"))

# Write the data as compressed R data object
write_rds(clean_dat, file = here("data/clean-citations.rds"))

# Citations counts by Month
clean_dat %>%
  mutate(mth_issued = floor_date(issue_date_time, unit = "month")) %>%
  group_by(mth_issued) %>%
  count() %>%
  ggplot(aes(mth_issued, n)) +
  geom_smooth(se = FALSE, method = "lm", color = "#ffd24f") +
  geom_line(color = "#ee3a43") +
  geom_point(color = "#ee3a43") +
  scale_y_continuous(labels = comma_format()) +
  scale_x_datetime(labels = label_date(format = "%b\n%Y"), expand = expansion(mult = c(0.01,0.04))) +
  labs(x = NULL, y = NULL,
       title = "City of Durham Parking Citations", subtitle = "Totals by Month") +
  theme_minimal() +
  theme(axis.text = element_text(family = "Roboto Condensed", size = 10),
        axis.title = element_text(family = "Roboto Condensed", size = 12),
        plot.title = element_text(family = "Roboto Condensed", face = "bold", size = 24, color = "#00467f"),
        plot.subtitle = element_text(family = "Roboto Condensed", color = "#00467f"),
        plot.margin = unit(x = c(1, 1, 3.5, 1), units = "lines"),
        axis.line = element_line(color = "black", size = 0.02)) +
  inset_element(p = n_logo, left = 1, bottom = 0, right = 0.5, top = 0.07, align_to = "full") +
  inset_element(p = d_logo, left = 0.85, bottom = 0, right = 1, top = 0.07, align_to = "full")
  
ggsave(filename = "monthly-verify.png", device = "png", path = here("plots/"), 
       width = 12, height = 7, dpi = "retina")

clean_dat %>%
  mutate(mth_issued = floor_date(issue_date_time, unit = "month")) %>%
  group_by(mth_issued) %>%
  summarise(total = sum(issue_amount, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(mth_issued, total)) +
  geom_smooth(se = FALSE, method = "lm", color = "#ffd24f") +
  geom_line(color = "#ee3a43") +
  geom_point(color = "#ee3a43") +
  scale_y_continuous(labels = dollar_format()) +
  scale_x_datetime(labels = label_date(format = "%b\n%Y"), expand = expansion(mult = c(0.01,0.04))) +
  labs(x = NULL, y = NULL,
       title = "City of Durham Parking Citations", subtitle = "Total Citation Amounts by Month") +
  theme_minimal() +
  theme(axis.text = element_text(family = "Roboto Condensed", size = 10),
        axis.title = element_text(family = "Roboto Condensed", size = 12),
        plot.title = element_text(family = "Roboto Condensed", face = "bold", size = 24, color = "#00467f"),
        plot.subtitle = element_text(family = "Roboto Condensed", color = "#00467f"),
        plot.margin = unit(x = c(1, 1, 3.5, 1), units = "lines"),
        axis.line = element_line(color = "black", size = 0.02)) +
  inset_element(p = n_logo, left = 1, bottom = 0, right = 0.5, top = 0.07, align_to = "full") +
  inset_element(p = d_logo, left = 0.85, bottom = 0, right = 1, top = 0.07, align_to = "full")

ggsave(filename = "monthly-amount-verify.png", device = "png", path = here("plots/"), 
       width = 12, height = 7, dpi = "retina")

clean_dat %>%
  mutate(fiscal_year = ifelse(month(issue_date_time) >= 6, year(issue_date_time) + 1, year(issue_date_time))) %>%
  group_by(fiscal_year) %>%
  summarise(total = sum(issue_amount, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(fiscal_year, total)) +
  geom_col(fill = "#ee3a43", color = "black", size = 0.02) +
  geom_text(aes(label = dollar(total), y = total - 15000), family = "Roboto Condensed", color = "#ffffff") +
  scale_y_continuous(labels = dollar_format(), expand = expansion(mult = c(0, 0.04))) +
  labs(x = NULL, y = NULL,
       title = "City of Durham Parking Citations", subtitle = "Total Citation Amounts by Fiscal Year") +
  theme_minimal() +
  theme(axis.text = element_text(family = "Roboto Condensed", size = 10),
        axis.title = element_text(family = "Roboto Condensed", size = 12),
        plot.title = element_text(family = "Roboto Condensed", face = "bold", size = 24, color = "#00467f"),
        plot.subtitle = element_text(family = "Roboto Condensed", color = "#00467f"),
        plot.margin = unit(x = c(1, 1, 3.5, 1), units = "lines"),
        axis.line = element_line(color = "black", size = 0.02)) +
  inset_element(p = n_logo, left = 1, bottom = 0, right = 0.5, top = 0.07, align_to = "full") +
  inset_element(p = d_logo, left = 0.85, bottom = 0, right = 1, top = 0.07, align_to = "full")

ggsave(filename = "fiscal-year-verify.png", device = "png", path = here("plots/"), 
       width = 12, height = 7, dpi = "retina")
