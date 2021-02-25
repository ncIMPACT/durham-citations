library(tidyverse)
library(here)
library(scales)
library(extrafont)
library(patchwork)
library(janitor)
library(gt)
library(glue)


d_logo <- png::readPNG(source = here("logo/durham.png"), native = TRUE)
n_logo <- png::readPNG(source = here("logo/black-logo-long.png"), native = TRUE)

dur_dat <- read_rds(here("data/clean-citations.rds"))

ttp <- dur_dat %>%
  filter(!(is.na(date_paid))) %>%
  mutate(ttp = as.integer(difftime(date_paid, issue_date_time, units = "days")))

# Pay immediately
ttp %>%
  filter(is.na(first_escalation_date)) %>%
  filter(is.na(second_escalation_date)) %>%
  filter(is.na(date_sent_to_collections)) %>%
  mutate(ttp = cut_interval(x = ttp, n = 10)) %>%
  tabyl(ttp) %>%
  mutate(percent = n / 43579) %>%
  mutate(upper = str_extract(ttp, "[^(]*(?=,)")) %>%
  mutate(lower = str_extract(ttp, "(?<=,)[^\\]]*")) %>%
  mutate(upper = str_remove(upper, "\\[")) %>%
  mutate(ttp = glue("{round(as.numeric(upper), digits = 0)} to {round(as.numeric(lower), digits = 0)}")) %>%
  select(-c(upper, lower)) %>%
  adorn_totals() %>%
  gt(rowname_col = "ttp") %>%
  tab_header(title = "City of Durham Parking Citations", subtitle = "Frequencies of days-to-pay, no escalation and not sent to collections") %>%
  tab_stubhead(label = "Days-To-Pay") %>%
  fmt_number(columns = "n", drop_trailing_zeros = TRUE) %>%
  fmt_percent(columns = "percent") %>%
  cols_align(align = "center") %>%
  tab_style(style = list(
    cell_borders(sides = "right", color = "#ffd24f", weight = px(3))),
    locations = list(
      cells_stub())) %>%
  tab_style(style = list(
    cell_borders(sides = "top", weight = px(3), color = "#ee3a43"),
    cell_borders(sides = "bottom", color = "#ee3a43", weight = px(3)),
    cell_fill(color = "#ee3a43"),
    cell_text(color = "#ffffff")),
    locations = list(
      cells_body(rows = "Total"),
      cells_stub(rows = "Total"))) %>%
  tab_style(style = list(
    cell_text(align = "right", weight = "bold")),
            locations = list(
              cells_stub(rows = "Total"))) %>%
  tab_style(style = list(
    cell_text(font = "Roboto Condensed", weight = "bold", color = "#ffffff"),
    cell_fill(color = "#00467f"),
    cell_borders(sides = "all", color = "#00467f", weight = px(3))),
    locations = list(
      cells_title(groups = "title"),
      cells_title(groups = "subtitle"))) %>%
  tab_style(style = list(
    cell_borders(sides = "bottom", weight = px(3), color = "#ffd24f"),
    cell_text(align = "center", weight = "bold")),
    locations = list(
      cells_column_labels(everything()),
      cells_stubhead())) %>%
  cols_label(.list = list("ttp" = "Days-to-Pay", "n" = "Count", "percent" = "Percent")) %>%
  tab_options(table.width = pct(80), table.font.names = c("Roboto Condensed"))
  
# Pay after first escalation
ttp %>%
  filter(!(is.na(first_escalation_date))) %>%
  filter(is.na(second_escalation_date)) %>%
  filter(is.na(date_sent_to_collections)) %>%
  mutate(ttp = cut_interval(x = ttp, n = 10)) %>%
  tabyl(ttp) %>%
  mutate(percent = n / 45690) %>%
  mutate(upper = str_extract(ttp, "[^(]*(?=,)")) %>%
  mutate(lower = str_extract(ttp, "(?<=,)[^\\]]*")) %>%
  mutate(upper = str_remove(upper, "\\[")) %>%
  mutate(ttp = glue("{round(as.numeric(upper), digits = 0)} to {round(as.numeric(lower), digits = 0)}")) %>%
  select(-c(upper, lower)) %>%
  adorn_totals() %>%
  gt(rowname_col = "ttp") %>%
  tab_header(title = "City of Durham Parking Citations", subtitle = "Frequencies of days-to-pay with only first escalation") %>%
  tab_stubhead(label = "Days-To-Pay") %>%
  fmt_number(columns = "n", drop_trailing_zeros = TRUE) %>%
  fmt_percent(columns = vars(percent)) %>%
  cols_align(align = "center") %>%
  tab_style(style = list(
    cell_borders(sides = "right", color = "#ffd24f", weight = px(3))),
    locations = list(
      cells_stub())) %>%
  tab_style(style = list(
    cell_borders(sides = "top", weight = px(3), color = "#ee3a43"),
    cell_borders(sides = "bottom", color = "#ee3a43", weight = px(3)),
    cell_fill(color = "#ee3a43"),
    cell_text(color = "#ffffff")),
    locations = list(
      cells_body(rows = "Total"),
      cells_stub(rows = "Total"))) %>%
  tab_style(style = list(
    cell_text(align = "right", weight = "bold")),
    locations = list(
      cells_stub(rows = "Total"))) %>%
  tab_style(style = list(
    cell_text(font = "Roboto Condensed", weight = "bold", color = "#ffffff"),
    cell_fill(color = "#00467f"),
    cell_borders(sides = "all", color = "#00467f", weight = px(3))),
    locations = list(
      cells_title(groups = "title"),
      cells_title(groups = "subtitle"))) %>%
  tab_style(style = list(
    cell_borders(sides = "bottom", weight = px(3), color = "#ffd24f"),
    cell_text(align = "center", weight = "bold")),
    locations = list(
      cells_column_labels(everything()),
      cells_stubhead())) %>%
  cols_label(.list = list("ttp" = "Days-to-Pay", "n" = "Count", "percent" = "Percent")) %>%
  tab_options(table.width = pct(80), table.font.names = c("Roboto Condensed"))

# Pay after second escalation
ttp %>%
  filter(!(is.na(first_escalation_date))) %>%
  filter(!(is.na(second_escalation_date))) %>%
  filter(is.na(date_sent_to_collections)) %>%
  mutate(ttp = cut_interval(x = ttp, n = 10)) %>%
  tabyl(ttp) %>%
  mutate(percent = n / 45690) %>%
  mutate(upper = str_extract(ttp, "[^(]*(?=,)")) %>%
  mutate(lower = str_extract(ttp, "(?<=,)[^\\]]*")) %>%
  mutate(upper = str_remove(upper, "\\[")) %>%
  mutate(ttp = glue("{round(as.numeric(upper), digits = 0)} to {round(as.numeric(lower), digits = 0)}")) %>%
  select(-c(upper, lower)) %>%
  adorn_totals() %>%
  gt(rowname_col = "ttp") %>%
  tab_header(title = "City of Durham Parking Citations", subtitle = "Frequencies of days-to-pay with second escalation") %>%
  tab_stubhead(label = "Days-To-Pay") %>%
  fmt_number(columns = "n", drop_trailing_zeros = TRUE) %>%
  fmt_percent(columns = vars(percent)) %>%
  cols_align(align = "center") %>%
  tab_style(style = list(
    cell_borders(sides = "right", color = "#ffd24f", weight = px(3))),
    locations = list(
      cells_stub())) %>%
  tab_style(style = list(
    cell_borders(sides = "top", weight = px(3), color = "#ee3a43"),
    cell_borders(sides = "bottom", color = "#ee3a43", weight = px(3)),
    cell_fill(color = "#ee3a43"),
    cell_text(color = "#ffffff")),
    locations = list(
      cells_body(rows = "Total"),
      cells_stub(rows = "Total"))) %>%
  tab_style(style = list(
    cell_text(align = "right", weight = "bold")),
    locations = list(
      cells_stub(rows = "Total"))) %>%
  tab_style(style = list(
    cell_text(font = "Roboto Condensed", weight = "bold", color = "#ffffff"),
    cell_fill(color = "#00467f"),
    cell_borders(sides = "all", color = "#00467f", weight = px(3))),
    locations = list(
      cells_title(groups = "title"),
      cells_title(groups = "subtitle"))) %>%
  tab_style(style = list(
    cell_borders(sides = "bottom", weight = px(3), color = "#ffd24f"),
    cell_text(align = "center", weight = "bold")),
    locations = list(
      cells_column_labels(everything()),
      cells_stubhead())) %>%
  cols_label(.list = list("ttp" = "Days-to-Pay", "n" = "Count", "percent" = "Percent")) %>%
  tab_options(table.width = pct(80), table.font.names = c("Roboto Condensed"))

# Sent to colle collections
ttp %>%
  filter(!(is.na(first_escalation_date))) %>%
  filter(!(is.na(second_escalation_date))) %>%
  filter(!(is.na(date_sent_to_collections))) %>%
  mutate(ttp = cut_interval(x = ttp, n = 10)) %>%
  tabyl(ttp) %>%
  mutate(percent = n / 45690) %>%
  mutate(upper = str_extract(ttp, "[^(]*(?=,)")) %>%
  mutate(lower = str_extract(ttp, "(?<=,)[^\\]]*")) %>%
  mutate(upper = str_remove(upper, "\\[")) %>%
  mutate(ttp = glue("{round(as.numeric(upper), digits = 0)} to {round(as.numeric(lower), digits = 0)}")) %>%
  select(-c(upper, lower)) %>%
  adorn_totals() %>%
  gt(rowname_col = "ttp") %>%
  tab_header(title = "City of Durham Parking Citations", subtitle = "Frequencies of days-to-pay, sent to collections") %>%
  tab_stubhead(label = "Days-To-Pay") %>%
  fmt_number(columns = "n", drop_trailing_zeros = TRUE) %>%
  fmt_percent(columns = vars(percent)) %>%
  cols_align(align = "center") %>%
  tab_style(style = list(
    cell_borders(sides = "right", color = "#ffd24f", weight = px(3))),
    locations = list(
      cells_stub())) %>%
  tab_style(style = list(
    cell_borders(sides = "top", weight = px(3), color = "#ee3a43"),
    cell_borders(sides = "bottom", color = "#ee3a43", weight = px(3)),
    cell_fill(color = "#ee3a43"),
    cell_text(color = "#ffffff")),
    locations = list(
      cells_body(rows = "Total"),
      cells_stub(rows = "Total"))) %>%
  tab_style(style = list(
    cell_text(align = "right", weight = "bold")),
    locations = list(
      cells_stub(rows = "Total"))) %>%
  tab_style(style = list(
    cell_text(font = "Roboto Condensed", weight = "bold", color = "#ffffff"),
    cell_fill(color = "#00467f"),
    cell_borders(sides = "all", color = "#00467f", weight = px(3))),
    locations = list(
      cells_title(groups = "title"),
      cells_title(groups = "subtitle"))) %>%
  tab_style(style = list(
    cell_borders(sides = "bottom", weight = px(3), color = "#ffd24f"),
    cell_text(align = "center", weight = "bold")),
    locations = list(
      cells_column_labels(everything()),
      cells_stubhead())) %>%
  cols_label(.list = list("ttp" = "Days-to-Pay", "n" = "Count", "percent" = "Percent")) %>%
  tab_options(table.width = pct(80), table.font.names = c("Roboto Condensed"))
