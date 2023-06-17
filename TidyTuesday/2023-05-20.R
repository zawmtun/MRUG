# Title: TidyTuesday exercise (2023 May 16)
# Author: ZMT
# Date: 20 May 2023

library(tidyverse)
library(tidytuesdayR)
library(maps)
library(sf)

# Read dataset
tt_datasets(2023)
tt <- tt_load("2023-05-16")

us_state <- data.frame(
  state_code = state.abb,
  state_name = state.name
)

tornados <- tt[[1]] |> 
  left_join(us_state, by = c("st" = "state_code"))

# Plot overall number of tornados over time

tor_overall <- tornados |> 
  count(yr)

ggplot(tor_overall, aes(x = yr, y = n)) +
  geom_line() +
  geom_smooth(method = "lm")

# Plot tornados with at least EF scale 3

tornados |> 
  count(mag)

tornados |> 
  filter(mag %in% c(3, 4, 5)) |> 
  count(yr) |> 
  ggplot(aes(x = yr, y = n)) +
  geom_line() +
  geom_smooth(method = "lm")

# Compare top states with highest number of tornados in 1960 and 2020

tornados |> 
  filter(yr == 1960) |> 
  count(state_name, sort = TRUE) |> 
  slice(1:10)

tornados |> 
  filter(yr == 2020) |> 
  count(state_name, sort = TRUE) |> 
  slice(1:10)

tornados |> 
  filter(state_name == "Texas") |> 
  count(yr) |> 
  ggplot(aes(x = yr, y = n)) +
  geom_line() +
  geom_smooth(method = "lm")

# Plot 2022 tornados in the US

usa <- map("state", fill = TRUE, plot = FALSE) |> 
  st_as_sf()

class(usa)

tornados_2022 <- tornados |> 
  filter(yr == 2022) |> 
  drop_na(state_name) |> 
  count(state_name)

tornados_2022_map <- tornados_2022 |> 
  mutate(state_name_1 = tolower(state_name)) |> 
  right_join(usa, by = c("state_name_1" = "ID")) |> 
  mutate(
    state_name = if_else(is.na(state_name),
                         str_to_title(state_name_1),
                         state_name),
    state_name = str_replace(state_name, "Of", "of"),
    n = replace_na(n, 0)
  ) |> 
  select(-state_name_1) |> 
  st_as_sf()
  
ggplot(tornados_2020_map) +
  geom_sf(aes(fill = n)) +
  labs(title = "Tornados in the US in 2022",
       fill = NULL) +
  scale_fill_gradient(low = "white", high = "#066599") +
  theme_void()

# Create an animation from 1950 to 2022

tornados_all_map <- tornados |> 
  count(state_name, yr) |> 
  mutate(state_name_1 = tolower(state_name)) |> 
  right_join(usa, by = c("state_name_1" = "ID")) |> 
  mutate(
    state_name = if_else(is.na(state_name),
                         str_to_title(state_name_1),
                         state_name),
    state_name = str_replace(state_name, "Of", "of"),
    n = replace_na(n, 0)
  ) |> 
  select(-state_name_1) |> 
  st_as_sf()

tornados_all_map |> 
  filter(yr %in% seq(1962, 2022, 10)) |> 
  ggplot() +
  geom_sf(aes(fill = n)) +
  labs(title = "Tornados in the US",
       fill = NULL) +
  facet_wrap(vars(yr)) +
  scale_fill_gradient(low = "white", high = "#066599") +
  theme_void()

