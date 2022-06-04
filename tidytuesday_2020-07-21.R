# Kütüphaneleri yükleyelim ----

library(tidyverse)
library(tidytuesdayR)
library(scales)
theme_set(theme_light())

# Haftalık veriyi yükleyelim ----

tt <- tt_load("2020-07-21")

animal_outcomes <- tt$animal_outcomes
animal_complaints <- tt$animal_complaints
brisbane_complaints <- tt$brisbane_complaints

# view(animal_outcomes)
# view(animal_complaints)
# view(brisbane_complaints)

# Verisetinin özelliklerine bakalım ----

tt

# Değişkenlerde düzenleme yapalım ---- 

animal_outcomes <- tt$animal_outcomes %>%
  rename(total = Total) %>%
  mutate(outcome = fct_recode(outcome, "Currently In Care" = "In Stock"))

str(animal_outcomes)

??fct_recode() 

# Her seviyenin değerini yeniden kodlamanıza veya değiştirmenize izin verir.

# Soru 1: animal_complaints verisetinde animal type --> dog yerine "kopek" yazalım.

animal_complaints <- animal_complaints %>% clean_names()

animal_complaints <- animal_complaints %>% 
  mutate(animal_type = fct_recode(animal_type, "kopek" = "dog"))

# Soru 2: "Alice River" kategorisini "Alice Nehri" yapalım.

animal_complaints <- animal_complaints %>% 
  mutate(suburb = fct_recode(suburb, "Alice Nehri" = "Alice River"))

# Soru 3: "cat" kategorisini "kedi" olarak değiştirelim.

animal_complaints <- animal_complaints %>% 
  mutate(animal_type = fct_recode(animal_type, "kedi" = "cat"))

# Yıllar itibariyle gruplara göre çizgi grafik ----

# Çeşitli kategorilerde yıllar itibariyle outcome değişimi.
animal_outcomes %>%
  count(outcome, year, sort = TRUE, wt = total) %>%
  mutate(outcome = fct_reorder(outcome, n, sum)) %>%
  ggplot(aes(year, n, color = outcome)) +
  geom_line() +
  # geom_col() +
  labs(x = "Year",
       y = "# of animals with this outcome")

# Tarih formatı nasıl değiştirilir? ----

# Tarih formatına çevirmek
linelist <- linelist %>% 
  mutate(date_onset = as.Date(date_of_onset, format = "%d/%m/%Y"))

# %d = Day number of month (5, 17, 28, etc.)
# %j = Day number of the year (Julian day 001-366)
# %a = Abbreviated weekday (Mon, Tue, Wed, etc.)
# %A = Full weekday (Monday, Tuesday, etc.) %w = Weekday number (0-6, Sunday is 0)
# %u = Weekday number (1-7, Monday is 1)
# %W = Week number (00-53, Monday is week start)
# %U = Week number (01-53, Sunday is week start)
# %m = Month number (e.g. 01, 02, 03, 04)
# %b = Abbreviated month (Jan, Feb, etc.)
# %B = Full month (January, February, etc.)
# %y = 2-digit year (e.g. 89)
# %Y = 4-digit year (e.g. 1989)
# %h = hours (24-hr clock)
# %m = minutes
# %s = seconds %z = offset from GMT
# %Z = Time zone (character)

# Çalışmadı.
animal_complaints <- animal_complaints %>% 
  mutate(date = as.Date(date_received, format = "%B %Y"))

animal_complaints <- animal_complaints %>% 
  mutate(date_received = lubridate::my(date_received))

# Soru:1 Yıllar itibariyle çeşitli düzeylerde şikayet sayılarını çizdirelim.

animal_complaints %>%
  group_by(date_received) %>% 
  count(complaint_type) %>% 
  ggplot(aes(date_received, n, color = complaint_type)) +
  geom_line()

# Yıllar itibariyle facet_wrap() grafiği ---- 

animal_outcomes %>%
  complete(animal_type, outcome, year, fill = list(total = 0)) %>%
  mutate(outcome = fct_reorder(outcome, total, sum),
         animal_type = fct_reorder(animal_type, -total, sum)) %>%
  ggplot(aes(year, total, color = outcome)) +
  geom_line() +
  facet_wrap(~ animal_type, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  labs(x = "Year",
       y = "# of animals with this outcome",
       color = "Outcome")

# Soru 1: Kedi ve köpeklerin yıllar itibariyle hangi konularda şikayet edildiği

animal_complaints %>% 
  group_by(animal_type, date_received) %>% 
  count(complaint_type) %>% 
  # complete(animal_type, complaint_type, date_received, fill = list(n = 0))
  # mutate(complaint_type = fct_reorder(complaint_type, n),
  #        animal_type = fct_reorder(complaint_type, -n)) %>% 
  ggplot(aes(date_received, n, color = complaint_type)) +
  geom_line() +
  facet_wrap(~ animal_type, scales = "free_y") +
  scale_y_continuous(labels = comma)
  


# Gruplanmış facet_wrap() sütun grafikler ----

animal_outcomes %>%
  filter(year == max(year)) %>%
  mutate(outcome = fct_reorder(outcome, total, sum)) %>%
  ggplot(aes(total, outcome)) +
  geom_col() +
  facet_wrap(~ animal_type, scales = "free_x") +
  labs(x = "Total # of animals in 2018",
       y = "Outcome",
       title = "What are the outcomes for different animal types?",
       subtitle = "In Australia in 2018")

# https://stackoverflow.com/questions/33221425/how-do-i-group-my-date-variable-into-month-year-in-r
# df %>%
#   mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
#   group_by(month, year) %>%
#   summarise(total = sum(value))

# Soru 1: Yıl değişkeninin date_received sütunundan alalım.

animal_complaints <- animal_complaints %>% 
  mutate(year = format(date_received, "%Y"))

# 2020 yılında kedi ve köpekler için hangi şikayet tipinde şikayetler oldu?

animal_complaints %>% 
  filter(year == max(year)) %>% 
  count(animal_type, complaint_type) %>% 
  mutate(complaint_type = fct_reorder(complaint_type, n)) %>% 
  ggplot(aes(n, complaint_type)) +
  geom_col() +
  facet_wrap(~ animal_type, scales = "free_x")
  
### Provinces ----

library(rvest)

states_territories <- read_html("https://en.wikipedia.org/wiki/States_and_territories_of_Australia") %>%
  html_nodes(".sortable") %>%
  map(html_table, fill = TRUE) %>%
  head(2) %>%
  map(janitor::clean_names) %>%
  map(as_tibble)

states <- states_territories[[1]] %>%
  select(region = state,
         code = postal,
         population = contains("population"),
         area = contains("area"))

territories <- states_territories[[2]] %>%
  filter(territory != "Jervis Bay Territory") %>%
  select(region = territory,
         code = postal,
         population = contains("population"),
         area = contains("area"))

state_territory_data <- bind_rows(states, territories) %>%
  mutate(code = str_to_upper(code),
         population = readr::parse_number(population),
         area = readr::parse_number(area))

# ----

animal_outcomes_tidy <- animal_outcomes %>%
  pivot_longer(ACT:WA, names_to = "code", values_to = "n") %>%
  inner_join(state_territory_data, by = "code")

animal_outcomes_tidy %>%
  filter(year == 2018, animal_type %in% c("Dogs", "Cats")) %>%
  mutate(per_capita_million = n / population * 1000000,
         region = fct_reorder(region, -n, sum),
         outcome = fct_reorder(outcome, n, sum)) %>%
  ggplot(aes(n, outcome, fill = animal_type)) +
  geom_col(position = "dodge") +
  facet_wrap(~ region, scales = "free_x") +
  labs(title = "Comparing cat and dog outcomes by region in 2018",
       x = "# of animals",
       fill = "")
# ----

by_year_region <- animal_outcomes_tidy %>%
  filter(animal_type %in% c("Dogs", "Cats")) %>%
  filter(!is.na(n)) %>%
  group_by(year, animal_type, region, code) %>%
  summarize(n_animals = sum(n),
            pct_euthanized = sum(n[outcome == "Euthanized"]) / sum(n)) %>%
  ungroup()

by_year_region %>%
  mutate(region = fct_reorder(region, -n_animals, sum)) %>%
  ggplot(aes(year, pct_euthanized, color = animal_type)) +
  geom_line() +
  geom_point(aes(size = n_animals)) +
  facet_wrap(~ region) +
  scale_y_continuous(labels = percent) +
  labs(x = "Year",
       y = "% euthanized",
       size = "# of animals")

# ----

library(sf)
library(gganimate)

australia_states <- sf::read_sf("~/Downloads/Australian States Shapefile/")

simplified <- australia_states %>%
  st_simplify(dTolerance = .02)

simplified %>%
  inner_join(by_year_region, by = c(ISO_SUB = "code")) %>%
  filter(animal_type == "Cats", year == 2018) %>%
  ggplot(aes(fill = pct_euthanized)) +
  geom_sf() +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = .5) +
  ggthemes::theme_map() +
  labs(x = "% cats euthanized in 2018")

simplified %>%
  inner_join(by_year_region, by = c(ISO_SUB = "code")) %>%
  filter(animal_type == "Cats") %>%
  mutate(year = as.integer(year)) %>%
  ggplot(aes(fill = pct_euthanized)) +
  geom_sf() +
  transition_manual(year) +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = .5,
                       labels = percent) +
  ggthemes::theme_map() +
  labs(fill = "% cats euthanized",
       title = "% of cats euthanized in each province in { current_frame }")
