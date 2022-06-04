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



































# Yıllar itibariyle gruplara göre çizgi grafik ----

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



