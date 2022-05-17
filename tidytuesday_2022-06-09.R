# Kütühaneyi yükleyip verisetini çağıralım. ----

library(tidyverse)
theme_set(theme_light())
tuesdata <- tidytuesdayR::tt_load('2020-06-09')
science <- tuesdata$science

# Listedeki 2 verisetine göz gezdirelim ----

tuesdata$firsts %>%
  View()

tuesdata$science %>%
  View()

# Firsts verisetini kaydet ----

firsts <- tuesdata$firsts

# Hangi yıl için daha fazla bilgi var? ----

tuesdata$firsts %>%
  ggplot(aes(year)) +
  geom_histogram()

# Soru1: "Science" verisetinde en fazla hangi yıl var?

science %>% ggplot(aes(birth)) + geom_histogram()

# Soru2: "Science" verisetinde en fazla hangi beş alanda icat vardır?

a <- science %>% count(occupation_s, sort = TRUE) %>%
  arrange(desc(n)) %>%
  head(5)

ggplot(a, aes(occupation_s, n)) +
  geom_col()

# First verisetindeki category'de bulunan unsurları say. ----

# Category'deki her bir unsurdan kaç tane bulunmaktadır? ----

tuesdata$firsts %>%
  count(category, sort = TRUE) %>%
  mutate(category = fct_reorder(category, n)) %>%
  ggplot(aes(n, category)) +
  geom_col()





# 

# str_remove() bir karakter dizisindeki eşleşen desenleri ortadan kaldırır.
# str_trim() karakter dizisindeki boşlukları kaldırır.

firsts_a <- tuesdata$firsts %>%
mutate(person = str_remove(person, "[\\[\\(].*"),
       person = str_trim(person))

# plotly ve glue kütüphanelerini yüklemek ----

library(plotly)
library(glue)

# First verisetindeki gelişmeleri bir zaman çizelgesine aktarmak. ----

g <- firsts %>%
  ggplot(aes(year,
             category,
             color = category,
             text = glue("{ year }: { accomplishment }\n{ person }"))) +
  geom_point() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none") +
  labs(title = "Timeline of some notable African-American achievements",
       subtitle = "Source: https://en.wikipedia.org/wiki/List_of_African-American_firsts",
       y = "Category",
       x = "Year")
ggplotly(g, tooltip = "text")

# Soru1: Hangi yılda doğan mucit neyi başardı (Zaman çizelgesinde gösterme)

p <- ggplot(science, aes(birth, name, 
                         color = occupation_s,
                         text = glue("{ name }: { inventions_accomplishments  } \n { death }"))) +
  geom_point() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none") +
  labs(title = "Mucitlerin Doğumlarını Gösteren Zaman Çizelgesi",
       x = "Yıl",
       y = "Çalıştıkları Alan")

ggplotly(p, tooltip = "text")

# Science ----


tuesdata$science %>%
  ggplot(aes(birth)) +
  geom_histogram()

tuesdata$science %>%
  separate_rows(occupation_s, sep = "; ") %>% # satırları birbirinden ayırmakta
  mutate(occupation = str_to_title(occupation_s)) %>%
  count(occupation, sort = TRUE)

science %>%
  filter(str_detect(occupation_s, regex("istician", ignore_case = TRUE))) %>%
  pull(name)

science %>%
  filter(str_detect(occupation_s, "statistician")) %>%
  View()

# Soru1: first verisetinden accomplishment değişkeninden "champion" kelimesini
# içeren başarılar nelerdir?

oscar <- firsts %>% 
  filter(str_detect(accomplishment, regex("hampion", .ignore_case = TRUE))) %>% 
  view()

# rvest ile html ----

library(rvest)
science_html <- science %>%
  mutate(html = map(links, possibly(read_html, NULL, quiet = FALSE)))

extract_infobox <- . %>%
  html_node(".vcard") %>%
  html_table(header = FALSE) %>%
  as_tibble()

infoboxes <- science_html %>%
  filter(!map_lgl(html, is.null)) %>%
  mutate(infobox = map(html, possibly(extract_infobox, NULL))) %>%
  select(link = links, infobox) %>%
  unnest(infobox) %>%
  filter(X1 != "" | X2 != "", X1 != "Scientific career") %>%
  rename(key = X1, value = X2)

science_infoboxes <- infoboxes %>%
  group_by(link) %>%
  mutate(name = first(key)) %>%
  group_by(key) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  distinct(name, key, .keep_all = TRUE) %>%
  spread(key, value) %>%
  janitor::clean_names()

science_infoboxes %>%
  count(nationality, sort = TRUE)


