# Kütüphaneleri yüklemek ----

library(tidyverse)
library(tidytuesdayR)
library(scales)
theme_set(theme_light())
tuesdata <- tt_load('2020-06-16')

# Tanımlayıcı grafikler ----

slave_routes <- tuesdata$slave_routes

slave_routes %>%
  summarize(sum(n_slaves_arrived, na.rm = TRUE))

slave_routes %>%
  count(ship_name, sort = TRUE)

slave_routes %>%
  ggplot(aes(n_slaves_arrived)) +
  geom_histogram()

# african_names verisetini yükleyelim.----

african_names <- tuesdata$african_names

str(african_names)
glimpse(african_names)

# Soru 1: Gelen yolcuların ortalama yaşı ne kadardır?

african_names %>% summarise(mean(age, na.rm = TRUE))

# Soru 2: Gelen yolcular hangi ülke orijinlidirler?

african_names %>% count(country_origin, sort = TRUE)

# Soru 3: Gelen yolcularda en çok rastlanan isim hangisidir?

african_names %>% count(name, sort = TRUE)

# Soru 4: Yaş histogramı nedir?

african_names %>% ggplot(aes(age)) +
  geom_histogram()

# Soru 5: Cinsiyet için sütun grafiği nedir?

a <- african_names %>%
  count(gender, sort = TRUE) %>% 
  head(10)

a %>% ggplot(aes(gender, n)) +
  geom_col()




# Boxplot çizmek ----

slave_routes %>%
  filter(!is.na(ship_name), !is.na(n_slaves_arrived)) %>% # NA verileri dışlamak
  filter(fct_lump(ship_name, 12) != "Other") %>% 
  mutate(ship_name = fct_reorder(ship_name, n_slaves_arrived)) %>%
  ggplot(aes(n_slaves_arrived, ship_name)) +
  geom_boxplot()


# Soru 1: Hangi gemi ortalama ne kadar mesafe aldı (boxplot grafiği)

african_names %>% 
  filter(!is.na(ship_name), !is.na(height)) %>% 
  filter(fct_lump(ship_name, 16) != "Other") %>% 
  mutate(ship_name = fct_reorder(ship_name, height)) %>% 
  ggplot(aes(height, ship_name)) + 
  geom_boxplot()

# Soru 2: Hangi cinsiyetin boy ortalaması nasıldı?

african_names %>% 
  filter(!is.na(gender), !is.na(age)) %>% 
  filter(fct_lump(gender, 4) != "Other") %>% 
  mutate(gender = fct_reorder(gender, age)) %>% 
  ggplot(aes(age, gender)) +
  geom_boxplot()

# Soru 3: Hangi isimlerin boy ortalaması nasıldı?

african_names %>% 
  filter(!is.na(name), !is.na(height)) %>% 
  filter(fct_lump(name, 20) !="Other") %>% 
  mutate(name = fct_reorder(name, height)) %>% 
  ggplot(aes(height, name)) +
  geom_boxplot()


# geom_histogram() grafiği çizdirmek. ----

slave_routes %>%
  count(port_arrival, sort = TRUE)

slave_routes %>%
  count(port_origin, sort = TRUE)

slave_routes %>%
  ggplot(aes(year_arrival)) +
  geom_histogram() +
  geom_vline(xintercept = c(1776, 1865), lty = 2) +
  labs(title = "# of slave ships over time")


blackpast <- tuesdata$blackpast

# Soru 1: Hangi yıl en fazla olay yaşanmıştır?

blackpast %>% count(year, sort = TRUE) 

blackpast$year <- as.numeric(blackpast$year)

blackpast %>% 
  ggplot(aes(year)) +
  geom_histogram() +
  geom_vline(xintercept = c(1865, 1970), lty = 2) +
  geom_hline(yintercept = 2.966887, lty = 5)
  labs(title = "Hangi yıl en fazla olay yaşanmıştır?")

# Soru 2: her yıl ortalama kaç olay meydana gelmektedir?

blackpast %>% 
  count(year, sort = TRUE) %>% 
  summarise(ortalama = mean(n))










# Fartklı port_originler için çizgi grafiği çizmek. ----

slave_routes %>%
  filter(!is.na(port_origin)) %>%
  mutate(port_origin = str_trunc(port_origin, 25), # character verisini kısaltıyor.
         port_origin = fct_lump(port_origin, 24),
         port_origin = fct_reorder(port_origin, year_arrival)) %>%
  count(port_origin,
        decade = 10 * (year_arrival %/% 10)) %>%
  ggplot(aes(decade, n)) +
  geom_line() +
  facet_wrap(~ port_origin) +
  geom_vline(xintercept = c(1807), lty = 2) +
  theme(strip.text = element_text(size = 6))

# Soru 1: 10 yıllar itibariyle her port_embark'dan (en büyük 15 port_embark için) 
# gelen yolcu sayısı nedir?

african_names %>% 
  filter(!is.na(port_embark)) %>% 
  mutate(port_embark = str_trunc(port_embark, 25),
         port_embark = fct_lump(port_embark, 15),
         port_embark = fct_reorder(port_embark, year_arrival)) %>% 
  count(port_embark,
        decade = 10 * (year_arrival %/% 10)) %>% 
  ggplot(aes(decade, n)) + 
  geom_line() +
  facet_wrap(~ port_embark) + 
  geom_vline(xintercept = c(1807), lty = 2) +
  theme(strip.text = element_text(size = 6))

# New York'a gelen yolcu sayısını yıllar itibariyle azalan biçimde göster ---- 

slave_routes %>%
  filter(port_origin == "New York") %>%
  arrange(desc(year_arrival)) %>%
  View()

# Soru 1: country_origin'e göre "CAlabar"dan gelen yolcular yıllar
# itibariyle nasıl değişmiştir?

african_names %>%
  filter(country_origin == "Calabar") %>% 
  arrange(desc(year_arrival)) %>% 
  view()


# Hangi gemi, hangi rotada toplam seferlerin yüzde kaçı oranında seferler yapıldı? ----

slave_routes %>%
  filter(fct_lump(ship_name, 12) != "Other") %>%
  count(ship_name, port_origin, port_arrival, sort = TRUE) %>%
  group_by(ship_name) %>%
  mutate(percent = n / sum(n)) %>%
  top_n(1, percent) %>%
  arrange(desc(percent))

# Soru 1: Freetown'a giden (tek port_disembark) gemilerin kalktığı yer (port_embark)
# için hesaplandığında hangi gemiler en fazla sefer sayısı oranına sahip?

african_names %>% 
  filter(fct_lump(ship_name, 12) != "Other") %>% 
  count(ship_name, port_embark, port_disembark) %>% 
  group_by(ship_name) %>% 
  mutate(percent = n / sum(n)) %>% 
  top_n(1, percent) %>% 
  arrange(desc(percent))

# En yaygın köle yılları ----

library(grid)
library(ggraph)

set.seed(2020)

slave_routes %>%
  count(port_origin, port_arrival, sort = TRUE) %>%
  filter(port_origin != port_arrival) %>%
  head(40) %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(width = n),
                 arrow = arrow(type = "closed",
                               length = unit(.1, "inch"))) +
  geom_node_point() +
  geom_node_text(aes(label = name), repel = TRUE) +
  scale_edge_width_continuous(range = c(.5, 4)) +
  labs(title = "Common slave routes",
       edge_width = "# of ships")

# Soru 1: Hangi country_origin'den çıkanlar hangi port embark'tan hangi
# gemilerle taşınıyor?

african_names %>% 
  count(country_origin, port_embark, sort = TRUE) %>% 
  filter(country_origin != port_embark) %>% 
  head(50) %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(width = n),
                 arrow = arrow(type = "closed",
                               length = unit(.1, "inch"))) +
  geom_node_point() +
  geom_node_text(aes(label = name), repel = TRUE) +
  scale_edge_width_continuous(range = c(.5, 4)) +
  labs(title = "Köle memleketleri ve en yaygın toplanma limanları",
       edge_width = "# of ships")


# Soru 2: En yoğun olarak kullanılan köle yollarını bulalım.
# (Hangi port_embark --> port_disembark rotası şeklinde)

african_names %>% 
  count(port_embark, port_disembark, sort = TRUE) %>% 
  filter(port_embark != port_disembark) %>% 
  head(50) %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(width = n), 
                 arrow = arrow(length = unit(.05 , "inch"))) +
  geom_node_point() +
  geom_node_text(aes(label = name), repel = TRUE) +
  scale_edge_width_continuous(range = c(.5, 4)) +
  labs(title = "En fazla kullanılan köle rotaları",
       edge_width = "# of ships")

# Limanların tanımlayıcı özellikleri ----

slave_routes %>%
  group_by(port_origin) %>%
  summarize(n_ships = n(),
            total_recorded = sum(n_slaves_arrived, na.rm = TRUE),
            pct_missing = mean(is.na(n_slaves_arrived)),
            estimated_total = mean(n_slaves_arrived, na.rm = TRUE) * n()) %>%
  arrange(desc(n_ships))

# Soru 1: Bu grupta aşağıdaki soruların yanıtları aranır:
# Hangi limandan toplam kaç gemi ayrıldı?
# Kayıtlı toplam kaç köle taşındı bu limandan?
# Kaç köle taşındığı bilinmeyen seferlerin oranı nedir?
# Tahmini kaç köle taşınmıştır bu seferlerde?

# Bu soru farklı metotla çözülmeli.

a <- african_names %>% 
  group_by(port_embark, port_disembark, ship_name) %>% 
  count() %>% 
  arrange(ship_name)

# census datasını yükleyelim ----

census <- tuesdata$census

# geom_col()  ile yıllar itibariyle değişim ----

census_gathered <- census %>%
  mutate(other = total - white - black) %>%
  arrange(year) %>%
  gather(racial_category, population, white, black_free, black_slaves, other)

census_gathered %>%
  filter(region == "USA Total") %>%
  ggplot(aes(year, population, fill = racial_category)) +
  geom_col() +
  scale_y_continuous(labels = comma) +
  labs(x = "Year",
       y = "Census Population",
       fill = "Racial category",
       title = "Census racial makeup of US, 1790-1870",
       subtitle = "No 'other' category existed before 1860")

df <- african_names %>% 
  group_by(year_arrival, ship_name) %>% 
  count() %>% 
  arrange(year_arrival)

# Soru 1: Yıllar itibariyle ne kadar yolcu taşındığına bakalım.

df %>% 
  ggplot(aes(year_arrival, n)) +
  geom_col() +
  scale_x_continuous(breaks = seq(1810, 1860,10)) +
  labs(x = "Yıl",
       Y= "Transfered Slave Population",
       title = "Yıllar İtibariyle Gemilerle Taşınan Köle Sayısı")
  





census_gathered %>%
  filter(region != "USA Total", !is.na(division)) %>%
  mutate(division = fct_reorder(division, -population, sum)) %>%
  ggplot(aes(year, population, fill = racial_category)) +
  geom_col() +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ division) +
  labs(x = "Year",
       y = "Census Population",
       fill = "Racial category",
       title = "Census racial makeup of US, 1790-1870",
       subtitle = "No 'other' category existed before 1860")

# Cinsiyetlere göre sınıflandırıldığında yıllar itibariyle köle transferi nedir?

african_names %>% 
  group_by(year_arrival) %>% 
  count(gender, port_disembark, ship_name) %>% 
  filter(!is.na(ship_name), !is.na(gender)) %>% 
  mutate(port_disembark = fct_reorder(port_disembark, -n, sum)) %>% 
  ggplot(aes(year_arrival, n)) +
  geom_col() +
  facet_wrap(~ gender)


census_gathered %>%
  filter(region != "USA Total", !is.na(division)) %>%
  mutate(division = fct_reorder(division, -population, sum)) %>%
  group_by(division, year) %>%
  mutate(percent = population / sum(population)) %>%
  ggplot(aes(year, percent, fill = racial_category)) +
  geom_col() +
  scale_y_continuous(labels = percent) +
  facet_wrap(~ division) +
  labs(x = "Year",
       y = "% of Census Population",
       fill = "Racial category",
       title = "Census racial makeup of US, 1790-1870",
       subtitle = "No 'other' category existed before 1860")

# Soru 1: Hangi yıllarda hangi cinsiyet dağılımında köle transferi gerçekleşmiştir?

african_names %>% 
  group_by(gender, year_arrival, ship_name, port_disembark) %>% 
  count() %>% 
  filter(!is.na(ship_name), !is.na(gender)) %>% 
  mutate(percent = n / sum(n)) %>% 
  ggplot(aes(year_arrival, percent, fill = gender)) +
  geom_col() +
  scale_y_continuous(labels = percent) 
  # facet_wrap(~ port_disembark)


# ggwordcloud ----

library(ggwordcloud)

name_counts <- tuesdata$african_names %>%
  count(name, gender, sort = TRUE)

wordcloud(name_counts$name, name_counts$n)

name_counts %>%
  head(100) %>%
  ggplot(aes(label = name, size = n, color = gender)) +
  geom_text_wordcloud()

# tidytext ----

library(tidytext)

tuesdata$african_names %>%
  filter(!is.na(gender)) %>%
  mutate(gender = fct_recode(gender, Man = "Boy", Woman = "Girl")) %>%
  count(name, gender, sort = TRUE) %>%
  group_by(gender) %>%
  top_n(20, n) %>%
  ungroup() %>%
  mutate(name = reorder_within(name, n, gender)) %>%
  ggplot(aes(n, name)) +
  geom_col() +
  scale_y_reordered() +
  facet_wrap(~ gender, scales = "free")
