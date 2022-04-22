# Kütüphaneleri yükleyelim ve çağıralım. ----

# pacman::p_load(tidyverse, tidytuesdayR, scales, 
#                funModeling, Hmisc, gt, gtsummary)

library(tidyverse)
library(tidytuesdayR)
library(scales)
library(skimr)
library(funModeling)
library(Hmisc)
library(gt)
library(gtsummary)
theme_set(theme_light())

# Haftalık veriyi yükleyelim. ----

# freedom <- tt_load("2022-02-22")$freedom

# Verinin tanımlayıcı istatistikleri ----

tanımlayıcı_istatistikler <- function(data) {
  str(data)
  glimpse(data)
  df_status(data)
  freq(data)
  profiling_num(data)
  plot_num(data)
  describe(data)
  head(data)
  tail(data)
  colnames(data)
  rownames(data)
  summary(data)
  skimr::skim(data)
}

# df_status(): Her değişken için şunu döndürür: Sıfırların miktarı ve yüzdesi 
# (sırasıyla q_zeros ve p_zeros).

# profiling_num() :Sayısal olmayan değişkenleri otomatik olarak atlayarak, 
# tüm sayısal değişkenler için birçok gösterge içeren bir metrik tablosu verir.

# plot_num(): Sayısal değişkenler için tüm histogramları içeren bir çizim alır. 
# NA değerleri görüntülenmeyecektir.

gt(tanımlayıcı_istatistikler(freedom)) 

# https://www.r-bloggers.com/2018/08/exploratory-data-analysis-in-r-introduction/
# http://www.danieldsjoberg.com/gt-and-gtsummary-presentation/#1

freedom <- tt_load("2022-02-22")$freedom 

freedom <- freedom %>% 
  janitor::clean_names() %>%
  rename(cl = civil_liberties,
         pr = political_rights) %>%
  mutate(country_code = countrycode::countrycode(country, "country.name", "iso2c"))

# Ülke kodlarına göre bir değişken oluşturmak.
# https://github.com/vincentarelbundock/countrycode#:~:text=The%20countrycode%20function%20can%20convert,coding%20schemes%20or%20country%20names.

# Bazı soruların yanıtını verisetinden alalım. ----
# Her bir yılda kaç tane farklı ülke olduğunu bulabiliriz.

freedom %>% count(year, sort = TRUE) %>% 
  arrange(desc(year))

# Hangi kıtada ne kadar gözlem olduğuna bakalım.

freedom %>% count(region_name, sort = TRUE) %>% 
  arrange(desc(n))

# https://bioconnector.github.io/workshops/r-dplyr-homework.html
# 1.) Kıta başına kaç benzersiz ülke temsil ediliyor?
# (Hint: group_by then summarize with a call to n_distinct(...)).

freedom %>% group_by(region_name) %>% 
  summarise(n = n_distinct(country))
  
# 2.) Hangi ülkeler 2020'de en az kişisel özgürlüklere ve sivil haklara sahipti?
# (Hint: filter, arrange, head(n=1))

freedom %>% filter(year == 2020) %>% 
  arrange(desc(cl, pr)) %>% 
  head(n = 10)

# 3.) Her kıtada, 2010'larda ortalama kişisel özgürlükler ve politik haklar nasıldı?
# (Hint: filter, group_by, summarize)

freedom %>% filter(year >= 2010) %>% 
  group_by(region_name) %>% 
  summarise(ortalama_cl = mean(cl),
            ortalama_pr = mean(pr)) %>% 
  arrange(desc(ortalama_cl, ortalama_pr))
  
# 4.) Tüm yıllar boyunca toplam GSYİH'si en yüksek olan 5 ülke hangisidir? 
# (Hint: Mutate, group_by, summarize, arrange, head(n=5)))
  



# Hangi ülkenin, hangi kıtada bulunduğunu görebiliriz.

freedom %>% distinct(country, region_name)

summarize_freedom <- function(tbl) {
  tbl %>%
    summarize(n_countries = n(),
              avg_civil_liberties = mean(civil_liberties),
              avg_political_rights = mean(political_rights),
              pct_free = mean(status == "F"),
              .groups = "drop") %>%
    arrange(desc(n_countries))
}

by_region <- freedom %>%
  filter(year == 2020) %>%
  group_by(region_name) %>%
  summarize_freedom()

by_region %>%
  ggplot(aes(avg_civil_liberties, avg_political_rights)) +
  geom_abline(color = "red") +
  geom_point(aes(size = n_countries)) +
  geom_text(aes(label = region_name), vjust = 1, hjust = 1) +
  expand_limits(x = 0, y = 0, size = 0)

freedom %>%
  ggplot(aes(avg_civil_liberties, avg_political_rights)) +
  geom_point()

freedom %>%
  filter(year == 2020) %>%
  ggplot(aes(civil_liberties, political_rights)) +
  geom_abline(color = "red") +
  geom_jitter(height = .2, width = .2) +
  # geom_text(aes(label = region_name), vjust = 1, hjust = 1) +
  expand_limits(x = 0, y = 0, size = 0)

freedom %>%
  summarize(sd(civil_liberties),
            sd(political_rights))


freedom %>%
  filter(year == 2020) %>%
  gather(metric, value, civil_liberties, political_rights) %>%
  mutate(metric = str_to_title(str_replace_all(metric, "_", " ")),
         region_name = fct_reorder(region_name, value)) %>%
  count(region_name, metric, value) %>%
  ggplot(aes(value, n)) +
  geom_col() +
  facet_grid(region_name ~ metric) +
  labs(x = "World Freedom Index rating",
       y = "# of countries",
       title = "Distribution of World Freedom Index by region in 2020")


freedom_gathered <- freedom %>%
  gather(metric, value, civil_liberties, political_rights) %>%
  mutate(metric = str_to_title(str_replace_all(metric, "_", " ")),
         region_name = fct_reorder(region_name, value))

overall <- freedom_gathered %>%
  group_by(year, metric) %>%
  summarize(avg_rating = mean(value)) 


freedom_gathered %>%
  group_by(year, region_name, metric) %>%
  summarize(avg_rating = mean(value)) %>%
  ggplot(aes(year, avg_rating)) +
  geom_line(aes(color = region_name)) +
  geom_line(data = overall, size = 3) +
  facet_wrap(~ metric) +
  expand_limits(y = 1) +
  scale_y_reverse(breaks = seq(1, 7)) +
  scale_color_discrete(guide = guide_legend(reverse = TRUE)) +
  labs(x = "Year",
       y = "World Freedom Index rating",
       title = "World Freedom Index rating over time by region",
       color = "Region",
       subtitle = "Black line shows overall trend")

freedom %>%
  ggplot(aes(civil_liberties)) +
  geom_histogram()

pacman::p_load(WDI, countrycode)

library(WDI)
library(countrycode)
gdp_percap <- WDI(indicator = "NY.GDP.PCAP.CD",
                  extra = TRUE,
                  start = 1995,
                  end = 2020) %>%
  as_tibble()

freedom_joined <- freedom_gathered %>%
  inner_join(gdp_percap, by = c(country_code = "iso2c", "year"),
             suffix = c("", "_wdi")) %>%
  mutate(income = fct_relevel(income, c("Low income", "Lower middle income", "Upper middle income")))

freedom_joined %>%
  filter(income != "Not classified") %>%
  group_by(metric, income, year) %>%
  summarize(avg_rating = mean(value)) %>%
  ggplot(aes(year, avg_rating)) +
  geom_line(aes(color = income)) +
  geom_line(data = overall, size = 3) +
  facet_wrap(~ metric) +
  expand_limits(y = 1) +
  scale_y_reverse(breaks = seq(1, 7)) +
  scale_color_discrete(guide = guide_legend(reverse = TRUE)) +
  labs(x = "Year",
       y = "World Freedom Index rating",
       title = "World Freedom Index rating over time by region",
       color = "Worldbank Income",
       subtitle = "Black line shows overall trend")

freedom_joined %>%
  filter(year == 2020) %>%
  ggplot(aes(NY.GDP.PCAP.CD, value)) +
  geom_point() +
  geom_jitter(height = .2, width = 0) +
  facet_wrap(~ metric) +
  scale_x_log10()

library(broom)
civil_liberties_2020 <- freedom_joined %>%
  filter(metric == "Civil Liberties",
         year == 2020,
         !is.na(NY.GDP.PCAP.CD))

lin_mod <- civil_liberties_2020 %>%
  lm(value ~ region_name + log2(NY.GDP.PCAP.CD), data = .)

library(ggrepel)
lin_mod %>%
  augment(data = civil_liberties_2020) %>%
  select(country, NY.GDP.PCAP.CD, region_name, income, value, .fitted, .resid) %>%
  arrange(desc(abs(.resid))) %>%
  head(20) %>%
  ggplot(aes(.fitted, value)) +
  geom_point() +
  geom_text_repel(aes(label = country)) +
  geom_abline(color = "red") +
  labs(x = "Expected freedom index based on region + income",
       y = "Actual freedom index",
       title = "What are the largest outliers?") +
  expand_limits(x = 1, y = 1)

install.packages("fuzzyjoin")

library(fuzzyjoin)
freedom_2020 <- freedom_joined %>%
  filter(year == 2020)
world_map_freedom_2020 <- map_data("world") %>%
  as_tibble() %>%
  regex_left_join(maps::iso3166, c(region = "mapname")) %>%
  left_join(freedom_2020 %>% select(-region), by = c(a2 = "country_code")) %>%
  filter(region != "Antarctica")
world_map_freedom_2020 %>%
  filter(metric == "Civil Liberties") %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = value)) +
  coord_map(xlim = c(-180, 180)) +
  scale_fill_gradient2(low = "blue",
                       high = "red",
                       midpoint = 3.5,
                       guide = guide_legend(reverse = TRUE)) +
  ggthemes::theme_map() +
  labs(fill = "Civil Liberties Rating",
       title = "World Freedom Index: Civil Liberties",
       subtitle = "In 2020")

library(gganimate)
world_map_freedom <- map_data("world") %>%
  as_tibble() %>%
  regex_left_join(maps::iso3166, c(region = "mapname")) %>%
  left_join(freedom_joined %>% select(-region), by = c(a2 = "country_code")) %>%
  filter(region != "Antarctica")
world_map_freedom %>%
  filter(metric == "Civil Liberties") %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = value)) +
  coord_map(xlim = c(-180, 180)) +
  scale_fill_gradient2(low = "blue",
                       high = "red",
                       midpoint = 3.5,
                       guide = guide_legend(reverse = TRUE)) +
  ggthemes::theme_map() +
  transition_manual(year) +
  labs(fill = "Civil Liberties Rating",
       title = "World Freedom Index: Civil Liberties ({ current_frame })")

freedom %>%
  distinct(country, country_code) %>%
  View()
gdp_percap %>%
  filter(str_detect(country, "Iran"))
