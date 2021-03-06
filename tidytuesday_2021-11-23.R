# Kütüphaneleri yükleeyelim ----

library(tidyverse)
library(tidytuesdayR)
library(scales)
theme_set(theme_light())

# tidytuesday verisini çekelim ----

data <- tt_load("2021-11-23")

glimpse(data)

df_episodes <- data$episodes

# Data manipulation ----

episodes <- df_episodes %>%
  select(-serial_title) %>% # serial_title değişkeni hariç diğer değişkenleri seç.
  fill(season_number) %>% # boş hücreleri yukarıdan aşağı mantıkla doldurur.
  mutate(episode = paste0(season_number, ".", coalesce(as.character(episode_number), "X"), " ", episode_title),
         episode = fct_reorder(episode, first_aired),
         episode_title = fct_reorder(episode_title, first_aired),
         overall_episode_number = as.integer(episode_title))


writers <- data$writers

imdb <- data$imdb

glimpse(imdb)

# Soru 1: "desc" değişkenini hariç bırakalım.

imdb_1 <- imdb %>% 
  select(-desc)

# Bir değişkenden bazı istenmeyen karakterleri kaldırma ----

# Alfanümerik Olmayan Tüm Karakterleri Kaldırma  

str_replace_all(my_string, "[^[:alnum:]]", "")    # Delete non-alphanumeric

# Tüm Noktalama Karakterlerini Kaldırma

str_replace_all(my_string, "[[:punct:]]", "")     # Delete punctuation

# Üstteki yöntemler işe yaramadı. Alternatif olarak;

df %>%
  mutate(yeni_değişken = gsub(pattern     = "[^a-z,A-Z]", 
                        replacement = "", 
                        eski_değişken))

# Soru 1: air_date değişkenindeki tüm noktalama işaretlerini kaldıralım.

imdb_2 <- imdb_1 %>% 
  mutate(air_date2 = str_replace_all(pattern = "[.]", #gsub() da kullanılabilir.
                                     replacement = "",
                                     air_date))

# Tarih formatı düzenleme ----

# %Y: 4-digit year (1982)
# %y: 2-digit year (82)
# %m: 2-digit month (01)
# %d: 2-digit day of the month (13)
# %A: weekday (Wednesday)
# %a: abbreviated weekday (Wed)
# %B: month (January)
# %b: abbreviated month (Jan)
# 
# as.Date("1982-01-13")
# as.Date("Jan-13-82", format = "%b-%d-%y")
# as.Date("13 January, 1982", format = "%d %B, %Y")

# Soru 1: air_date2 değişkenimizi tarih değişkeni olarak düzenleyelim.

imdb_2$air_date4 <- as.Date(imdb_2$air_date3, format = "%d %b %Y")

imdb_2 <- imdb_2 %>% 
  mutate(date = lubridate::dmy(air_date3))

# geom_col() grafiği ----

episodes %>%
  filter(season_number <= 4) %>%
  ggplot(aes(episode_title, uk_viewers, fill = factor(season_number))) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "",
       y = "# of UK viewers (millions)",
       title = "UK Viewers per episode of Doctor Who (Davies years)",
       fill = "Season")

# Soru 1: imdb_2 veri setinde 1-4. sezonlar için hangi episode'un ne kadar
# imdb rating sahibi olduğuna bakalım. 


# gr_rating <- imdb_2 %>% 
#   filter(season <= 4) %>% 
#   ggplot(aes(ep_num, rating, fill = factor(season))) +
#   geom_col() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Not: Sezonların bölümleri 1-10 arası sıralandığından geom_col()
# toplulaştırılmış bir çubuk grafik vermekte. yani her son 1. bölüm tek bir 
# çubuk, 2. ve diğer bölümler de öyle.
# Öyleyse sezon ve bölüm değişkenlerini birleştirelim. Mesela 1.1 gibi.
# paste0() komutunu mutate() ile çalıştıralım.

gr_rating2 <- imdb_2 %>% 
  filter(season <= 4) %>% 
  mutate(bolum = paste0("S", season, "E", ep_num)) %>%
  mutate(bolum = fct_reorder(bolum, date)) %>% 
  ggplot(aes(bolum, rating, fill = factor(season))) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Grafik analiz ----

episodes %>%
  filter(season_number <= 4) %>%
  ggplot(aes(episode_title, rating)) +
  geom_line(group = 1) +
  geom_point(aes(color = factor(season_number))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "",
       y = "Average rating (IMDb)",
       title = "Popularity of episodes (Davies years)",
       fill = "Season")

# Soru 1: Tüm sezonlar boyunca nasıl bir IMDB rating performansı vardı?

imdb_2$date <- as.Date(imdb_2$date)

imdb_2 %>% 
  # filter(date >= "2020-01-01") %>%
  # filter(season <= 4) %>% 
  ggplot(aes(factor(date), rating)) + 
  geom_line(group = 1) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Grafik analiz 2 ----

episodes %>%
  filter(!is.na(rating)) %>%
  ggplot(aes(as.numeric(episode_title), rating)) +
  geom_line(group = 1) +
  geom_point(aes(color = factor(season_number))) +
  geom_text(aes(label = episode_title), hjust = 1, vjust = 1, check_overlap = TRUE) +
  geom_smooth(method = "loess") +
  theme(axis.text.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "",
       y = "Average rating (IMDb)",
       title = "Rating of episodes over time",
       color = "Season")

# Soru 3: Zaman içerisinde meydana gelen rating değişimine bakalım.
# Ama öncelikle yeni bir değişken oluşturalım.

# install.packages("ggpmisc") # statpeaks() komutu için
library(ggpmisc)

imdb3 <- imdb2 %>% 
  mutate(season_epsiode = paste0(season, ".", ep_num)) %>% 
  mutate(season_epsiode = fct_reorder(season_epsiode, date))

imdb3 %>% 
  ggplot(aes(as.numeric(season_epsiode), rating)) +
  geom_smooth(method = "loess") +
  geom_line(group = 1) +
  geom_point(aes(color = factor(season))) +
  stat_peaks(colour = "red") +
  stat_peaks(geom = "text", colour = "blue", vjust = -0.5, 
             check_overlap = TRUE, span = NULL) +
  stat_valleys(colour = "red") +
  stat_valleys(geom = "text", colour = "red", vjust = 0.5,
               check_overlap = TRUE, span = NULL) 

# Grafik analiz 3 ----

episodes %>%
  ggplot(aes(as.numeric(episode_title), uk_viewers, fill = factor(season_number))) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "",
       y = "# of UK viewers (millions)",
       title = "UK Viewers per episode of Doctor Who (Davies years)",
       fill = "Season")

# t-test ----

summarize_episodes <- function(tbl) {
  tbl %>%
    summarize(avg_rating = mean(rating, na.rm = TRUE),
              avg_viewers = mean(uk_viewers, na.rm = TRUE),
              n_episodes = n(),
              t_test = list(broom::tidy(t.test(rating[!is.na(rating)])))) %>%
    unnest(t_test)
}

episodes %>%
  group_by(season_number) %>%
  summarize_episodes()



# Soru 1: Özet istatistikler için fonksiyon oluşturalım.

descriptive_stats <- function(descriptive) {
  descriptive %>% 
    summarise(avg_rates = mean(rating, na.rm = TRUE),
              avg_rates_n = mean(rating_n, na.rm = TRUE),
              total_episodes = n(),
              t_test = list(broom::tidy(t.test(rating[!is.na(rating)])))) %>% 
    unnest(t_test)
}


imdb3 %>% 
  group_by(season) %>% 
  descriptive_stats()

























