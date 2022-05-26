# Kütüphaneleri yüklemek ----

library(tidyverse)

theme_set(theme_light())

# Verisetlerini yüklemek ----

critic <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/critic.tsv')
user_reviews <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')
items <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv')
villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')

villagers
items %>%
  View()

# Text Analysis ----

critic %>%
  ggplot(aes(grade)) +
  geom_histogram()

user_reviews %>%
  ggplot(aes(grade)) +
  geom_histogram()

head(user_reviews$text)

options(scipen = 999)

items %>% 
  ggplot(aes(sell_value)) +
  geom_histogram()

# tidytext ----

library(tidytext)
library(lubridate)

user_review_words <- user_reviews %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  count(user_name, date, grade, word)

# blackpast verisini yüklemek.

blackpast <- tuesdata$blackpast

# Soru 1: blackpast verisinde events değişkeninde en çok hangi kelimeler 
# kullanılmış? (era'ya göre renkli sınıflandırılma)

blackpast_words <- blackpast %>% 
  unnest_tokens(word, events) %>% 
  anti_join(stop_words, by = "word") %>% 
  count(subject, country, state, era, year, word)

blackpast_words %>% 
  head(100) %>% 
  ggplot(aes(label = word, size = n, color = era)) +
  geom_text_wordcloud()


# Soru 2: critic verisetinde en fazla geçen kelime nedir? 
# Not: grade'e göre sınıflandır.

critic_words <- critic %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words, by = "word") %>% 
  count(publication, date, grade, word)

critic_words %>% 
  head(100) %>% 
  ggplot(aes(label = word, size = n)) +
  ggwordcloud::geom_text_wordcloud()

















# Özet istatistikler ----

by_week <- user_reviews %>%
  group_by(week = floor_date(date, "week", week_start = 1)) %>%
  summarize(nb_reviews = n(),
            avg_grade = mean(grade),
            pct_zero = mean(grade == 0),
            pct_ten = mean(grade == 10))

blackpast <- tuesdata$blackpast

blackpast$year <- gsub("s", "", as.character(blackpast$year))

blackpast$year <- as.Date(blackpast$year, format = "%Y")

# Soru 1: 10 yıllar halinde gruplandığında en fazla hangi yıllarda olay yaşandı?
# blackpast veriseti için

by_year <- blackpast %>% 
  group_by(onyil = floor_date(year, years(10))) %>% 
  summarise(toplam_olay = n()) %>% 
  arrange(desc(toplam_olay))

# Grafik çizdirmek (Çizgi ve Nokta) ----

by_week %>%
  ggplot(aes(week, avg_grade)) +
  geom_line() +
  geom_point(aes(size = nb_reviews)) +
  expand_limits(y = 0) +
  labs(x = "Time",
       y = "Average grade",
       size = "# of reviews")

# Yıllar itibariyle en fazla gelişm yaşanmışe yaşanmış yılların grafiğini çizelim.
# blackpast veriseti için

by_year %>% 
  ggplot(aes(onyil, toplam_olay)) +
  geom_line() +
  geom_point(aes(size = toplam_olay)) +
  expand_limits(y = 0) +
  labs(x = "Yıllar",
       y = "Toplam Olay",
       size = "Toplam Olay Sayısı")

# Grafik çizdirmek (Histogram) ----
  
user_reviews %>%
  ggplot(aes(grade)) +
  geom_histogram() +
  labs(title = "Most reviews were very low or very high")

# Soru 1: Yaşanan olayların histogramını çizdirelim.

by_year %>% 
  ggplot(aes(toplam_olay)) +
  geom_histogram() +
  labs(title = "On Yıllar İtibariyle Yaşanan Olay Sayısı")

# Grafik çizdirmek (0 - 10 puan verenlerin çizgi grafiği)----

by_week %>%
  gather(type, value, contains("pct")) %>%
  mutate(type = ifelse(type == "pct_zero", "% rated 0", "% rated 10")) %>%
  ggplot(aes(week, value, color = type)) +
  geom_line() +
  geom_point(aes(size = nb_reviews)) +
  scale_y_continuous(labels = scales::percent) +
  expand_limits(y = 0) +
  labs(x = "Time",
       y = "% of reviews",
       size = "Total reviews in week",
       title = "Reviews got more polarizing in middle of game")


# Alıştırma: critic veriseti ile text analizi yapalım. ----

by_grade <- critic %>% 
  group_by(week = floor_date(date, "week", week_start = 1)) %>% 
  summarise(total = n(),
            ort_grade = mean(grade),
            perc_doksan_alti = mean(grade < 90),
            perc_doksan_ustu = mean(grade >= 90))

by_grade %>% 
  ggplot(aes(week, total)) +
  geom_line() +
  geom_point(aes(size = ort_grade)) +
  labs(x = "Hafta",
       y = "Toplam Yorum Sayısı",
       title = "Haftalar itibariyle ortalama yorum sayısı ve ortalaması")

by_grade %>% 
  ggplot(aes(ort_grade)) +
  geom_histogram() +
  labs(title = "Yapılan yorumların ortalamasının Histogramı")

by_grade %>% 
  gather(type, value, contains("perc")) %>% 
  mutate(type = ifelse(type == "perc_doksan_alti", "90 altı oy verenler", "90 üstü oy verenler")) %>% 
  ggplot(aes(week, value, color = type)) +
  geom_line() +
  geom_point(aes(size = total)) +
  scale_y_continuous(labels = scales::percent) +
  expand_limits(y = 0) +
  labs(x = "Haftalar",
       y = "Değer (%)",
       title = "Haftalar İtibariyle Olumlu / Olumsuz Yorum Yapanlar Yüzdesi")
  
# En fazla ele alınan kelimelerin istatistikleri  ----

by_word <- user_review_words %>%
  group_by(word) %>%
  summarize(avg_grade = mean(grade),
            nb_reviews = n()) %>%
  arrange(desc(nb_reviews)) %>%
  filter(nb_reviews >= 25) %>%
  arrange(desc(avg_grade))

by_word %>%
  filter(nb_reviews >= 75) %>%
  ggplot(aes(nb_reviews, avg_grade)) +
  geom_point() +
  geom_text(aes(label = word), vjust = 1, hjust = 1, check_overlap = TRUE) +
  scale_x_log10()

# Soru 1: critic verisetindeki yorum text değişkenini kelimelere ayrıalım.

critic_words <- critic %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words, by = "word") %>% 
  count(grade, publication, date, word)

# Soru 2: 25'ten fazla yorumlarda kullanılmış kelimelerin sayısı ve ort_grade bul.

by_critic_word <- critic_words %>% 
  group_by(word) %>% 
  summarise(top_word = n(),
            ort_grade = mean(grade)) %>% 
  filter(top_word >= 5) %>% 
  arrange(desc(ort_grade))

# Soru 3: Yorumlarda 10'dan fazla kullanılan ifadeler ile yapılmış ortalama
# grade puanını grafikleyelim.

by_critic_word %>% 
  filter(top_word >= 10) %>% 
  ggplot(aes(top_word, ort_grade)) +
  geom_point() +
  geom_text(aes(label = word), vjust = 1, hjust = 1, check_overlap = TRUE) +
  scale_x_log10()


# Hangi kelimeler düşük dereceli yorumlarla ilişkili? ----

# En az 25 kere yorumlarda kullanılmış en negatif 20 kelime

by_word %>%
  top_n(20, -avg_grade) %>%
  ggplot(aes(nb_reviews, avg_grade)) +
  geom_point() +
  geom_text(aes(label = word), vjust = 1, hjust = 1, check_overlap = TRUE) +
  scale_x_log10() +
  labs(title = "What words were associated with low-grade reviews?",
       subtitle = "20 most negative words; only words in at least 25 reviews")

# Soru 1: Pozitif yorumlarda en sık tekrarlanmış 20 kelime nedir?

by_critic_word %>% 
  top_n(20, ort_grade) %>% 
  ggplot(aes(x = top_word, ort_grade)) +
  geom_point() +
  geom_text(aes(label = word), vjust = 1, hjust = 1, check_overlap = TRUE) +
  scale_x_log10() +
  labs(x = "En fazla tekrarlanan kelimeler",
       y = "Ortalama Yorum Puanı",
       title = "Pozitif yorumlarda en sık tekrarlanmış 20 kelime")

# widyr ve stm paketleri ----

pacman::p_load(widyr, stm)

library(widyr)
library(stm)
library(tidytext)

review_matrix <- user_review_words %>%
  group_by(word) %>%
  filter(n() >= 25) %>%
  cast_sparse(user_name, word, n)

topic_model_6 <- stm(review_matrix,
                     K = 6,
                     verbose = TRUE,
                     init.type = "Spectral",
                     emtol = 5e-5)


tidy(topic_model_6) %>%
  group_by(topic) %>%
  top_n(12, beta) %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  facet_wrap(~ topic, scales = "free_y")

tidy(topic_model_6) %>%
  filter(term == "progress")

topic_model_gamma <- tidy(topic_model_6, matrix = "gamma") %>%
  mutate(user_name = rownames(review_matrix)[document]) %>%
  inner_join(user_reviews, by = "user_name")

topic_model_gamma %>%
  group_by(topic) %>%
  top_n(1, gamma)

topic_model_gamma %>%
  group_by(topic) %>%
  summarize(correlation = cor(gamma, grade),
            spearman_correlation = cor(gamma, grade, method = "spearman"))

topic_model_gamma %>%
  group_by(week = floor_date(date, "week", week_start = 1),
           topic) %>%
  summarize(avg_gamma = mean(gamma)) %>%
  ggplot(aes(week, avg_gamma, color = factor(topic))) +
  geom_line() +
  expand_limits(y = 0) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Time",
       y = "Average gamma (document-topic association)")














