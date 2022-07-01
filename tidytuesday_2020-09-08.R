# Kütüphaneleri yükleyelim ----

library(tidyverse)
library(tidytuesdayR)
library(scales)
theme_set(theme_light())

# Veri setlerini yükleyelim ----

tt <- tt_load("2020-09-08")

friends <- tt$friends

episodes <- tt$friends_info

friends_emotions <- tt$friends_emotions

# glue paketi ----

library(glue)

episodes <- episodes %>% 
  mutate(full_title = glue("{ season }.{ episode } { title }"),
         full_title = fct_reorder(full_title, season + .001 * episode))


# Soru 1: Directed by and written by değişkenlerini birleştirelim

episodes <- episodes %>% 
  mutate(directed_and_written = glue("{ directed_by } , { written_by }"))

# Soru 2: Bir değişkeni farklı sütunlara nasıl dağıtabiliriz?

episodes2 <- episodes %>% 
  separate(directed_and_written, c("directed_lan", "written_lan"), ",")

# written_by değişkenini & işareti yüzünden bölelim.

episodes2 <- episodes %>% 
  separate(written_by, c("ilk_yonetmen", "ikinci_yonetmen"), "&")

# Veri setlerini birleştirelim ----

friends <- tt$friends %>%
  inner_join(episodes, by = c("season", "episode"))

# Hangi bölümde ne kadar cümle var?

friends %>%
  count(season, full_title)

# Hangi karakter ne kadar cümleye sahip?

tt$friends %>%
  count(speaker, sort = TRUE)

# Graph ratings over time ----

episodes %>%
  ggplot(aes(as.integer(full_title), us_views_millions)) +
  geom_line() +
  geom_point(aes(color = factor(season))) +
  geom_text(aes(label = title), vjust = 1, hjust = 1,
            check_overlap = TRUE,
            size = 2) +
  expand_limits(y = 0) +
  labs(x = "Episode number",
       color = "Season")

episodes %>%
  ggplot(aes(as.integer(full_title), imdb_rating)) +
  geom_line() +
  geom_point(aes(color = factor(season))) +
  geom_text(aes(label = title), vjust = 1, hjust = 1,
            check_overlap = TRUE,
            size = 2) +
  labs(x = "Episode number",
       y = "IMDB Rating",
       color = "Season")

# Predict a rating based on the characters who speak? ----

speaker_lines_per_episode <- friends %>%
  count(speaker, title, imdb_rating, season) %>%
  complete(speaker, title, fill = list(n = 0)) %>%
  group_by(title) %>%
  fill(imdb_rating, season, .direction = "downup") %>%
  ungroup() %>%
  add_count(title, wt = n, name = "episode_total") %>%
  mutate(pct = n / episode_total)

speaker_lines_per_episode %>%
  semi_join(main_cast, by = "speaker") %>%
  mutate(speaker = fct_reorder(speaker, pct)) %>%
  ggplot(aes(pct, speaker)) +
  geom_boxplot() +
  scale_x_log10()


speaker_lines_per_episode %>%
  semi_join(main_cast, by = "speaker") %>%
  group_by(speaker) %>%
  summarize(correlation = cor(log2(pct), imdb_rating))




## Alıştırma Soruları ----

# friends_emotions veri seti için araştırma soruları

str(friends_emotions)
library(Hmisc)

describe(friends_emotions)

# Hangi bölümde hangi duyguların en fazla hissedildiğini tespit edelim.

friends_emotions %>% 
  count(season, episode, scene, emotion, sort = TRUE)

str(friends_emotions$emotion)

# Soru 1: En fazla eğlenilen (joyful) ve en fazla sinirlenilen 3 bölümü bulalım.

soru_1 <- friends_emotions %>% 
  filter(emotion == "Joyful") %>% 
  count(season, episode, scene, emotion, sort = TRUE) %>% 
  head(3)

soru_1_1 <- friends_emotions %>% 
  filter(emotion == "Mad") %>% 
  count(season, episode, scene, sort = TRUE) %>% 
  head(3)

# Soru 2: Hangi sezonlarda duygu dağılımının nasıl gerçekleştiğine bakalım.

soru_2 <- friends_emotions %>% 
  count(season, emotion, sort = TRUE) %>% 
  ggplot(aes(season, n, fill = emotion)) +
  geom_col(position = "dodge")

# Soru 3: farklı sezonlarda emotion türlerinin yüzdesini alalım.

soru_3 <- friends_emotions %>% 
  count(season, emotion, sort = TRUE) %>% 
  mutate_at(vars(n), funs("percent" = ./sum(n)*100))

# Soru 4: Yüzdesi alınan emotionların sütun grafiğini çizdirelim.

soru_4 <- friends_emotions %>% 
  count(season, emotion, sort = TRUE) %>% 
  mutate_at(vars(n), funs("yuzde" = ./sum(n)*100)) %>% 
  ggplot(aes(season, yuzde, fill = emotion)) +
  geom_col(position = "dodge")
  
  
  
  
  
  
  
  
  
  
  








































