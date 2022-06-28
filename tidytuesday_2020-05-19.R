# Kütüphaneleri yükleyelim ----

library(tidyverse)
library(scales)
library(lubridate)
theme_set(theme_light())

tuesdata <- tidytuesdayR::tt_load('2020-05-19')

vb_matches <- tuesdata$vb_matches %>%
  mutate(match_id = row_number())

# count() fonksiyonu ----

vb_matches %>%
  count(circuit, tournament, date, sort = TRUE)

vb_matches %>%
  count(gender, sort = TRUE)

vb_matches %>%
  count(year) 

vb_matches %>% 
  count(country, gender, sort = TRUE) %>%
  head(10) %>% 
  ggplot(aes(country, n, fill = gender)) +
  geom_col(position = "dodge")

vb_long <- vb_matches %>%
  # yeniden isimlendirme
  rename(w_p1_name = w_player1, w_p2_name = w_player2,
         l_p1_name = l_player1, l_p2_name = l_player2,
         w_team_rank = w_rank, l_team_rank = l_rank) %>%
  #  burada belirtilen sütunlar için yeni sütunlar oluşturur.
  mutate_at(vars(starts_with("w_"), starts_with("l_")), as.character) %>%
  pivot_longer(cols = c(starts_with("w_"), starts_with("l_"))) %>%
  separate(name, c("winner_loser", "player", "name"),
           sep = "_",
           extra = "merge",
           fill = "right") %>%
  mutate(winner_loser = str_to_upper(winner_loser))

# mutate_all() fonksiyonu tüm değişkenler üzerinde değişiklik yapılır ----

iris <- iris

# 5. sıradaki değişken hariç, tüm değişkenlerin yüzdesini al.
iris_a <- mutate_all(iris[,-5], funs("percent" = ./100))

# mutate_at(), belirtilen sütunlar için yeni sütunlar oluşturur ----

iris %>% head(10)

# Soru 1: petal.width değişkeninin yüzdesini alalım. 
iris_petal <- iris %>% mutate_at(vars(Petal.Width), funs("percent" = ./100))

iris_b = mutate_at(iris, 
                    vars(Sepal.Length,Sepal.Width), 
                    funs(Rank=min_rank(desc(.))))

# vb_long verisetini düzenlemek ----

vb_player_matches <- vb_long %>%
  filter(name != "rank") %>%
  spread(name, value) %>%
  type_convert()

# vb_sets verisetini düzenlemek ----

vb_sets <- vb_matches %>%
  select(match_id, circuit:match_num, score) %>%
  separate_rows(score, sep = ", ") %>%
  mutate(score = str_remove(score, " retired")) %>%
  mutate(score = na_if(score, "Forfeit or other")) %>%
  separate(score, c("w_score", "l_score"), convert = TRUE)




## Look at players ----

by_player <- vb_player_matches %>%
  group_by(name, gender) %>%
  summarize(n_matches = n(),
            pct_winner = mean(winner_loser == "W"),
            first_game = min(date),
            last_game = max(date)) %>%
  arrange(desc(n_matches)) %>%
  ungroup()

# Soru 1: Hangi turnuvada oyunlar ortalama kaç dakika sürdü? 
# Soru 2: Turnuvalarda yarışan sporcular için ortalama yaş kaçtı?

b_tourn <- na.omit(vb_player_matches) # Tüm NA verileri sileriz

b_tourn2 <- b_tourn %>% 
  group_by(tournament) %>% 
  summarise(total_matches = n(),
            ort_yaş = mean(age),
            ort_süre = mean(duration, convert = FALSE),
            yıl = min(year)) %>% 
  arrange(desc(total_matches)) %>% 
  ungroup()
