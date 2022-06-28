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




# ggplot grafik----

by_player %>%
  filter(n_matches >= 200) %>%
  ggplot(aes(n_matches, pct_winner, color = gender)) +
  geom_point() +
  scale_x_log10() +
  scale_y_continuous(labels = percent) +
  labs(x = "# of matches since 2000 (log scale)",
       y = "% of matches won")

# Soru 1: en erken ve en geç galibiyet alma yaşları neydi? 

by_age <- vb_player_matches %>% 
  filter(winner_loser == "W") %>% 
  group_by(name, gender) %>%
  summarise(toplam_mac = n(),
            ilk_galibiyet = min(age),
            son_galibiyet = max(age)) %>% 
  arrange(desc(toplam_mac)) %>% 
  ungroup()
  
# Soru 2: En erken galibiyet alanları, toplam maç sayılarıyla çizdirelim. 
# Renklendirme "cinsiyet" kategorisine göre olsun.
# En az 100 maç yapmış olsun.

by_age %>%
  filter(toplam_mac >= 100) %>% 
  ggplot(aes(toplam_mac, ilk_galibiyet, color = gender)) +
  geom_point() 

# Soru 2: En son galibiyet alanları, toplam maç sayılarıyla çizdirelim. 
# Renklendirme "cinsiyet" kategorisine göre olsun.
# En az 100 maç yapmış olsun.

by_age %>% 
  filter(toplam_mac >= 100) %>% 
  ggplot(aes(toplam_mac, son_galibiyet, color = gender)) +
  geom_point() +
  labs(x = "Toplam Maç sayısı (n >= 100)",
       y = "Son Galibiyet", 
       title = "Sprocuların Son Galibiyetini Aldıklara Yaşa Göre Dağılım Grafiği",
       subtitle = "Cinsiyet kategorisine göre renklendirilmiş",
       caption = "tidy-tuesday verisi")

# En fazla galibiyet oranına sahip sporcuların listelenmesi ----

by_player %>%
  filter(n_matches >= 200) %>%
  arrange(desc(pct_winner))

# Değişkenlerde verinin bulunmadığı durumların oranı ----

vb_player_matches %>%
  summarize_all(~ mean(!is.na(.))) %>%
  gather() %>%
  View()

vb_player_matches %>%
  group_by(tournament) %>%
  summarize(pct_has_attacks = mean(!is.na(tot_attacks)),
            n = n()) %>%
  arrange(desc(n))

# Soru 1: Her turnuvadaki ortalama yaşı bulalım.
# Turnuva yaş ortalamasını büyükten küçüğe sıralayalım.

by_yas <- vb_player_matches %>% 
  filter(!is.na(age)) %>% # yaş değişkeninden NA verileri çıkart 
  group_by(tournament) %>% 
  summarise(ort_yas = mean(age), # turnuvadaki ortalama yaşı bulalım.
            total = n()) %>% 
  arrange(desc(ort_yas))
  
# Diğer bir yolu da şöyle:

by_yas2 <- vb_player_matches %>% 
  group_by(tournament) %>% 
  summarise(toplam = n(),
            ort_yas2 = mean(age, na.rm = TRUE)) %>% # yaş'tan NA verileri çıkart 
  arrange(desc(ort_yas2))


### How would we predict if a player will win in 2019? ----

summarize_players <- . %>%
  summarize(n_matches = n(),
            pct_winner = mean(winner_loser == "W"),
            avg_attacks = mean(tot_attacks, na.rm = TRUE),
            avg_errors = mean(tot_errors, na.rm = TRUE),
            avg_serve_errors = mean(tot_serve_errors, na.rm = TRUE),
            avg_kills = mean(tot_kills, na.rm = TRUE),
            avg_aces = mean(tot_aces, na.rm = TRUE),
            n_with_data = sum(!is.na(tot_attacks))) %>%
  ungroup() %>%
  arrange(desc(n_matches))

players_before_2019 <- vb_player_matches %>%
  filter(year < 2019) %>%
  group_by(name, gender, hgt, birthdate, country) %>%
  summarize_players() %>%
  filter(!is.na(avg_attacks))

players_2019 <- vb_player_matches %>%
  filter(year == 2019) %>%
  group_by(name, gender, hgt, birthdate, country, year,
           age = year - year(birthdate)) %>%
  summarize_players()


performance_joined <- players_before_2019 %>%
  inner_join(players_2019 %>%
               select(name, n_matches, pct_winner),
             by = "name",
             suffix = c("", "_2019"))

performance_joined %>%
  filter(n_matches >= 10,
         n_matches_2019 >= 5) %>%
  ggplot(aes(pct_winner, pct_winner_2019)) +
  geom_point() +
  geom_abline(color = "red") +
  geom_smooth(method = "lm")

performance_joined %>%
  mutate(n_wins_2019 = n_matches_2019 * pct_winner_2019,
         country = fct_lump(country, 3)) %>%
  glm(cbind(n_wins_2019, n_matches_2019 - n_wins_2019) ~
        pct_winner + avg_errors + avg_serve_errors,
      data = .,
      family = "binomial") %>%
  summary()

players_before_2019 %>%
  filter(n_with_data >= 20) %>%
  ggplot(aes(avg_serve_errors, avg_aces, size = n_with_data)) +
  geom_point() +
  labs(size = "Matches",
       title = "DON'T TRUST THIS")


player_first_year_summarized %>%
  filter(!is.nan(avg_attacks))
