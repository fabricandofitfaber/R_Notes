# Kütüphaneleri yükleyelim ----

library(tidyverse)
library(lubridate)
library(scales)
theme_set(theme_light())

# Veri setini yükleyelim ----

tuesdata <- tidytuesdayR::tt_load('2020-04-21')

df_text <- tuesdata$gdpr_text

df_violations <- tuesdata$gdpr_violations

library(Hmisc)

str(df_text)
str(df_violations)

gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv') %>%
  mutate(date = na_if(mdy(date), "1970-01-01")) %>%
  rename(country = name)


# Grafik Analiz ----

gdpr_violations %>% 
  summarise(Toplam = sum(price))

gdpr_violations %>%
  count(country = fct_lump(country, 8, w = price),
        sort = TRUE, wt = price, name = "total_price") %>%
  mutate(country = fct_reorder(country, total_price)) %>%
  ggplot(aes(total_price, country)) +
  geom_col() +
  scale_x_continuous(labels = dollar_format())
        
# Soru 1: En fazla violations yaşanan 10 ülke hangileridir?

gdpr_violations %>% 
  count(country = fct_lump(country, 9)) %>%
  mutate(country = fct_reorder(country, n)) %>% 
  ggplot(aes(n, country)) +
  geom_col()+
  geom_text(aes(label = n), hjust = -0.5) # değerlerden tex eklemek

# Grafik Analiz 2 ----

gdpr_violations %>%
  count(month = floor_date(date, "month"),
        country = fct_lump(country, 6, w = price),
        sort = TRUE, wt = price, name = "total_price") %>%
  mutate(country = fct_reorder(country, -total_price, sum)) %>%
  ggplot(aes(month, total_price, fill = country)) +
  geom_col() +
  scale_y_continuous(labels = dollar_format()) +
  labs(x = "Time",
       y = "Total fines",
       fill = "Country")

# source değişkeni ----

gdpr_violations %>%
  count(source, country, sort = TRUE)

gdpr_violations %>%
  select(controller, date, article_violated, type, summary, price) %>%
  mutate(summary = str_trunc(summary, 140)) %>%
  arrange(desc(price)) %>%
  mutate(price = dollar(price)) %>%
  head(10)

# Grafik analiz 3 ----

my_list <- gdpr_violations %>%
  mutate(type = fct_lump(type, 8, w = price),
         type = fct_reorder(type, price),
         country = fct_lump(country, 5)) %>%
  ggplot(aes(price, type)) +
  geom_boxplot() +
  geom_jitter(aes(color = country), width = 0, height = .25) +
  scale_x_log10(labels = dollar_format())

### Which article was violated? ----

# Tekrar eden hücreleri dışlayarak bir alt veri seti oluşturmak.

article_titles <- df_text %>%
  distinct(article, article_title)

df_new <- df_text %>% 
  distinct(article, gdpr_text)

df_new2 <- df_text %>% 
  distinct(chapter, chapter_title)

separated_articles <- gdpr_violations %>%
  separate_rows(article_violated, sep = "\\|") %>%
  extract(article_violated, "article", "Art\\. ?(\\d+)", convert = TRUE, remove = FALSE) %>%
  left_join(article_titles, by = "article") %>%
  mutate(article_title = paste0(article, ". ", str_trunc(article_title, 50)),
         article_title = ifelse(is.na(article), "Unknown", article_title)) %>%
  add_count(id) %>%
  mutate(price_per_article = price / n)


separated_articles %>%
  group_by(article = fct_lump(article_title, 8, w = price)) %>%
  summarize(total_fine = sum(price_per_article),
            violations = n()) %>%
  arrange(desc(total_fine)) %>%
  mutate(article = fct_reorder(article, total_fine)) %>%
  ggplot(aes(total_fine, article)) +
  geom_col() +
  scale_x_continuous(labels = dollar_format()) +
  labs(title = "What articles got the most fines?",
       y = "",
       x = "Total fine")

new_df <- gdpr_violations %>%
  filter(str_detect(controller, "Vodafone")) %>%
  group_by(date, country) %>%
  summarize(violations = n(),
            total_fine = sum(price)) %>%
  ggplot(aes(date, total_fine, size = violations, color = country)) +
  geom_point() +
  scale_y_continuous(labels = dollar_format()) +
  scale_size_continuous(guide = FALSE) +
  labs(title = "Vodafone's GDPR violations",
       color = "",
       x = "Time",
       y = "Total fine on this day")

# Soru 1: seperated_articles veri setinde article_title da 
# processing kelimesini içeren verilerin nokta grafiğini çizelim.

separated_articles %>% 
  filter(str_detect(article_title, "data")) %>% 
  group_by(date, country) %>%
  summarise(total_ihlal = n(),
            total_ceza = sum(price)) %>%
  head(5) %>% 
  ggplot(aes(date, total_ihlal, size = total_ceza, color = country)) +
  geom_point() +
  scale_y_continuous(labels = dollar_format()) +
  scale_size_continuous(guide = FALSE)

# tidymetrics paketi ----

# install.packages("remotes")
# library(remotes)
# install_github("datacamp/tidymetrics")


library(tidymetrics)

summarized <- separated_articles %>%
  filter(!is.na(date)) %>%
  mutate(country = fct_lump(country, 6, w = price_per_article),
         article_title = fct_lump(article_title, 6, w = price_per_article),
         type = fct_lump(type, 6, w = price_per_article)) %>%
  cross_by_dimensions(country, article_title, type) %>%
  cross_by_periods(c("month", "quarter")) %>%
  summarize(nb_violations = n_distinct(id),
            nb_total_fine = sum(price_per_article)) %>%
  ungroup()







