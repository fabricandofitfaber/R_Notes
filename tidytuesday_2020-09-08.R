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










