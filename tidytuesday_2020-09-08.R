# Kütüphaneleri yükleyelim ----

library(tidyverse)
library(tidytuesdayR)
library(scales)
theme_set(theme_light())

# Veri setlerini yükleyelim ----

tt <- tt_load("2020-09-08")

friends <- tt$friends

friends_info <- tt$friends_info

friends_emtions <- tt$friends_emotions

