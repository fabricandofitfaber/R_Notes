install.packages("waffle", repos = "https://cinc.rud.is")

library(waffle)

# Usage

library(waffle)
library(magrittr)
library(hrbrthemes)
library(ggplot2)
library(dplyr)
library(waffle)

# current verison
packageVersion("waffle")
## [1] '1.0.1'


# Some new bits up first

data.frame(
  parts = factor(rep(month.abb[1:3], 3), levels=month.abb[1:3]),
  vals = c(10, 20, 30, 6, 14, 40, 30, 20, 10),
  col = rep(c("blue", "black", "red"), 3),
  fct = c(rep("Thing 1", 3),
          rep("Thing 2", 3),
          rep("Thing 3", 3))
) -> xdf

xdf %>%
  count(parts, wt = vals) %>%
  ggplot(aes(fill = parts, values = n)) +
  geom_waffle(n_rows = 20, size = 0.33, colour = "white", flip = TRUE) +
  scale_fill_manual(
    name = NULL,
    values = c("#a40000", "#c68958", "#ae6056"),
    labels = c("Fruit", "Sammich", "Pizza")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle()

xdf %>% 
  ggplot(fill = "parts", values = )


df <- data.frame(
  neden = c("8 İş bulma ümidi olmayanlar", "7 İşbaşı yapabilecek olup iş aramayanlar", 
            "6 İş arayıp işbaşı yapamayacak olanlar", "5 Ev işleriyle meşgul", 
            "4 Eğitim", "3 Emekli", "2 Çalışamaz halde", "1 Diğer"),
  değer = c(17, 16, 1, 9.9, 4.8, 5, 5, 3))

## Örnek ----

df %>% 
  ggplot(aes(fill = neden, values = değer)) +
  geom_waffle(n_rows = 10, size = 0.33, colour = "white", flip = TRUE,
              make_proportional = TRUE) +
  scale_fill_manual(values = c("purple4", "green", "steelblue3", 
                               "darkorange", "pink4", "goldenrod1",
                               "grey30", "black")) +
  # scale_fill_manual(
  #   name = NULL,
  #   values = c("#a40000", "#c68958", "#ae6056"),
  #   labels = c("Fruit", "Sammich", "Pizza")
  # ) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  labs(
    title = "İşgücüne dahil olmama nedenleri",
    subtitle = "TÜİK İşgücü İstatistikleri",
    caption = "https://data.tuik.gov.tr/Bulten/Index?p=Isgucu-Istatistikleri-Subat-2022-45646"
  )

## https://data.tuik.gov.tr/Bulten/Index?p=Isgucu-Istatistikleri-Subat-2022-45646
