# installing package

install.packages("ggchicklet", repos = "https://cinc.rud.is")
# or
remotes::install_git("https://git.rud.is/hrbrmstr/ggchicklet.git")
# or
remotes::install_git("https://git.sr.ht/~hrbrmstr/ggchicklet")
# or
remotes::install_gitlab("hrbrmstr/ggchicklet")
# or
remotes::install_bitbucket("hrbrmstr/ggchicklet")
# or
remotes::install_github("hrbrmstr/ggchicklet")

#loading packages

library(ggchicklet)

# current version
packageVersion("ggchicklet")
## [1] '0.5.2'

library(hrbrthemes)
library(tidyverse)

data("debates2019")

# Example 1.

debates2019 %>%
  filter(debate_group == 1) %>% 
  mutate(speaker = fct_reorder(speaker, elapsed, sum, .desc=FALSE)) %>%
  mutate(topic = fct_other(
    topic,
    c("Immigration", "Economy", "Climate Change", "Gun Control", "Healthcare", "Foreign Policy"))
  ) %>%
  ggplot(aes(speaker, elapsed, group = timestamp, fill = topic)) +
  geom_chicklet(width = 0.75) +
  scale_y_continuous(
    expand = c(0, 0.0625),
    position = "right",
    breaks = seq(0, 14, 2),
    labels = c(0, sprintf("%d min.", seq(2, 14, 2)))
  ) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Immigration" = "#ae4544",
      "Economy" = "#d8cb98",
      "Climate Change" = "#a4ad6f",
      "Gun Control" = "#cc7c3a",
      "Healthcare" = "#436f82",
      "Foreign Policy" = "#7c5981",
      "Other" = "#cccccc"
    ),
    breaks = setdiff(unique(debates2019$topic), "Other")
  ) +
  guides(
    fill = guide_legend(nrow = 1)
  ) +
  coord_flip() +
  labs(
    x = NULL, y = NULL, fill = NULL,
    title = "How Long Each Candidate Spoke",
    subtitle = "Nights 1 & 2 of the June 2019 Democratic Debates",
    caption = "Each bar segment represents the length of a candidate’s response to a question.\n\nOriginals <https://www.nytimes.com/interactive/2019/admin/100000006581096.embedded.html?>\n<https://www.nytimes.com/interactive/2019/admin/100000006584572.embedded.html?>\nby @nytimes Weiyi Cai, Jason Kao, Jasmine C. Lee, Alicia Parlapiano and Jugal K. Patel\n\n#rstats reproduction by @hrbrmstr"
  ) +
  theme_ipsum_rc(grid="X") +
  theme(axis.text.x = element_text(color = "gray60", size = 10)) +
  theme(legend.position = "top")

# Example 2

pacman::p_load(tidyverse, ggchicklet, extrafont, hrbrthemes, ggtext, grid)
extrafont::loadfonts(device = 'win',  quiet = TRUE)

dt_rand <- tibble(
  Sport = c("NFL", "NFL", "NFL", "MLB", "MLB", "MLB", "NBA", "NBA",
            "NBA", "NHL", "NHL", "NHL", "EPL", "EPL", "EPL"),
  Type = c("Game Action", "Nonaction", "Commercials", "Game Action", 
           "Nonaction", "Commercials", "Game Action", "Nonaction", "Commercials", 
           "Game Action", "Nonaction", "Commercials", "Game Action", "Nonaction", 
           "Commercials"),
  Time = c(18, 140.6, 49.9, 22.5, 150.9, 51.8, 49.6, 61.8,
           33.5, 63, 56.6, 37.4, 58.7, 47.8, 10.1)
)


p <- dt_rand %>% 
  group_by(Sport) %>% mutate(Percent = Time/sum(Time)) %>% ungroup() %>% 
  mutate(Sport = fct_relevel(
    Sport,
    rev(c("NFL", "MLB", "NBA", "NHL", "EPL")))
  ) %>% 
  mutate(Type = fct_relevel(
    Type,
    c("Commercials","Nonaction","Game Action"))) %>% 
  ggplot(aes(Sport, Percent, label = Time)) +
  geom_chicklet(aes(fill = Type), position = ggplot2::position_fill()) +
  scale_fill_manual(values = c("#222831", "#30475e", "#f05454")) +
  coord_flip() +
  annotate(
    geom = "curve", xend = .5, yend = .05, x = 0, y = .12,
    curvature = -.3,
    size = .7,
    color = "black",
    arrow = arrow(type = "open", length = unit(.5, "lines")),
  ) +
  annotate(
    geom = "richtext", x = 0, y = .45, 
    size = 4,
    label.color = NA,
    fill = NA,
    label = "<span style='font-size:13pt;'>The average share of broadcast time showing <strong style='color:#FA759F'>GAME ACTION</strong> is highest in<br>the English Premier League - but there is more total action in an average<br>National Hockey League game, which lasts longer.</span>"
  ) +
  labs(
    title = "<b>NFL and MLB games are long, slow affairs</b>",
    subtitle = "Minutes by broadcast by what is shown on screen across five major men's sports leagues"
  ) + theme_minimal() +
  theme(
    legend.position = "top", 
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_markdown(hjust = .5, size = 18),
    plot.subtitle = element_markdown(hjust = .5),
    plot.margin = margin(1,1,2,1, unit = "lines")
  ) 


gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

# Example 3

library(tidyverse)

AMSsurvey <- read.csv("AMSsurvey.csv")

dt_AMS <- AMSsurvey %>%
  group_by(type, sex) %>%                        
  summarise(counted = sum(count)) %>% 
  ungroup() %>% 
  mutate(type = fct_relevel(
  type, 
  rev(c("I(Pu)", "I(Pr)", "II", "III", "IV", "Va")))
  ) %>% 
  mutate(sex = fct_relevel(
    sex,
    c("Male", "Female"))) %>% 
  ggplot(aes(type, counted, label = sex)) +
  geom_chicklet(aes(fill = sex), position = ggplot2::position_fill()) +
  scale_fill_manual(values = c("#222831", "#30475e")) +
  coord_flip()


