# Load libs ---------------------------------------------------------------
library(glue)
library(tidyverse)
library(baseballr)
library(lubridate)
library(here)
library(janitor)
library(ggrepel)
library(ggtext)
library(ggiraph)

# Load data ---------------------------------------------------------------
al <- read_csv(here("data/american-league.csv"))
nl <- read_csv(here("data/national-league.csv"))
teams <- read_csv(here("data/mlb-standings-competition-2023-teams.csv")) %>% 
  clean_names()


# Clean data --------------------------------------------------------------
df <- al %>% 
  bind_rows(nl) %>% 
  rename(wins = team_records_wins, team = team_records_team_name)

df_team_1 <- df %>% 
  left_join(teams %>% 
              select(person, team = team_1)) %>%  
  filter(!is.na(person)) 
  
df_team_2 <- df %>% 
  left_join(teams %>% 
              select(person, team = team_1)) %>%  
  filter(is.na(person)) %>% 
  select(!person) %>% 
  left_join(teams %>% 
              select(person, team = team_2))

df_full <- df_team_1 %>% 
  bind_rows(df_team_2)


# Cumulative wins ---------------------------------------------------------
cum_wins <- df_full %>% 
  group_by(person, date) %>% 
  summarise(wins = sum(wins)) %>% 
  mutate(label = ifelse(date == max(date), 
                        person,
                        NA)) %>% 
  ungroup() %>% 
  mutate(person_fct = fct_reorder(person, wins, max),
         person_fct = fct_rev(person_fct))

cum_wins %>% 
  ggplot(aes(date, wins, color = person_fct)) + 
  geom_line(lwd = 1) + 
  scale_x_date(expand = expansion(mult = c(0,.15)), date_breaks = "1 month",
               date_labels = "%b %d") +
  scale_y_continuous(breaks = seq(0, 200, 25),
                     expand = expansion(mult = c(0,0))) +
  scale_color_manual(values = c("#f44336", "#3f51b5", "#ffeb3b", "#673ab7",
                                "#e81e63", "#2196f3", "#ff9800", "#03a9f4", 
                                "#4caf50", "#00bcd4", "#8bc34a", "#cddc39", 
                                "#9c27b0", "#ffc107", "#009688", "#ff5722")) +
  coord_cartesian(ylim = c(0, 200)) +
  labs(title = "**The 2023 MLB standings competition has become a two person <br>race, with <span style='color:#f44336;'>Alex</span> on top and <span style='color:#3f51b5;'>Jake</span> just behind**",
       subtitle = glue("*Cumulative wins as of {max(cum_wins$date)}*"),
       caption = "Source: baseballr package",
       x = NULL,
       y = NULL) +
  theme_bw(base_size = 12) +
  theme(plot.title.position = "plot",
        plot.title = element_markdown(size = rel(1.5)),
        plot.subtitle = element_markdown(size = rel(1), 
                                         color = "grey30"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey70"),
        legend.title = element_blank(),
        legend.position = c(.93, .539),
        legend.background = element_blank(),
        legend.key = element_blank())

ggsave(here("figures/cumulative-wins.png"), bg = "white", 
       dpi = 600, units = "in", height = 6, width = 8)

# Interactive -------------------------------------------------------------
wide_wins <- cum_wins %>% 
  select(person, date, wins) %>%   
  pivot_wider(names_from = person, values_from = wins) %>% 
  mutate(label_wide = as.character(glue("Alex: {Alex}<br>Anita: {Anita}<br>Brandon: {Brandon}<br>Dan: {Dan}<br>Emily: {Emily}<br>George: {George}<br>Jake: {Jake}<br>Jeremy: {Jeremy}<br>Kevin: {Kevin}<br>Lou: {Lou}<br>Matt: {Matt}<br>Niall: {Niall}<br>Russel: {Russel}<br>Ryan: {Ryan}<br>Vinny: {Vinny}"))) %>% 
  select(date, label_wide)

(cum_wins_plot <- cum_wins %>% 
    left_join(wide_wins %>% 
                select(date, label_wide)) %>% 
    ggplot(aes(date, wins, color = person)) + 
    geom_point_interactive(lwd = 1,
                           aes(tooltip = label_wide)) + 
  scale_x_date(expand = expansion(mult = c(0,.15)), date_breaks = "1 month",
               date_labels = "%b %d") +
  scale_y_continuous(breaks = seq(0, 200, 25),
                     expand = expansion(mult = c(0,0))) +
  scale_color_manual(values = c("#f44336", "#3f51b5", "#ffeb3b", "#673ab7",
                                "#e81e63", "#2196f3", "#ff9800", "#03a9f4", 
                                "#4caf50", "#00bcd4", "#8bc34a", "#cddc39", 
                                "#9c27b0", "#ffc107", "#009688", "#ff5722")) +
  coord_cartesian(ylim = c(0, 200)) +
  labs(title = "**The 2023 MLB standings competition has become a two person <br>race, with <span style='color:#f44336;'>Alex</span> on top and <span style='color:#3f51b5;'>Jake</span> just behind**",
       subtitle = glue("*Cumulative wins as of {max(cum_wins$date)}*"),
       caption = "Source: baseballr package",
       x = NULL,
       y = NULL) +
  theme_bw(base_size = 12) +
  theme(plot.title.position = "plot",
        plot.title = element_markdown(size = rel(1.5)),
        plot.subtitle = element_markdown(size = rel(1), 
                                         color = "grey30"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey70"),
        legend.title = element_blank(),
        legend.position = c(.93, .539),
        legend.background = element_blank(),
        legend.key = element_blank()))

girafe(ggobj = cum_wins_plot, 
       options = list(opts_sizing(rescale = FALSE)))

# Facet Wrap --------------------------------------------------------------
cum_wins

df_full %>% 
  mutate(person_fct = fct_reorder(person, wins, max),
         person_fct = fct_rev(person_fct)) %>% 
  ggplot(aes(date, wins, fill = team)) +
  geom_area() +
  facet_wrap(~person_fct) +
  theme_bw() +
  theme(legend.position = "none")
