source("code/libraries.R")

# the following code performs data exploration on our MLB player valuation data
# author: joseph coleman, 5/28/2025

# load data ----
# load the contract year stats data from '3_scrape_accolades.R'
contract_year_stats <- readxl::read_xlsx("data/contract_year_stats.xlsx")

# explore data ----
# define features
features <- names(contract_year_stats)[11:ncol(contract_year_stats)]

summary_df <- contract_year_stats %>%
  select(all_of(features)) %>%
  summarise(across(everything(), list(
    mean = ~mean(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE),
    cv = ~sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE)
  ))) %>%
  pivot_longer(everything(),
               names_to = c("metric", ".value"),
               names_sep = "_(?=[^_]+$)")

summary_df %>% 
  filter(str_starts(metric, "career_")) %>% 
  print(n = 23)

# metric                         mean        sd     cv
# 1 career_seasons               5.40      2.35   0.436 
# 2 career_ab                 2260.     1129.     0.500 
# 3 career_avg_ab              410.      101.     0.247 
# 4 career_hr                  105.       62.5    0.597 
# 5 career_avg_hr               18.8       8.30   0.442 
# 6 career_hr_per_ab             0.0446    0.0157 0.352 
# 7 career_k_percent            21.6       5.29   0.245 
# 8 career_stolen_bases         43.5      43.8    1.01  
# 9 career_avg_stolen_bases      8.55      7.65   0.895 
# 10 career_games_played        622.      300.     0.482 
# 11 career_avg_games_played    113.       24.4    0.216 
# 12 career_xba                   0.260     0.0222 0.0855
# 13 career_xslg                  0.453     0.0638 0.141 
# 14 career_woba                  0.346     0.0292 0.0845
# 15 career_xwoba                 0.340     0.0329 0.0966
# 16 career_xobp                  0.336     0.0309 0.0920
# 17 career_xiso                  0.193     0.0517 0.268 
# 18 career_exit_velocity_avg    89.6       1.93   0.0215
# 19 career_sweet_spot_percent   34.3       2.68   0.0780
# 20 career_barrel_rate           9.11      3.74   0.410 
# 21 career_hard_hit_percent     40.7       6.61   0.162 
# 22 career_sprint_speed         27.8       1.21   0.0436
# 23 career_avg_war               2.92      1.48   0.506 

# the table above shows (1) the average value for each metric - the mean
# (2) the average distance away, from the average, each value is - the standard deviation
# and (3) the coefficient of variation is the percent variance of the standard deviation to the mean
# quick takeaways/observations:

# are there any patterns in contract terms as the years go on?
# interestingly, contract value has been trending down since 2019
# similarly, contract length is also down
# average contract terms in 2019 were 9.6 yrs/$230M
# average contract terms in 2025 were 4.76 yrs/$123.3M
# however, as far as aav goes, it is roughly the same: $22.2M/yr in 2019, $22.7M/yr in 2025
contract_year_stats %>% group_by(start) %>% summarise(avg_value = mean(value))
contract_year_stats %>% group_by(start) %>% summarise(avg_years = mean(yrs))
contract_year_stats %>% group_by(start) %>% summarise(avg_aav = mean(aav))

contract_summary_long <- contract_year_stats %>%
  group_by(start) %>%
  summarise(
    avg_value = mean(value, na.rm = TRUE) / 1000000,
    avg_years = mean(yrs, na.rm = TRUE),
    avg_aav = mean(aav, na.rm = TRUE) / 1000000
  ) %>%
  pivot_longer(cols = starts_with("avg_"), names_to = "metric", values_to = "average")

contract_summary_long$metric <- recode(contract_summary_long$metric,
                                       avg_value = "Contract Value ($M)",
                                       avg_years = "Contract Length (Years)",
                                       avg_aav = "AAV ($M)")

ggplot(contract_summary_long, aes(x = start, y = average)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkred", size = 2) +
  facet_wrap(~ metric, scales = "free_y") +
  labs(title = "Contract Trends by Start Year",
       x = "Start Year",
       y = "Average") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 12))

# this was pulled manually in April 2025 and will obviously not reflect the player's
# following at the time of signing
# but nonetheless will help us quantify the player's marketability

top_10_ig <- contract_year_stats %>%
  arrange(desc(ig_followers)) %>%
  slice(1:10) %>%
  mutate(
    ig_followers_millions = ig_followers / 1e6,
    full_name = factor(full_name, levels = rev(full_name))
  )

ggplot(top_10_ig, aes(x = full_name, y = ig_followers_millions)) +
  geom_col(fill = "#1DA1F2") +
  geom_text(aes(label = paste0(round(ig_followers_millions, 1), "M")), 
            hjust = -0.1, size = 5) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Winning off the Field",
    subtitle = "Top 10 MLB Players by Instagram Followers",
    x = "",
    y = "Instagram Followers (Millions)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0),
    panel.grid.major.y = element_blank()
  )

ggplot(contract_year_stats, aes(x = ig_followers, y = value / 1e6)) +
  geom_point() +
  scale_x_log10() +
  labs(
    x = "Instagram Followers (log scale)",
    y = "Contract Value (Millions)",
    title = "Do Instagram Followers Influence Contract Size?"
  ) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue", linetype = "dashed") +
  ggrepel::geom_text_repel(
    data = contract_year_stats %>%
      slice_max(order_by = value, n = 4),
    aes(label = full_name),
    size = 3.5,
    box.padding = 0.3,
    point.padding = 0.5,
    segment.color = "gray40"
  ) +
  theme_minimal(base_size = 14)

plot_data <- contract_year_stats %>%
  mutate(aav = aav / 1e6) %>%
  select(player_age, aav, yrs) %>%
  pivot_longer(cols = c(aav, yrs), names_to = "contract_metric", values_to = "value") %>%
  mutate(
    contract_metric = case_when(
      contract_metric == "aav" ~ "Average Annual Value (Millions)",
      contract_metric == "yrs" ~ "Contract Length (Years)"
    )
  )

# Create faceted plot
ggplot(plot_data, aes(x = player_age, y = value)) +
  geom_point(alpha = 0.6, color = "#2c3e50", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "#2980b9", linetype = "dashed", size = 1) +
  facet_wrap(~ contract_metric, scales = "free_y") +
  scale_x_continuous(breaks = seq(20, 40, by = 5)) +
  labs(
    x = "Player Age at Signing",
    y = NULL,
    title = "How Player Age Influences MLB Contract Value and Length"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    strip.text = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggplot(contract_year_stats, aes(x = career_avg_war, y = value / 1e6)) +
  geom_point(alpha = 0.7, color = "#2c3e50", size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, color = "#27ae60", linetype = "dashed", size = 1) +
  labs(
    x = "Career Average WAR per Season",
    y = "Total Contract Value (Millions)",
    title = "The Mike Trout Graph"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  ggrepel::geom_text_repel(
    data = contract_year_stats %>%
      slice_max(order_by = value, n = 4),
    aes(label = full_name),
    size = 3.5,
    box.padding = 0.3,
    point.padding = 0.5,
    segment.color = "gray40"
  )

ig_long <- top_10_ig %>%
  mutate(
    contract_value_millions = value / 1e6
  ) %>%
  select(full_name, ig_followers_millions, contract_value_millions) %>%
  pivot_longer(
    cols = c(ig_followers_millions, contract_value_millions),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(metric,
                    ig_followers_millions = "Instagram Followers (Millions)",
                    contract_value_millions = "Contract Value (Millions)")
  )

# Facet-wrapped lollipop plot
ggplot(ig_long, aes(x = reorder(full_name, value), y = value)) +
  geom_segment(aes(xend = full_name, y = 0, yend = value), 
               color = "skyblue", size = 1) +
  geom_point(color = "steelblue", size = 4) +
  coord_flip() +
  facet_wrap(~ metric, scales = "free_x") +
  labs(
    title = "Off-Field Fame and On-Field Fortune",
    subtitle = "Comparing Instagram Followers and MLB Contract Value",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )

avg_value_by_pos <- contract_year_stats %>%
  group_by(pos) %>%
  summarise(avg_value = mean(value, na.rm = TRUE) / 1e6) %>%
  arrange(desc(avg_value))

ggplot(avg_value_by_pos, aes(x = reorder(pos, avg_value), y = avg_value)) +
  geom_col(fill = "#69b3a2") +
  coord_flip() +
  labs(
    title = "Average MLB Contract Value by Position",
    x = "Position",
    y = "Avg Contract Value (Millions)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0),
    panel.grid.major.y = element_blank()
  )
