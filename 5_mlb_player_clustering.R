source("libraries.R")

# load the contract year stats (2) data from '3_scrape_accolades.R'
contract_year_stats <- readxl::read_xlsx("contract_year_stats2.xlsx")

# define features
features <- names(contract_year_stats)[11:ncol(contract_year_stats)]

# Normalization ----
# we need to normalize our features using z-score standardization
# this is going to transform each predictor so that it has a mean of 0
# and a standard deviation of 1
# this uses the formula (original value - mean) / standard deviation
# we need to do this because our features are using very different scales
# for example (see table below) we have variables like sprint speed and career_hr, which may be in the tens
# and then we have variables like woba and xslg or xba, which are going to be less than 0.4 most of the time
# without normalization, models like linear regression or k-means clustering could give more weight to features 
# just because they have higher numeric values — not because they're more important

# normalize features using z-score standardization (for modeling)
scaled_df <- contract_year_stats %>%
  select(all_of(features)) %>%
  replace(is.na(.), 0) %>%
  mutate(across(everything(), ~scale(.)[,1])) # scale() returns a matrix; we grab the vector

# combine normalized features with player info
normalized_df <- bind_cols(
  contract_year_stats %>% select(1:10),
  scaled_df
)

writexl::write_xlsx(normalized_df, "normalized_contract_year_stats.xlsx")

# K-Means Clustering ----
# we can try clustering using k-means to see if there are patterns in which player prototypes
# are landing bigger contracts
# k-means clustering is an unsupervised machine learning algorithm used to group similar data points into 'K' distinct clusters
# it tries to partition the data such that the intra-cluster variation (within-cluster sum of squares, or WCSS) is minimized
# think of it as finding 'k' centers (centroids) in your dataset so that each point is assigned to the nearest center
# and the average distance from the points to their center is minimized
# here we'll use the elbow method for choosing k
# which will store the WCSS for 'k' clusters, up to 10 (in this case: see "for (k in 1:10)" line of code)
# the lower the WCSS, the tighter the cluster
set.seed(42)
wcss <- vector()

for (k in 1:10) {
  kmeans_model <- kmeans(scaled_df, centers = k, nstart = 25)
  wcss[k] <- kmeans_model$tot.withinss
}

# plot elbow curve
# the optimal number of clusters is usually the point in the graph
# at which the line creates an angle, like an elbow
# and the values start to decrease more slowly
elbow_df <- data.frame(k = 1:10, wcss = wcss)
ggplot(elbow_df, aes(x = k, y = wcss)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "red", size = 2) +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Elbow Method For Optimal k",
       x = "Number of Clusters (k)",
       y = "Within-Cluster Sum of Squares") +
  theme_minimal(base_size = 14)

# to verify the optimal number of clusters we can add
# a variable to our elbow_df showing the difference in wcss
# from one cluster to the next
# starting from cluster 4 to 5, the variance starts to shrink
elbow_df$wcss_change <- elbow_df$wcss - lag(elbow_df$wcss)
elbow_df$wcss_percent_change <- round((elbow_df$wcss_change / lag(elbow_df$wcss)) * 100, 2)

# optimal number of clusters may be 4 or 5
optimal_k <- 5
kmeans_result <- kmeans(normalized_df[, 11:ncol(normalized_df)], centers = optimal_k)

cluster_df <- contract_year_stats %>%
  mutate(cluster = kmeans_result$cluster) %>%
  select(cluster, everything())

cluster_summary <- cluster_df %>%
  select(cluster, 9:ncol(cluster_df)) %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

cluster_summary2 <- cluster_summary %>%
  select(1:5, 23:24, 26:27, 32, 34:37, 42:43, 96:97, 102:105, 113)

cluster_df %>% group_by(cluster) %>% summarise(count = n())

# based on the cluster summary, we can come up with some player prototypes and create a named vector 
# to map cluster numbers to the labels we come up with
cluster_labels <- c(
  "1" = "all-star veterans",
  "2" = "franchise superstars",
  "3" = "young with upside",
  "4" = "high-floor contributors",
  "5" = "power-first sluggers"
)

cluster_summary$cluster_label <- cluster_labels[as.character(cluster_summary$cluster)]
cluster_summary <- cluster_summary %>% select(1,114,2:113)
cluster_df$cluster_label <- cluster_labels[as.character(cluster_df$cluster)]

ggplot(cluster_df, aes(x = yrs, y = aav, color = as.factor(cluster))) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_viridis_d() +
  labs(title = "Player Clusters by Contract Terms",
       x = "Contract Length (Years)",
       y = "Average Annual Value (AAV)",
       color = "cluster") +
  theme_minimal()

ggplot(cluster_df, aes(x = as.factor(cluster), y = aav)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "AAV by Cluster",
       x = "Cluster",
       y = "Average Annual Value (AAV)") +
  theme_minimal(base_size = 14)

# select top 3 earners per cluster for visualization
top_players <- cluster_df %>%
  filter(!full_name %in% "wander franco") %>%
  group_by(cluster) %>%
  slice_max(order_by = value, n = 3) %>%
  ungroup() %>%
  select(cluster, everything())

# scatter plot
ggplot(top_players, aes(x = yrs, y = value / 1e6, color = cluster_label, label = full_name)) +
  geom_point(size = 4, alpha = 0.8) +
  ggrepel::geom_text_repel(
    size = 3.5,
    show.legend = FALSE,
    max.overlaps = 30,
    box.padding = 0.35,
    point.padding = 0.5,
    segment.color = 'grey50'
  ) +
  labs(
    title = "Top 3 Earners by Cluster",
    x = "Contract Length (Years)",
    y = "Total Contract Value (Millions)",
    color = "Player Cluster"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_brewer(palette = "Set2")

# select top 3 in xWOBA per cluster for visualization
# when we do correlation analysis below, we find out
# career xWOBA is a strong predictor of contract value
# let's compare the top hitters in xWOBA per cluster
# to see if it can highlight any potentially 'undervalued' players
top_xwoba_players <- cluster_df %>%
  filter(!full_name %in% "wander franco") %>% 
  group_by(cluster) %>%
  slice_max(order_by = career_xwoba, n = 3) %>%
  ungroup() %>%
  select(cluster, everything())

# scatter plot
ggplot(top_xwoba_players, aes(x = career_xwoba, y = value / 1e6, color = cluster_label, label = full_name)) +
  geom_point(size = 4, alpha = 0.8) +
  ggrepel::geom_text_repel(
    size = 3.5,
    show.legend = FALSE,
    max.overlaps = 30,
    box.padding = 0.35,
    point.padding = 0.5,
    segment.color = 'grey50'
  ) +
  labs(
    title = "Top 3 xWOBA by Cluster",
    x = "xWOBA",
    y = "Total Contract Value (Millions)",
    color = "Player Cluster"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_brewer(palette = "Set2")

top_players_plot <- top_players %>%
  mutate(plot_type = "Contract Length vs Value")

top_xwoba_plot <- top_xwoba_players %>%
  mutate(plot_type = "xWOBA vs Value")

combined_plot_data <- bind_rows(top_players_plot, top_xwoba_plot)

ggplot(combined_plot_data, aes(
  x = ifelse(plot_type == "Contract Length vs Value", yrs, career_xwoba),
  y = value / 1e6,
  color = cluster_label,
  label = full_name
)) +
  geom_point(size = 4, alpha = 0.8) +
  ggrepel::geom_text_repel(
    size = 3.5,
    show.legend = FALSE,
    max.overlaps = 30,
    box.padding = 0.35,
    point.padding = 0.5,
    segment.color = 'grey50'
  ) +
  labs(
    x = "",
    y = "Total Contract Value (Millions)",
    color = "Player Cluster"
  ) +
  facet_wrap(~ plot_type, scales = "free_x") +
  theme_minimal(base_size = 14) +
  scale_color_brewer(palette = "Set2")

# yordan alvarez seems to be really undervalued
# as does freddie freeman
# interesting to notice bryce harper and corey seager with nearly the same
# career xWOBA and contract value
# but in different clusters...
