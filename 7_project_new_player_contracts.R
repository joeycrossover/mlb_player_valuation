source("code/libraries.R")

# the following code uses models previously fit
# to project new contracts for upcoming free agents
# author: joseph coleman, 5/28/2025

# load data ----
# this file contains advanced batting statistics and player details
# from 2015 to present (6/10/2025) for players that had at least 10 at-bats in a season
# it was generated manually and downloaded from baseballsavant.com
new_stats <- read_csv("data/new_stats.csv")

# repeat the same data preparation steps from 2, 3, and 5 scripts
# aggregate career statistics ----
new_stats <- new_stats %>%
  mutate(
    full_name = str_to_lower(str_trim(str_replace(new_stats$`last_name, first_name`, "(.*),\\s*(.*)", "\\2 \\1"))) %>%
      str_remove_all("[\\.'’]") %>%
      str_squish() %>%
      stri_trans_general("Latin-ASCII")
  )

new_stats$hr_per_ab <- round(new_stats$home_run / new_stats$ab, 4)

new_stats <- new_stats %>%
  select(2,21,3:6,22,7:20) %>%
  arrange(full_name, year)

new_stats <- new_stats %>%
  mutate(
    season_progress = as.numeric(as.Date("2025-07-08") - as.Date("2025-03-27")) / 
      as.numeric(as.Date("2025-09-29") - as.Date("2025-03-27")),
    
    games_on_pace = if_else(year == 2025, round(b_game / season_progress, 0), b_game),
    ab = if_else(year == 2025, round(ab / b_game * games_on_pace, 0), ab),
    home_run = if_else(year == 2025, round(home_run / b_game * games_on_pace, 0), home_run),
    r_total_stolen_base = if_else(year == 2025, round(r_total_stolen_base / b_game * games_on_pace, 0), r_total_stolen_base),
    b_game = if_else(year == 2025, round(b_game / b_game * games_on_pace, 0), b_game)
  ) %>%
  select(-c(season_progress, games_on_pace))

# aggregate each player's statistics by year
# first get career totals/averages
new_career_stats <- new_stats %>%
  arrange(full_name, year) %>%
  group_by(full_name) %>%
  mutate(
    career_seasons = row_number(),
    career_ab = cumsum(ab),
    career_avg_ab = cummean(ab),
    career_hr = cumsum(home_run),
    career_avg_hr = cummean(home_run),
    career_hr_per_ab = cummean(hr_per_ab),
    career_k_percent = cummean(k_percent),
    career_stolen_bases = cumsum(r_total_stolen_base),
    career_avg_stolen_bases = cummean(r_total_stolen_base),
    career_games_played = cumsum(b_game),
    career_avg_games_played = cummean(b_game),
    career_xba = cummean(xba),
    career_xslg = cummean(xslg),
    career_woba = cummean(woba),
    career_xwoba = cummean(xwoba),
    career_xobp = cummean(xobp),
    career_xiso = cummean(xiso),
    career_exit_velocity_avg = cummean(exit_velocity_avg),
    career_sweet_spot_percent = cummean(sweet_spot_percent),
    career_barrel_rate = cummean(barrel_batted_rate),
    career_hard_hit_percent = cummean(hard_hit_percent),
    career_sprint_speed = cummean(sprint_speed)
  ) %>%
  ungroup()

# then get year to year improvement/regression
new_career_stats <- new_career_stats %>%
  arrange(full_name, year) %>%
  group_by(full_name) %>%
  mutate(
    delta_ab = ab - lag(ab),
    delta_hr = home_run - lag(home_run),
    delta_hr_per_ab = hr_per_ab - lag(hr_per_ab),
    delta_k_percent = k_percent - lag(k_percent),
    delta_stolen_bases = r_total_stolen_base - lag(r_total_stolen_base),
    delta_games_played = b_game - lag(b_game),
    delta_xba = xba - lag(xba),
    delta_xslg = xslg - lag(xslg),
    delta_woba = woba - lag(woba),
    delta_xwoba = xwoba - lag(xwoba),
    delta_xobp = xobp - lag(xobp),
    delta_xiso = xiso - lag(xiso),
    delta_exit_velocity_avg = exit_velocity_avg - lag(exit_velocity_avg),
    delta_sweet_spot_percent = sweet_spot_percent - lag(sweet_spot_percent),
    delta_barrel_rate = barrel_batted_rate - lag(barrel_batted_rate),
    delta_hard_hit_percent = hard_hit_percent - lag(hard_hit_percent),
    delta_sprint_speed = sprint_speed - lag(sprint_speed)
  ) %>%
  ungroup()

# get improvement/regression from rookie year to contract year
new_rookie_stats <- new_career_stats %>%
  group_by(full_name) %>%
  filter(year == min(year)) %>%
  select(full_name, starts_with("ab"):starts_with("sprint_speed")) %>%
  rename_with(~ paste0("rookie_", .), -full_name)

new_career_stats <- new_career_stats %>%
  left_join(new_rookie_stats, by = "full_name") %>%
  mutate(
    imp_ab = ab - rookie_ab,
    imp_hr = home_run - rookie_home_run,
    imp_hr_per_ab = hr_per_ab - rookie_hr_per_ab,
    imp_k_percent = k_percent - rookie_k_percent,
    imp_stolen_bases = r_total_stolen_base - rookie_r_total_stolen_base,
    imp_games_played = b_game - rookie_b_game,
    imp_xba = xba - rookie_xba,
    imp_xslg = xslg - rookie_xslg,
    imp_woba = woba - rookie_woba,
    imp_xwoba = xwoba - rookie_xwoba,
    imp_xobp = xobp - rookie_xobp,
    imp_xiso = xiso - rookie_xiso,
    imp_exit_velocity = exit_velocity_avg - rookie_exit_velocity_avg,
    imp_sweet_spot = sweet_spot_percent - rookie_sweet_spot_percent,
    imp_barrel = barrel_batted_rate - rookie_barrel_batted_rate,
    imp_hard_hit = hard_hit_percent - rookie_hard_hit_percent,
    imp_sprint_speed = sprint_speed - rookie_sprint_speed
  )

# set aside a data frame that has only the rows (years) where a player signed a new contract.
new_contract_year_stats <- new_career_stats %>%
  group_by(full_name) %>%
  filter(year == max(year)) %>%
  ungroup()

# merge instagram followers ----
new_ig_followers <- readxl::read_xlsx("data/new_ig_followers.xlsx")

new_contract_year_stats <- merge(new_contract_year_stats,
                                 new_ig_followers,
                                 by = c("player_id", "full_name"),
                                 all.x = TRUE)

# scrape baseball reference for career accolades ----
# define player url using baseball-reference's url naming mechanism
player_urls <- new_contract_year_stats %>%
  mutate(
    clean_name = full_name %>%
      str_remove_all("\\s+(jr|ii|iii|iv)$") %>%         # remove suffixes
      str_remove_all("[\\.'’]") %>%                     # remove apostrophes and periods
      str_squish() %>%
      stri_trans_general("Latin-ASCII"),                # remove accents

    first_name = word(clean_name, 1),
    last_name = word(clean_name, -1),
    first_init = tolower(substr(last_name, 1, 1)),
    slug_last = str_to_lower(str_sub(last_name, 1, 5)),
    slug_first = str_to_lower(str_sub(first_name, 1, 2)),
    url_slug = paste0(slug_last, slug_first, "01"),
    br_url = paste0("https://www.baseball-reference.com/players/", first_init, "/", url_slug, ".shtml")
  )

# had to manually confirm each url and update some of them
player_urls <- player_urls %>%
  mutate(
    url_slug = if_else(
      player_id %in% c(665489, 665487, 677951, 521692, 
                       572233, 621566, 623993, 682998,
                       665161), # after manually confirming url, these id's use 02 instead of 01
      str_replace(url_slug, "01$", "02"),
      if_else(
        player_id %in% c(547180, 621035, 695578, 682985), # and these use 03 instead of 01
        str_replace(url_slug, "01$", "03"),
        if_else(
          player_id %in% c(669257),
          str_replace(url_slug, "01$", "05"),
          if_else(
            player_id %in% c(671739),
            str_replace(url_slug, "01$", "04"),
            url_slug
          )))),
    br_url = paste0("https://www.baseball-reference.com/players/", first_init, "/", url_slug, ".shtml")
  ) %>%
  select(player_id, full_name, br_url, year)

player_urls$br_url <- ifelse(player_urls$full_name == "elly de la cruz",
                             "https://www.baseball-reference.com/players/d/delacel01.shtml",
                             player_urls$br_url)

scrape_player_data <- function(url, player_name, player_id) {
  tryCatch({
    page <- read_html(url)
    
    # Attempt to extract awards
    awards <- page %>%
      html_nodes(xpath = "//ul[@id='bling']/li") %>%
      html_text(trim = TRUE)
    
    # Extract position
    position_text <- page %>%
      html_nodes(xpath = "//div[@id='meta']//p[strong[contains(text(), 'Position')]]") %>%
      html_text(trim = TRUE)
    
    position <- str_remove(position_text, "Position:") %>%
      str_squish()
    
    if (length(awards) == 0) {
      return(tibble(
        full_name = player_name,
        player_id = player_id,
        pos = position,
        url = url
      ))
    }
    
    awards_df <- tibble(raw = awards) %>%
      mutate(
        raw = str_replace(raw, "^\\d{4}\\s+(World Series)$", "\\1"),
        raw = str_replace(raw, "^(AL|NL)CS MVP$", "CS MVP"),
        count = str_extract(raw, "^\\d+(?=x)") %>% replace_na("1") %>% as.numeric(),
        award = str_remove(raw, "^\\d+x\\s*")
      ) %>%
      group_by(award) %>%
      summarise(count = sum(count), .groups = "drop") %>%
      pivot_wider(names_from = award, values_from = count, values_fill = 0)
    
    result <- awards_df %>%
      mutate(
        full_name = player_name,
        player_id = player_id,
        pos = position,
        url = url,
        .before = 1
      )
    
    return(result)
  }, error = function(e) {
    warning(paste("Failed to scrape", player_name, ":", e$message))
    return(NULL)
  })
}

safe_scrape_player_data <- function(...) {
  Sys.sleep(runif(1, 1.5, 3.5))
  scrape_player_data(...)
}

player_data_list <- pmap(
  list(
    url = player_urls$br_url,
    player_name = player_urls$full_name,
    player_id = player_urls$player_id
  ),
  safe_scrape_player_data
)

player_data <- bind_rows(player_data_list)
player_data$pos <- ifelse(player_data$pos == "First Baseman", "1B",
                          ifelse(player_data$pos == "Rightfielder", "RF",
                                 ifelse(player_data$pos == "Shortstop", "SS",
                                        ifelse(player_data$pos == "Leftfielder", "LF",
                                               ifelse(player_data$pos == "Outfielder", "OF",
                                                      ifelse(player_data$pos == "Third Baseman", "3B",
                                                             ifelse(player_data$pos == "Positions: Leftfielder and Designated Hitter", "DH",
                                                                    ifelse(player_data$pos == "Positions: Third Baseman and First Baseman", "3B",
                                                                           ifelse(player_data$pos == "Positions: Shortstop and Centerfielder", "SS",
                                                                                  ifelse(player_data$pos == "Positions: Catcher and Designated Hitter", "C",
                                                                                         ifelse(player_data$pos == "Positions: Second Baseman and Shortstop", "2B",
                                                                                                ifelse(player_data$pos == "Positions: Shortstop and Third Baseman", "SS",
                                                                                                       ifelse(player_data$pos == "Centerfielder", "CF",
                                                                                                              player_data$pos)))))))))))))

new_contract_year_stats <- merge(new_contract_year_stats, player_data, by = c("full_name", "player_id"))
new_contract_year_stats <- new_contract_year_stats %>%
  select(-url) %>%
  select(1:3,pos,4:95,97:105)

new_contract_year_stats <- new_contract_year_stats %>%
  rename_with(
    .cols = 97:ncol(.),
    .fn = ~ str_replace_all(tolower(.x), "\\s+", "_")
  )

# scrape fangraphs for fWAR ----
# load the fangraphs id workbook 'fangraphs_id.xlsx'
fangraphs_id <- readxl::read_xlsx("data/fangraphs_id.xlsx")
fangraphs_id <- fangraphs_id %>%
  select(MLBID, IDFANGRAPHS) %>%
  filter(MLBID %in% new_contract_year_stats$player_id) %>%
  distinct(MLBID, .keep_all = TRUE)

# which players are missing?
new_contract_year_stats %>% filter(!player_id %in% fangraphs_id$MLBID) %>% select(player_id, full_name)

missing_fangraph_ids <- tribble(
  ~MLBID,     ~IDFANGRAPHS,
  682829,     "26668",  # elly de la cruz
  683002,     "26289",  # gunnar henderson
  695578,     "29518",  # james wood
  691406,     "28163",  # junior caminero
  691026,     "27479",  # masyn winn
  694671,     "33333"   # wyatt langford
)

fangraphs_id <- rbind(fangraphs_id, missing_fangraph_ids)
fangraphs_id <- merge(fangraphs_id,
                      new_contract_year_stats %>% select(player_id, full_name),
                      by.x = "MLBID",
                      by.y = "player_id")

# these were wrong
fangraphs_id$IDFANGRAPHS <- ifelse(fangraphs_id$full_name == "steven kwan",
                                   "24610",
                                   ifelse(fangraphs_id$full_name == "cj abrams",
                                          "25768",
                                          ifelse(fangraphs_id$full_name == "jeremy pena",
                                                 "21636",
                                                 ifelse(fangraphs_id$full_name == "adley rutschman",
                                                        "26288",
                                                        ifelse(fangraphs_id$full_name == "bryson stott",
                                                               "26294",
                                                               ifelse(fangraphs_id$full_name == "riley greene",
                                                                      "25976",
                                                                      fangraphs_id$IDFANGRAPHS))))))

get_fwar <- function(fg_id) {
  url <- paste0(
    "https://www.fangraphs.com/api/players/stats?playerid=",
    fg_id,
    "&position=all&stats=bat&season=2025&season1=2015&type=0&split=season"
  )

  tryCatch({
    res <- jsonlite::fromJSON(url)
    res$data %>%
      filter(AbbLevel == "MLB", sortSeason >= 2015, sortSeason <= 2025, !is.na(WAR)) %>%
      select(season = sortSeason, WAR)
  }, error = function(e) {
    warning(paste("Failed to fetch data for player", fg_id, ":", e$message))
    return(tibble(season = integer(), WAR = numeric()))
  })
}

fwar_list <- lapply(fangraphs_id$IDFANGRAPHS, get_fwar)
fwar_df <- bind_rows(fwar_list, .id = "row_index") %>%
  mutate(IDFANGRAPHS = fangraphs_id$IDFANGRAPHS[as.integer(row_index)]) %>%
  select(IDFANGRAPHS, season, WAR)
fwar_df <- merge(fwar_df, fangraphs_id, by = "IDFANGRAPHS")
fwar_df <- merge(fwar_df, new_contract_year_stats %>% select(player_id, full_name, year),
                 by.x = c("MLBID", "full_name"),
                 by.y = c("player_id", "full_name")) %>%
  filter(season <= year)
names(fwar_df) <- tolower(names(fwar_df))

# aggregate each player's statistics by year
# first get career totals/averages
career_fwar <- fwar_df %>%
  group_by(mlbid, full_name, season) %>%
  summarise(season_avg_war = mean(war, na.rm = TRUE), .groups = "drop") %>%
  arrange(mlbid, season) %>%
  group_by(mlbid, full_name) %>%
  mutate(career_avg_war = cummean(season_avg_war)) %>%
  ungroup()

# set aside a data frame that has only the rows (years) where a player signed a new contract.
contract_fwar <- career_fwar %>%
  group_by(mlbid, full_name) %>%
  filter(season == max(season)) %>%
  ungroup()

contract_fwar$season <- as.numeric(contract_fwar$season)

new_contract_year_stats <- merge(new_contract_year_stats,
                                 contract_fwar,
                                 by.x = c("full_name", "player_id", "year"),
                                 by.y = c("full_name" , "mlbid", "season"))

# normalization ----
scaling_means <- readRDS("models/scaling_means.rds")
scaling_sds <- readRDS("models/scaling_sds.rds")
centroids <- readRDS("models/kmeans_centroids.rds")

cluster_features <- colnames(centroids)

missing_features <- setdiff(cluster_features, names(new_contract_year_stats))
if (length(missing_features) > 0) {
  new_contract_year_stats[missing_features] <- 0
  message("Added missing features: ", paste(missing_features, collapse = ", "))
}

new_contract_year_stats <- new_contract_year_stats[, c(names(new_contract_year_stats)[1:4], cluster_features)]

new_features <- names(new_contract_year_stats)[5:ncol(new_contract_year_stats)]

# normalize features using z-score standardization (for modeling)
new_scaled_df <- map_dfc(new_features, function(feature) {
  x <- new_contract_year_stats[[feature]]
  x[is.na(x)] <- 0  # handle NAs first
  
  mean_val <- scaling_means[[feature]]
  sd_val <- scaling_sds[[feature]]
  
  if (is.na(mean_val) || is.na(sd_val) || sd_val == 0) {
    x_scaled <- rep(0, length(x))  # or NA or original value
  } else {
    x_scaled <- (x - mean_val) / sd_val
  }
  
  setNames(as_tibble(x_scaled), feature)
})

# combine normalized features with player info
new_normalized_df <- bind_cols(
  new_contract_year_stats %>% select(1:4),
  new_scaled_df
)

# clustering ----
assign_clusters <- function(new_data, centroids) {
  dist_mat <- as.matrix(dist(rbind(centroids, new_data)))
  dist_to_centroids <- dist_mat[(nrow(centroids) + 1):nrow(dist_mat), 1:nrow(centroids)]
  cluster_assignment <- apply(dist_to_centroids, 1, which.min)
  return(cluster_assignment)
}

new_clusters <- assign_clusters(new_normalized_df[, 5:ncol(new_normalized_df)], centroids)

# Add the cluster assignments to your dataframe
new_cluster_df <- new_contract_year_stats %>%
  mutate(cluster = new_clusters) %>%
  select(cluster, everything())

new_cluster_df %>% group_by(cluster) %>% summarise(count = n())

# based on the cluster summary, we can come up with some player prototypes and create a named vector 
# to map cluster numbers to the labels we come up with
cluster_labels <- c(
  "1" = "all-star veterans",
  "2" = "franchise superstars",
  "3" = "young with upside",
  "4" = "high-floor contributors",
  "5" = "power-first sluggers"
)

new_cluster_df$cluster_label <- cluster_labels[as.character(new_cluster_df$cluster)]

new_normalized_cluster_df <- new_normalized_df %>%
  mutate(cluster = new_clusters) %>%
  select(cluster, everything())

new_normalized_cluster_df$cluster_label <- cluster_labels[as.character(new_normalized_cluster_df$cluster)]

new_cluster_df <- new_cluster_df %>% select(full_name, cluster_label, everything())
new_normalized_cluster_df <- new_normalized_cluster_df %>% select(full_name, cluster_label, everything())

writexl::write_xlsx(new_cluster_df, "data/new_cluster_df.xlsx")
writexl::write_xlsx(new_normalized_cluster_df, "data/new_normalized_cluster_df.xlsx")

# model with new players ----
contract_length_model <- readRDS("models/contract_length_model.rds")
contract_value_model <- readRDS("models/contract_value_model.rds")
selected_value_features <- readRDS("models/selected_value_features.rds")
selected_length_features <- readRDS("models/selected_length_features.rds")
multivariate_features <- unique(c(selected_length_features, selected_value_features))
normalized_cluster_df <- read_xlsx("data/normalized_cluster_df.xlsx")
cluster_df <- read_xlsx("data/cluster_df.xlsx")

cluster_df <- fastDummies::dummy_cols(cluster_df, select_columns = "pos")
cluster_df <- fastDummies::dummy_cols(cluster_df, select_columns = "cluster")
normalized_cluster_df <- fastDummies::dummy_cols(normalized_cluster_df, select_columns = "pos")
normalized_cluster_df <- fastDummies::dummy_cols(normalized_cluster_df, select_columns = "cluster")
new_normalized_cluster_df <- fastDummies::dummy_cols(new_normalized_cluster_df, select_columns = "pos")
new_normalized_cluster_df <- fastDummies::dummy_cols(new_normalized_cluster_df, select_columns = "cluster")
new_cluster_df <- fastDummies::dummy_cols(new_cluster_df, select_columns = "pos")
new_cluster_df <- fastDummies::dummy_cols(new_cluster_df, select_columns = "cluster")

train_long <- normalized_cluster_df %>%
  select(all_of(multivariate_features)) %>%
  mutate(source = "train") %>%
  pivot_longer(cols = -source, names_to = "feature", values_to = "value")

test_long <- new_normalized_cluster_df %>%
  select(all_of(multivariate_features)) %>%
  mutate(source = "new") %>%
  pivot_longer(cols = -source, names_to = "feature", values_to = "value")

combined_long <- bind_rows(train_long, test_long)

# Plot
ggplot(combined_long, aes(x = value, fill = source)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~feature, scales = "free", ncol = 4) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold")) +
  labs(title = "Distribution of Features: Training vs. New Data",
       x = "Normalized Value",
       fill = "Source")

project_spotrac_style <- function(new_player,
                                  comps_df,
                                  age_tolerance = 2,
                                  store_name = NULL,
                                  stat_features = c("career_avg_war", "career_xwoba", "career_woba",
                                               "career_xobp", "career_xslg", "career_avg_hr", "season_avg_war")) {

  important_features <- c("career_avg_war", "career_xwoba", "career_woba")
  weights <- setNames(rep(1, length(stat_features)), stat_features)
  weights[important_features] <- 3

  cluster_val <- new_player$cluster
  pos_val <- new_player$pos
  age_val <- new_player$player_age

  player_comps <- comps_df %>%
    filter(cluster == cluster_val & abs(player_age - age_val) <= age_tolerance)

  if (pos_val %in% c("C", "DH")) {
    player_comps <- player_comps %>% filter(pos == pos_val)
  }

  if (nrow(player_comps) > 5) {
    player_vec <- as.numeric(new_player[, stat_features])
    comp_mat <- player_comps[, stat_features] %>% as.matrix()
    distances <- apply(comp_mat, 1, function(x) sqrt(sum((x - player_vec)^2, na.rm = TRUE)))
    player_comps <- player_comps %>%
      mutate(similarity_dist = distances) %>%
      arrange(similarity_dist) %>%
      slice_head(n = 5)
  }

  if (nrow(player_comps) < 3) {
    for (tol in (age_tolerance + 1):5) {
      player_comps <- comps_df %>%
        filter(cluster == cluster_val & abs(player_age - age_val) <= tol)
      if (nrow(player_comps) >= 3) break
    }
  }

  if (nrow(player_comps) < 3) return(list(proj_yrs = NA, proj_value = NA))

  pct_diffs <- map_dbl(
    stat_features,
    function(f) {
      comp_vals <- player_comps[[f]]
      player_val <- new_player[[f]]
      diffs <- (player_val - comp_vals) / abs(comp_vals)
      mean(diffs, na.rm = TRUE) * weights[[f]]
    }
  ) %>% set_names(stat_features)

  pct_diffs <- pct_diffs[is.finite(pct_diffs)]
  remaining_features <- names(pct_diffs)
  used_weights <- weights[remaining_features]
  avg_change <- weighted.mean(pct_diffs, w = used_weights, na.rm = TRUE)

  base_yrs <- mean(player_comps$yrs, na.rm = TRUE)
  base_val <- mean(player_comps$value, na.rm = TRUE)

  proj_yrs <- round(base_yrs * (1 + avg_change))
  proj_value <- round(base_val * (1 + avg_change), 1)

  if (!is.null(store_name)) {
    comps_out <- player_comps %>%
      select(full_name, player_age, pos, yrs, value, all_of(stat_features))

    comp_avgs <- comps_out %>%
      summarise(across(c(yrs, value, all_of(stat_features)), ~mean(.x, na.rm = TRUE)))

    pct_diffs_row <- map_dbl(
      stat_features,
      function(f) {
        comp_mean <- comp_avgs[[f]]
        player_val <- new_player[[f]]
        if (is.na(comp_mean) || comp_mean == 0) return(NA_real_)
        (player_val - comp_mean) / abs(comp_mean)
      }
    )
    names(pct_diffs_row) <- stat_features

    projection_row <- tibble(
      full_name = "projection/comps",
      player_age = age_val,
      pos = pos_val,
      yrs = proj_yrs,
      value = proj_value
    ) %>%
      bind_cols(as_tibble_row(pct_diffs_row))

    player_row <- new_player %>%
      select(all_of(stat_features)) %>%
      mutate(full_name = store_name,
             player_age = age_val,
             pos = pos_val,
             yrs = NA, value = NA) %>%
      select(full_name, player_age, pos, yrs, value, all_of(stat_features))

    comp_log_tbl <- bind_rows(
      comps_out,
      comp_avgs %>% mutate(full_name = "base/averages", player_age = NA, pos = NA),
      player_row,
      projection_row
    )
    
    comp_log_tbl$aav <- round(comp_log_tbl$value / comp_log_tbl$yrs, 1)
    comp_log_tbl <- comp_log_tbl %>%
      select(full_name, player_age, pos, yrs, value, aav, all_of(stat_features))

    if (!exists("player_comp_tables", envir = .GlobalEnv)) {
      assign("player_comp_tables", list(), envir = .GlobalEnv)
    }
    comp_log <- get("player_comp_tables", envir = .GlobalEnv)
    comp_log[[store_name]] <- comp_log_tbl
    assign("player_comp_tables", comp_log, envir = .GlobalEnv)
  }

  return(list(proj_yrs = proj_yrs, proj_value = proj_value))
}

# Run for all players
results <- map_dfr(
  1:nrow(new_cluster_df),
  function(i) {
    player <- new_cluster_df[i, ]
    res <- project_spotrac_style(
      new_player = player,
      comps_df = cluster_df,
      store_name = player$full_name
    )
    tibble(full_name = player$full_name, proj_yrs = res$proj_yrs, proj_value = res$proj_value)
  }
)

new_cluster_df <- left_join(new_cluster_df, results, by = "full_name") %>%
  mutate(proj_aav = round(proj_value / proj_yrs, 1))

# Write to Excel with formatting
wb <- createWorkbook()
for (name in names(player_comp_tables)) {
  sheet_data <- player_comp_tables[[name]]
  safe_name <- str_trunc(make.names(name), 31)
  addWorksheet(wb, safe_name)
  stat_cols <- which(names(sheet_data) %in% c("career_avg_war", "career_xwoba", "career_woba",
                                              "career_xobp", "career_xslg", "career_avg_hr", "season_avg_war"))
  sheet_data <- sheet_data %>%
    mutate(
      yrs = round(yrs),
      value = round(value / 1e6, 1),
      aav = round(value / yrs, 1),
      across(all_of(stat_cols), ~ round(.x, 3))
    )

  writeData(wb, sheet = safe_name, x = sheet_data)


  for (col in stat_cols) {
    conditionalFormatting(
      wb, sheet = safe_name,
      cols = col, rows = nrow(sheet_data) + 1,
      rule = ">=0", style = createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
    )
    conditionalFormatting(
      wb, sheet = safe_name,
      cols = col, rows = nrow(sheet_data) + 1,
      rule = "<0", style = createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
    )
  }
}

saveWorkbook(wb, "data/player_comps_output.xlsx", overwrite = TRUE)

new_contract_projections <- new_cluster_df %>%
  select(1, 2, 6, proj_yrs, proj_value, proj_aav, career_avg_war, career_xwoba, career_woba,
         career_xobp, career_xslg, career_avg_hr, season_avg_war)

new_contract_projections <- new_contract_projections %>%
  mutate(proj_value = round(proj_value / 1e6, 1),
         proj_aav = round(proj_aav / 1e6, 1))

writexl::write_xlsx(new_contract_projections, "data/new_contract_projections.xlsx")

new_contract_projections %>%
  mutate(full_name = fct_reorder(full_name, proj_value)) %>%
  ggplot(aes(x = full_name, y = proj_value, fill = cluster_label)) +
  geom_col() +
  geom_text(
    aes(label = paste("$", round(proj_value, 1), "M", sep = "")),
    hjust = -0.1, size = 5
  ) +
  coord_flip() +
  labs(
    x = "Player",
    y = "Projected Value (M USD)",
    title = "Projected Value by Player",
    fill = "Cluster"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold")) +
  expand_limits(y = max(new_contract_projections$proj_value) * 1.1)

ggplot(new_contract_projections, aes(x = proj_yrs, y = proj_value, color = proj_aav)) +
  geom_point(size = 5) +
  scale_color_viridis_c(name = "Projected AAV\n($M/year)", option = "C") +
  geom_text(
    aes(label = full_name),
    size = 5,
    hjust = -0.1,
    vjust = 0.5,
    check_overlap = TRUE
  ) +
  scale_x_continuous(
    breaks = seq(0, max(new_contract_projections$proj_yrs, na.rm = TRUE), by = 2)
  ) +
  labs(
    x = "Projected Contract Length (Years)",
    y = "Projected Contract Value ($M)",
    title = "MLB Contract Projections: Length vs. Value",
    subtitle = "Color shows projected AAV (Millions USD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right")

ggplot(new_contract_projections, aes(x = proj_yrs, y = proj_value, color = cluster_label, size = proj_aav)) +
  geom_point(alpha = 1.0) +
  geom_text_repel(aes(label = full_name), size = 5, max.overlaps = 15) +
  scale_size_continuous(name = "AAV ($M/year)") +
  scale_color_brewer(palette = "Set2", name = "Cluster") +
  labs(
    x = "Projected Years",
    y = "Projected Value ($M)",
    title = "Contract Value vs. Length, by Cluster",
    subtitle = "Bubble size shows Average Annual Value (AAV)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right")

spotrac_projections <- read_xlsx("data/spotrac_projections.xlsx")

spotrac_projections <- merge(spotrac_projections, new_contract_projections, by.x = "full_name", all.x = TRUE) %>%
  select(1,5,6,2:4,7:16) %>%
  mutate(spotrac_aav = round(spotrac_aav, 1))

ggplot(spotrac_projections, aes(x = spotrac_value, y = proj_value, label = full_name)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  geom_point(aes(color = cluster_label), size = 5) +
  geom_text_repel(size = 5, max.overlaps = 10) +
  scale_color_brewer(palette = "Set2") +
  labs(
    x = "Spotrac Projected Value (M USD)",
    y = "My Projected Value (M USD)",
    title = "Contract Value: My Model vs. Spotrac",
    subtitle = "Dotted line = perfect agreement"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right")

# plot actuals vs predictions
actual_length <- spotrac_projections$spotrac_yrs
actual_value <- spotrac_projections$spotrac_value

proj_yrs <- spotrac_projections$proj_yrs
proj_value <- spotrac_projections$proj_value

v_residuals <- actual_value - proj_value
classification <- case_when(
  actual_value < 50 ~ ifelse(abs(v_residuals) < 0.5 * actual_value, "accurate",
                             ifelse(v_residuals < 0, "undervalued", "overvalued")),
  actual_value < 150 ~ ifelse(abs(v_residuals) < 0.35 * actual_value, "accurate",
                              ifelse(v_residuals < 0, "undervalued", "overvalued")),
  TRUE ~ ifelse(abs(v_residuals) < 0.25 * actual_value, "accurate",
                ifelse(v_residuals < 0, "undervalued", "overvalued"))
)

spotrac_projections$classification <- classification

writexl::write_xlsx(spotrac_projections, "data/compare_projections_w_spotrac.xlsx")

# top_players <- new_cluster_df %>%
#   arrange(desc(career_xwoba)) %>%
#   slice_head(n = 5)
# 
# top_players_normalized <- new_normalized_cluster_df %>%
#   arrange(desc(career_xwoba)) %>%
#   slice_head(n = 5)
# 
# top_players$proj_yrs <- round(predict(contract_length_model, newdata = top_players_normalized %>% select(all_of(selected_length_features))))
# proj_value <- predict(contract_value_model, newdata = top_players_normalized %>% select(all_of(multivariate_features)))
# top_players$proj_value <- round(proj_value[, 2] /1e6, 1)
# top_players$proj_aav <- round(top_players$proj_value / top_players$proj_yrs, 1)
# 
# new_contract_projections <- top_players %>%
#   select(1, 2, 7, 6, proj_yrs, proj_value, proj_aav, career_avg_war, career_xwoba, career_woba,
#          career_xobp, career_xslg, career_avg_hr, silver_slugger)
# 
# train_long <- normalized_cluster_df %>%
#   select(all_of(multivariate_features)) %>%
#   mutate(source = "train") %>%
#   pivot_longer(cols = -source, names_to = "feature", values_to = "value")
# 
# test_long <- top_players_normalized %>%
#   select(all_of(multivariate_features)) %>%
#   mutate(source = "new") %>%
#   pivot_longer(cols = -source, names_to = "feature", values_to = "value")
# 
# combined_long <- bind_rows(train_long, test_long)
# 
# # Plot
# ggplot(combined_long, aes(x = value, fill = source)) +
#   geom_density(alpha = 0.5) +
#   facet_wrap(~feature, scales = "free", ncol = 4) +
#   theme_minimal() +
#   labs(title = "Distribution of Features: Training vs. New Data",
#        x = "Normalized Value",
#        fill = "Source")
