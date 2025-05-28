source("libraries.R")

# the following code uses models previously fit
# to project new contracts for upcoming free agents
# author: joseph coleman, 5/28/2025

# this file contains advanced batting statistics and player details
# from 2015 to present (5/15/2025) for players that had at least 10 at-bats in a season
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
career_totals <- new_career_stats %>%
  group_by(full_name) %>%
  filter(year == max(year)) %>%
  ungroup()

# merge instagram followers ----
new_ig_followers <- readxl::read_xlsx("data/new_ig_followers.xlsx")

career_totals <- merge(career_totals,
                       new_ig_followers,
                       by = c("player_id", "full_name"),
                       all.x = TRUE)

# scrape baseball reference for career accolades ----
# define player url using baseball-reference's url naming mechanism
player_urls <- career_totals %>%
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
                                                                                                       player_data$pos))))))))))))

career_totals <- merge(career_totals, player_data, by = c("full_name", "player_id"))
career_totals <- career_totals %>%
  select(-url) %>%
  select(1:3,pos,4:95,97:105)

career_totals <- career_totals %>%
  rename_with(
    .cols = 97:ncol(.),
    .fn = ~ str_replace_all(tolower(.x), "\\s+", "_")
  )

# scrape fangraphs for fWAR ----
# load the fangraphs id workbook 'fangraphs_id.xlsx'
fangraphs_id <- readxl::read_xlsx("data/fangraphs_id.xlsx")
fangraphs_id <- fangraphs_id %>%
  select(MLBID, IDFANGRAPHS) %>%
  filter(MLBID %in% career_totals$player_id) %>%
  distinct(MLBID, .keep_all = TRUE)

# which players are missing?
career_totals %>% filter(!player_id %in% fangraphs_id$MLBID) %>% select(player_id, full_name)

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
                      career_totals %>% select(player_id, full_name),
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
    "&position=all&stats=bat&season=2024&season1=2015&type=0&split=season"
  )

  tryCatch({
    res <- jsonlite::fromJSON(url)
    res$data %>%
      filter(AbbLevel == "MLB", sortSeason >= 2020, sortSeason <= 2025, !is.na(WAR)) %>%
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
fwar_df <- merge(fwar_df, career_totals %>% select(player_id, full_name, year),
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

career_totals <- merge(career_totals,
                       contract_fwar,
                       by.x = c("full_name", "player_id", "year"),
                       by.y = c("full_name" , "mlbid", "season"))

# normalization ----
# define features
# contract_year_stats <- readxl::read_xlsx("data/contract_year_stats2.xlsx")
# contract_year_stats <- fastDummies::dummy_cols(contract_year_stats, select_columns = "pos", remove_first_dummy = TRUE)
# contract_year_stats[, 103:117] <- contract_year_stats[, 103:117] %>%
#   mutate(across(everything(), ~replace_na(., 0)))
contract_year_stats$df <- "original"

career_totals <- fastDummies::dummy_cols(career_totals, select_columns = "pos", remove_first_dummy = TRUE)
career_totals$df <- "new"

all_players <- bind_rows(contract_year_stats, career_totals)

features <- names(all_players)[11:128]

# normalize features using z-score standardization (for modeling)
new_scaled_df <- all_players %>%
  select(all_of(features)) %>%
  replace(is.na(.), 0) %>%
  mutate(across(everything(), ~scale(.)[,1])) # scale() returns a matrix; we grab the vector

# combine normalized features with player info
new_normalized_df <- bind_cols(
  all_players %>% select(129, 1:10),
  new_scaled_df
)

# model with new players ----
# load the normalized contract year stats from '6_mlb_player_valuation.R'
# normalized_df <- readxl::read_xlsx("data/normalized_contract_year_stats.xlsx")

train_data <- normalized_df
test_data <- new_normalized_df %>%
  filter(df == "new") %>%
  select(-df)

# model 1: linear regression with correlated features only
set.seed(123)
new_lmlc_df_train <- train_data %>% select(yrs, all_of(selected_length_features))
new_lmlc <- lm(yrs ~ ., data = new_lmlc_df_train)

new_lmlc_df_test <- test_data %>% select(all_of(selected_length_features))
test_data$lmlc_pred <- predict(new_lmlc, newdata = new_lmlc_df_test)
test_data$lmlc_pred <- pmax(test_data$lmlc_pred, min_years)

new_mlmv_df_train <- train_data %>% select(yrs, value, multivariate_features[multivariate_features %in% names(test_data)])
new_mlmv <- lm(cbind(yrs, value) ~ ., data = new_mlmv_df_train)

new_mlmv_df_test <- test_data %>% select(all_of(multivariate_features[multivariate_features %in% names(test_data)]))
new_mlmv_pred <- predict(new_mlmv, newdata = new_mlmv_df_test)

test_data$mlmv_pred <- new_mlmv_pred[, 2]

test_data$mlmv_pred <- pmax(test_data$mlmv_pred, min_salary)

new_predicted_lmlc <- test_data$lmlc_pred
new_predicted_mlmv <- test_data$mlmv_pred

predicted_aav <- new_predicted_mlmv / new_predicted_lmlc
test_data$predicted_aav <- predicted_aav
new_final_results <- test_data %>% select(1:4, lmlc_pred, mlmv_pred, predicted_aav, multivariate_features[multivariate_features %in% names(test_data)]) %>%
  mutate(mlmv_pred = round(mlmv_pred / 1000000, 1),
         lmlc_pred = round(lmlc_pred),
         predicted_aav = round(predicted_aav / 1000000, 1))


ggplot(new_final_results, aes(x = lmlc_pred, y = mlmv_pred, color = mlmv_pred / lmlc_pred)) +
  geom_point(size = 3, alpha = 0.9) +
  geom_text_repel(
    aes(label = full_name),
    size = 4,
    box.padding = 0.35,
    point.padding = 0.4,
    segment.color = "gray60",
    max.overlaps = 15
  ) +
  colorspace::scale_color_continuous_sequential(name = "AAV", palette = "Batlow") +
  labs(
    x = "Predicted Contract Length (Years)",
    y = "Predicted Contract Value (Millions USD)",
    title = "Projected MLB Contracts: Length vs Value"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 10)),
    legend.position = "right"
  )
