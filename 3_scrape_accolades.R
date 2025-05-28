source("libraries.R")

# the following code merges instagram followers for each player and
# scrapes each player's career accolades from Baseball-Reference
# and fWAR from FanGraphs
# the instagram followers were collected manually
# author: joseph coleman, 5/28/2025

# load the contract year stats data from '2_mlb_prep_data.R'
contract_year_stats <- readxl::read_xlsx("data/contract_year_stats.xlsx")

# load instagram follower counts workbook
ig_followers <- readxl::read_xlsx("data/ig_followers.xlsx")

contract_year_stats <- merge(contract_year_stats, 
                             ig_followers, 
                             by = c("player_id", "full_name"),
                             all.x = TRUE)

# scrape baseball reference for career accolades
# define player url using baseball-reference's url naming mechanism
player_urls <- contract_year_stats %>%
  mutate(
    clean_name = full_name %>%
      str_remove_all("\\s+(jr|ii|iii|iv)$") %>%         # remove suffixes
      str_remove_all("[\\.'’]") %>%                        # remove apostrophes and periods
      str_squish() %>%
      stri_trans_general("Latin-ASCII"),                   # remove accents
    
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
      player_id %in% c(665489, 665487, 677951, 521692, 572233, 621566, 623993, 682998), # after manually confirming url, these id's use 02 instead of 01
      str_replace(url_slug, "01$", "02"),
      if_else(
        player_id %in% c(547180, 621035), # and these use 03 instead of 01
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

# this isn't perfect but it'll have to do
# i can't get all the awards up to the contract year
# so if i can't, i'll just have to take the total
scrape_player_data <- function(url, player_name, player_id, contract_season) {
  tryCatch({
    page <- read_html(url)
    
    awards <- page %>%
      html_nodes(xpath = "//ul[@id='bling']/li") %>%
      html_text(trim = TRUE)
    
    if (length(awards) == 0) {
      return(tibble(
        full_name = player_name,
        player_id = player_id,
        url = url
      ))
    }
    
    awards_df <- tibble(raw = awards) %>%
      mutate(
        year = str_extract(raw, "^\\d{4}") %>% as.numeric(),
        raw = str_replace(raw, "^\\d{4}\\s+(World Series)$", "\\1"),
        raw = str_replace(raw, "^(AL|NL)CS MVP$", "CS MVP"),
        count = str_extract(raw, "^\\d+(?=x)") %>% replace_na("1") %>% as.numeric(),
        award = str_remove(raw, "^\\d+x\\s*")
      ) %>%
      # Only keep awards with year <= contract_season OR no year at all
      filter(is.na(year) | year <= contract_season) %>%
      group_by(award) %>%
      summarise(count = sum(count), .groups = "drop") %>%
      pivot_wider(names_from = award, values_from = count, values_fill = 0)
    
    result <- awards_df %>%
      mutate(
        full_name = player_name,
        player_id = player_id,
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
    player_id = player_urls$player_id,
    contract_season = player_urls$year
  ),
  safe_scrape_player_data
)

player_data <- bind_rows(player_data_list)

contract_year_stats <- merge(contract_year_stats, player_data, by = c("full_name", "player_id"))
contract_year_stats <- contract_year_stats %>%
  select(-url)

contract_year_stats <- contract_year_stats %>%
  rename_with(
    .cols = 103:ncol(.),
    .fn = ~ str_replace_all(tolower(.x), "\\s+", "_")
  )

# load the fangraphs id workbook 'fangraphs_id.xlsx'
fangraphs_id <- readxl::read_xlsx("fangraphs_id.xlsx")
fangraphs_id <- fangraphs_id %>%
  select(MLBID, IDFANGRAPHS) %>%
  filter(MLBID %in% contract_year_stats$player_id) %>%
  distinct(MLBID, .keep_all = TRUE)

# which players are missing?
contract_year_stats %>% filter(!player_id %in% fangraphs_id$MLBID) %>% select(player_id, full_name)

# the fangraph player id's were pulled from smartfantasybaseball.com
# the following id's/players were not in the workbook, so i'm just going
# to add them manually and look up the fangraphs id myself
missing_fangraph_ids <- tribble(
  ~MLBID,     ~IDFANGRAPHS,
  678882,     "24262" , # ceddanne rafaela
  682998,     "25878",  # corbin carroll
  678662,     "24064",  # ezequiel tovar
  672695,     "22799",  # geraldo perdomo
  701538,     "29490",  # jackson merrill
  671732,     "22542",  # lawrence butler
  671739,     "25931"   # michael harris ii
)

fangraphs_id <- rbind(fangraphs_id, missing_fangraph_ids)
fangraphs_id <- merge(fangraphs_id, 
                      contract_year_stats %>% select(player_id, full_name), 
                      by.x = "MLBID", 
                      by.y = "player_id")

# these were wrong
fangraphs_id$IDFANGRAPHS <- ifelse(fangraphs_id$full_name == "bobby witt jr", 
                                   "25764", 
                                   ifelse(fangraphs_id$full_name == "julio rodriguez",
                                          "23697",
                                          fangraphs_id$IDFANGRAPHS))

get_fwar <- function(fg_id) {
  url <- paste0(
    "https://www.fangraphs.com/api/players/stats?playerid=",
    fg_id,
    "&position=all&stats=bat&season=2024&season1=2015&type=0&split=season"
  )
  
  tryCatch({
    res <- jsonlite::fromJSON(url)
    res$data %>%
      filter(AbbLevel == "MLB", sortSeason >= 2015, sortSeason <= 2024, !is.na(WAR)) %>%
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
fwar_df <- merge(fwar_df, contract_year_stats %>% select(player_id, full_name, year),
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

contract_year_stats <- merge(contract_year_stats, 
                             contract_fwar, 
                             by.x = c("full_name", "player_id", "year"),
                             by.y = c("full_name" , "mlbid", "season"))

writexl::write_xlsx(contract_year_stats, "data/contract_year_stats2.xlsx")
