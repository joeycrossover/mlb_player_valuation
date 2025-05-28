source("libraries.R")

# the following code performs data cleaning and transformation on
# contract data scraped from Spotrac
# and advanced hitting metrics exported from Baseball Savant
# author: joseph coleman, 5/28/2025

# this file contains contract details for the 
# top 100 contracts (value) signed in the MLB
# generated from 'scrape_contract_data.R'
contracts <- read_xlsx("data/contracts.xlsx")

# this file contains advanced batting statistics and player details
# from 2015 - 2024 for players that had at least 50 at-bats in a season
# it was generated manually and downloaded from baseballsavant.com
stats <- read_csv("data/stats.csv")

# assumption: start by defining the contract year as the year BEFORE the player signed said contract
# as we do not want to consider the player's current year's statistics as a predictor for a contract
# they've already signed
contracts <- contracts %>%
  group_by(player) %>%
  filter(start == max(start, na.rm = TRUE)) %>% # vlad jr has two contracts in here, going to keep the most recent (extension)
  ungroup() %>%
  mutate(contract_year = start - 1)

# since my batter statistics data only goes up to 2024
# if a player signed a contract that starts in 2026 (contract year of 2025)
# change to 2024
contracts$contract_year <- ifelse(contracts$contract_year == 2025, 2024, contracts$contract_year)

# we found rhys hoskins missing in our final data set below
# so coming back here to change his contract year to 2022
# because he didn't play in 2023
contracts$contract_year <- ifelse(contracts$player == "Rhys Hoskins", 2022, contracts$contract_year)

# the contract data we scraped was pulled in as character strings with commas and dollar signs
# so we need to remove those characters and convert the variables to be numeric.
contracts <- contracts %>%
  mutate(
    value = as.numeric(gsub("[$,]", "", value)),
    aav   = as.numeric(gsub("[$,]", "", aav))
  )

# need to standardize the player names in both data sets
# by converting to "first name last name" format
# and all lowercase letters
# while also removing any periods or accents
# we should try to keep suffixes like "jr" or "sr" or "ii" or "iii"
stats <- stats %>%
  mutate(
    full_name = str_to_lower(str_trim(str_replace(stats$`last_name, first_name`, "(.*),\\s*(.*)", "\\2 \\1"))) %>%
      str_remove_all("[\\.'’]") %>%
      str_squish() %>%
      stri_trans_general("Latin-ASCII")
  )

contracts <- contracts %>%
  mutate(
    full_name = str_to_lower(str_trim(player)) %>%
      str_remove_all("[\\.'’]") %>%
      str_squish() %>%
      stri_trans_general("Latin-ASCII")
  )

# are there any players we're missing?
contracts %>% filter(!contracts$full_name %in% stats$full_name) %>% select(full_name) %>% distinct()

# we have "george springer" in contracts, and "george springer iii" in stats
# kristian campbell signed an 8yr/$60M deal with no major league experience
# we have "brent rooker" in contracts, and "brent rooker jr" in stats
stats$full_name <- ifelse(stats$full_name == "george springer iii", 
                          "george springer",
                          ifelse(stats$full_name == "brent rooker jr",
                                 "brent rooker",
                                 stats$full_name))

# now we can merge contract details to yearly player stats using the player name and contract year.
stats_all <- merge(stats, contracts, by.x = c("full_name", "year"), by.y = c("full_name", "contract_year"), all.x = TRUE)
stats_all <- merge(stats_all, contracts %>% select(full_name, contract_year), by = "full_name", all.x = TRUE)

# calculate a hr/ab stat
stats_all$hr_per_ab <- round(stats_all$home_run / stats_all$ab, 4)

# i want to organize the columns a bit better (logically)
# and i want to remove seasons after the player signed their contract
stats_all <- stats_all %>%
  select(4,1,2,24,25,27:32,5,6,7,33,8:21) %>%
  filter(!is.na(contract_year)) %>% # only keep players we have contracts for
  filter(year <= contract_year) %>% # only keep seasons less than or equal to the contract year
  arrange(full_name, year) %>%
  select(-contract_year)

# are there any players we're missing?
contracts %>% filter(!contracts$full_name %in% stats_all$full_name) %>% select(full_name) %>% distinct()

# giancarlo stanton signed in 2015 (contract year 2014)
# jung hoo lee, masataka yoshida, seiya suzuki
# jackson chourio, kristian campbell, luis robert jr, colt keith
# all signed deals prior to any mlb experience
# so our final dataset should have 91 players

# aggregate each player's statistics by year
# first get career totals/averages
career_stats <- stats_all %>%
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
career_stats <- career_stats %>%
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
rookie_stats <- career_stats %>%
  group_by(full_name) %>%
  filter(year == min(year)) %>%
  select(full_name, starts_with("ab"):starts_with("sprint_speed")) %>%
  rename_with(~ paste0("rookie_", .), -full_name)

career_stats <- career_stats %>%
  left_join(rookie_stats, by = "full_name") %>%
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
contract_years <- career_stats %>%
  filter(!is.na(value))

# are there any players we're missing?
contracts %>% filter(!contracts$full_name %in% contract_years$full_name) %>% select(full_name) %>% distinct()

# rhys hoskins signed in 2024 but didn't play in 2023
# so we need to manually change his contract year to 2022
# going to do this up top in my original contracts df

# contract_years can be a good starting point for next steps, as it has a row per contract signed per player
# with the player's contract year statistics as well as career totals and averages
# up to and including the contract year, and first season to contract season changes in statistics.
writexl::write_xlsx(career_stats, "data/career_stats.xlsx")
writexl::write_xlsx(contract_years, "data/contract_year_stats.xlsx")
