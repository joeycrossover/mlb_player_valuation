source("libraries.R")

# the following code uses scraping packages in R to obtain MLB player contract data from  spotrac
# we are omitting all pitcher contracts, we are just looking at position players

headers <- add_headers(
  "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.93 Safari/537.36"
)

url <- paste0("https://www.spotrac.com/mlb/contracts/_/position/b")
page <- GET(url, headers)
html <- read_html(content(page, as = "text"))
contracts <- html %>%
  html_node("table") %>%
  html_table()
Sys.sleep(1)  # be respectful
contracts <- contracts %>% clean_names()
contracts$team_currently_with <- substr(contracts$team_currently_with, 1, 3)
writexl::write_xlsx(contracts, "contracts.xlsx")