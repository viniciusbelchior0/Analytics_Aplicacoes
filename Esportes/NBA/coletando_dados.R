#coletar dados NBA
devtools::install_github("abresler/nbastatR")
library(nbastatR)


# Boxscore Players
boxscore_players <- game_logs(seasons = 1947:2021, result_types = "player",
                              season_types = c("Regular Season","Playoffs"))

# Boxscore Teams
boxscore_teams <- game_logs(seasons = 1947:2021, result_types = "team",
                            season_types = c("Regular Season","Playoffs"))


# Fonte dos dados(game_logs): Site Oficial da NBA
# https://www.nba.com/stats/players/boxscores/