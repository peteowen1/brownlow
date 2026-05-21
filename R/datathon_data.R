#' Load Brownlow Datathon Dataset
#' 
#' Loads and cleans the official Brownlow datathon dataset which contains
#' comprehensive AFL statistics with actual Brownlow votes.
#' 
#' @param file_path Character. Path to datathon CSV file 
#' @param seasons Integer vector. Seasons to include (default: all available)
#' @param clean Logical. Apply basic data cleaning (default: TRUE)
#' 
#' @return List containing cleaned datasets: stats, votes, metadata
#' @export
load_datathon_data <- function(file_path = "data/brownlow_datathon_dataset.csv",
                               seasons = NULL,
                               clean = TRUE) {
  
  if (!file.exists(file_path)) {
    stop("Datathon dataset not found at: ", file_path, 
         "\nPlease ensure the CSV file is in the correct location.")
  }
  
  cat("Loading Brownlow datathon dataset...\n")
  
  # Load the dataset
  raw_data <- readr::read_csv(file_path, show_col_types = FALSE)
  
  cat(sprintf("Loaded %d records\n", nrow(raw_data)))
  
  # Filter by seasons if specified
  if (!is.null(seasons)) {
    raw_data <- raw_data %>%
      dplyr::filter(season %in% seasons)
    cat(sprintf("Filtered to %d records for seasons: %s\n", 
                nrow(raw_data), paste(seasons, collapse = ", ")))
  }
  
  if (clean) {
    cat("Applying basic data cleaning...\n")
    
    # Basic cleaning
    cleaned_data <- raw_data %>%
      dplyr::mutate(
        # Standardize date
        match_date = as.Date(match_date, format = "%d/%m/%Y"),
        
        # Clean player names
        player_name = paste(player_first_name, player_last_name),
        
        # Handle missing values in key numeric columns
        across(c(kicks, handballs, disposals, goals, marks, tackles, 
                 brownlow_votes, supercoach_score, afl_fantasy_score), 
               ~replace_na(.x, 0)),
        
        # Create useful derived variables
        total_score = home_final_score + away_final_score,
        win_margin = ifelse(match_winner == player_team, match_margin, -match_margin),
        
        # Position indicators (already exist but ensure they're logical)
        is_midfielder = as.logical(is_midfielder),
        is_forward = as.logical(is_forward), 
        is_back = as.logical(is_back)
      ) %>%
      # Remove any completely empty rows
      dplyr::filter(!is.na(player_id))
    
  } else {
    cleaned_data <- raw_data
  }
  
  # Create separate votes dataset
  votes_data <- cleaned_data %>%
    dplyr::select(
      season, match_date, match_round, player_team, player_name, 
      brownlow_votes, match_id, player_id
    ) %>%
    dplyr::rename(
      Season = season,
      Date = match_date,
      Round = match_round,
      Team = player_team,
      Player = player_name,
      Votes = brownlow_votes
    )
  
  # Main stats dataset
  stats_data <- cleaned_data %>%
    dplyr::rename(
      Season = season,
      Date = match_date,
      Round = match_round,  
      Team = player_team,
      Player = player_name,
      Votes = brownlow_votes
    )
  
  # Create metadata
  available_seasons <- sort(unique(cleaned_data$season))
  
  metadata <- list(
    seasons = available_seasons,
    n_matches = length(unique(cleaned_data$match_id)),
    n_players = length(unique(cleaned_data$player_id)),
    n_records = nrow(cleaned_data),
    n_vote_records = sum(cleaned_data$brownlow_votes > 0),
    date_range = range(cleaned_data$match_date, na.rm = TRUE),
    venues = sort(unique(cleaned_data$venue_name)),
    teams = sort(unique(cleaned_data$player_team)),
    source = "datathon_dataset",
    created = Sys.time()
  )
  
  cat("\nDatathon Dataset Summary:\n")
  cat(sprintf("- Seasons: %s\n", paste(available_seasons, collapse = ", ")))
  cat(sprintf("- Matches: %d\n", metadata$n_matches))
  cat(sprintf("- Players: %d\n", metadata$n_players))
  cat(sprintf("- Total records: %d\n", metadata$n_records))
  cat(sprintf("- Vote records: %d\n", metadata$n_vote_records))
  cat(sprintf("- Date range: %s to %s\n", metadata$date_range[1], metadata$date_range[2]))
  
  return(list(
    stats = stats_data,
    votes = votes_data,  
    metadata = metadata
  ))
}

#' Get Datathon Dataset Column Info
#' 
#' Returns information about available columns in the datathon dataset
#' 
#' @return DataFrame with column descriptions
#' @export
get_datathon_columns <- function() {
  
  columns_info <- tibble::tribble(
    ~column, ~type, ~description,
    "brownlow_votes", "target", "Brownlow Medal votes (0-3) - TARGET VARIABLE",
    "kicks", "stat", "Number of kicks",
    "handballs", "stat", "Number of handballs", 
    "disposals", "stat", "Total disposals (kicks + handballs)",
    "goals", "stat", "Goals scored",
    "behinds", "stat", "Behinds scored",
    "marks", "stat", "Total marks",
    "tackles", "stat", "Number of tackles",
    "contested_possessions", "stat", "Contested possessions",
    "uncontested_possessions", "stat", "Uncontested possessions",
    "supercoach_score", "derived", "SuperCoach fantasy points",
    "afl_fantasy_score", "derived", "AFL Fantasy points",
    "rating_points", "derived", "Official AFL player rating",
    "player_position", "context", "Playing position",
    "is_midfielder", "context", "Boolean: plays midfield",
    "is_forward", "context", "Boolean: plays forward", 
    "is_back", "context", "Boolean: plays defence",
    "home_advantage", "context", "Boolean: playing at home",
    "player_age", "context", "Player age",
    "player_career_games", "context", "Career games played",
    "venue_name", "context", "Match venue",
    "match_round", "context", "Round number",
    "season", "context", "AFL season year"
  )
  
  return(columns_info)
}