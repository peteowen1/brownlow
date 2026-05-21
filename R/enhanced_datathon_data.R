#' Enhanced Datathon Data with Umpire and Weather Information
#'
#' Functions to enhance the base datathon dataset with supplementary data from
#' AFLTables (umpire information) and fryzigg (weather data) to improve
#' Brownlow Medal prediction models.

#' Enhanced Datathon Data Loader
#'
#' Loads the datathon dataset and enhances it with umpire and weather data
#'
#' @param file_path Character. Path to datathon CSV file
#' @param seasons Integer vector. Seasons to include (default: all available)
#' @param add_umpires Logical. Add umpire data from afltables (default: TRUE)
#' @param add_weather Logical. Add weather data from fryzigg (default: TRUE)
#' @param cache_dir Character. Directory for caching supplementary data
#' @param progress Logical. Show progress messages (default: TRUE)
#' @param fallback_on_error Logical. Continue on errors (default: TRUE)
#' @param force_reload Logical. Force reload of cached data (default: FALSE)
#'
#' @return List containing enhanced datasets: stats, votes, metadata
#' @export
enhance_datathon_data <- function(file_path = "data/brownlow_datathon_dataset.csv",
                                  seasons = NULL,
                                  add_umpires = TRUE,
                                  add_weather = TRUE,
                                  cache_dir = "data/cache",
                                  progress = TRUE,
                                  fallback_on_error = TRUE,
                                  force_reload = FALSE) {

  if (progress) cat("Loading datathon data...\n")

  # Load base datathon data
  base_data <- load_datathon_data(file_path = file_path, seasons = seasons, progress = progress)

  # Add umpires if requested
  if (add_umpires) {
    if (progress) cat("Adding umpire data...")
    tryCatch({
      umpire_data <- fetch_umpire_data(seasons = base_data$seasons, cache_dir = cache_dir, force_reload = force_reload)
      enhanced_stats <- merge_umpire_data(base_data$stats, umpire_data, progress = FALSE)
      base_data$stats <- enhanced_stats
      if (progress) cat(" ✓\n")
    }, error = function(e) {
      if (fallback_on_error) {
        if (progress) cat(sprintf(" ⚠️ Failed: %s\n", e$message))
      } else {
        stop(sprintf("Umpire data loading failed: %s", e$message))
      }
    })
  }

  # Add weather if requested
  if (add_weather) {
    if (progress) cat("Adding weather data...")
    tryCatch({
      weather_data <- fetch_weather_data(seasons = base_data$seasons, cache_dir = cache_dir, force_reload = force_reload)
      enhanced_stats <- merge_weather_data(base_data$stats, weather_data, progress = FALSE)
      base_data$stats <- enhanced_stats
      if (progress) cat(" ✓\n")
    }, error = function(e) {
      if (fallback_on_error) {
        if (progress) cat(sprintf(" ⚠️ Failed: %s\n", e$message))
      } else {
        stop(sprintf("Weather data loading failed: %s", e$message))
      }
    })
  }

  if (progress) {
    cat(sprintf("✅ Enhanced dataset loaded: %d observations, %d seasons\n",
                nrow(base_data$stats), length(base_data$seasons)))
  }

  return(base_data)
}

#' Standardize Team Names
#'
#' Standardizes AFL team names to consistent format for data merging
#'
#' @param team_name Character. Team name to standardize
#'
#' @return Character. Standardized team name
#' @export
standardize_team_name <- function(team_name) {
  dplyr::case_when(
    team_name == "GWS" ~ "Greater Western Sydney",
    team_name == "Brisbane Lions" ~ "Brisbane",
    team_name == "North Melbourne" ~ "North Melbourne",
    team_name == "Port Adelaide" ~ "Port Adelaide",
    team_name == "St Kilda" ~ "St Kilda",
    team_name == "West Coast" ~ "West Coast",
    team_name == "Western Bulldogs" ~ "Western Bulldogs",
    team_name == "Gold Coast" ~ "Gold Coast",
    TRUE ~ team_name  # Keep other names as-is
  )
}

enhance_datathon_data <- function(file_path = "data/brownlow_datathon_dataset.csv",
                                  seasons = NULL,
                                  add_umpires = TRUE,
                                  add_weather = TRUE,
                                  cache_dir = "data/cache",
                                  progress = TRUE,
                                  fallback_on_error = FALSE,
                                  force_reload = FALSE) {

  if (progress) {
    cat("=== ENHANCING DATATHON DATASET ===\n")
    start_time <- Sys.time()
  }

  # Load base datathon dataset
  if (progress) cat("Loading base datathon dataset...\n")
  base_data <- load_datathon_data(file_path, seasons, clean = TRUE)

  # Create cache directory
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Apply snake_case to base stats immediately
  enhanced_stats <- base_data$stats %>%
    janitor::clean_names() %>%
    dplyr::rename(brownlow_votes = votes) %>%
    dplyr::mutate(player_name = paste(player_first_name, player_last_name))

  # Add umpire data if requested
  if (add_umpires) {
    if (progress) cat("Attempting to fetch umpire data from AFLTables...\n")

    tryCatch({
      umpire_data <- fetch_umpire_data(
        seasons = unique(enhanced_stats$season),
        cache_dir = cache_dir,
        progress = progress,
        force_reload = force_reload
      )

      if (!is.null(umpire_data) && nrow(umpire_data) > 0) {
        enhanced_stats <- merge_umpire_data(enhanced_stats, umpire_data, progress)
        if (progress) cat(sprintf("Added umpire data to %d records\n", nrow(enhanced_stats)))
      } else {
        if (progress) cat("WARNING: No umpire data available\n")
      }

    }, error = function(e) {
      if (progress) {
        cat(sprintf("ERROR: Error fetching umpire data: %s\n", e$message))
        cat("INFO: Continuing without umpire data enhancement\n")
      }
    })
  }

  # Add weather data if requested
  if (add_weather) {
    if (progress) cat("Attempting to fetch weather data from Fryzigg...\n")

    tryCatch({
      weather_data <- fetch_weather_data(
        seasons = unique(enhanced_stats$season),
        cache_dir = cache_dir,
        progress = progress,
        force_reload = force_reload
      )

      if (!is.null(weather_data) && nrow(weather_data) > 0) {
        enhanced_stats <- merge_weather_data(enhanced_stats, weather_data, progress)
        if (progress) cat(sprintf("Added weather data to %d records\n", nrow(enhanced_stats)))
      } else {
        if (progress) cat("WARNING: No weather data available\n")
      }

    }, error = function(e) {
      if (progress) {
        cat(sprintf("ERROR: Error fetching weather data: %s\n", e$message))
        cat("INFO: Continuing without weather data enhancement\n")
      }
    })
  }

  # Update votes dataset to match enhanced stats
  # After janitor::clean_names(), create player_name and standardize brownlow_votes column
  enhanced_votes <- enhanced_stats %>%
    dplyr::mutate(
      # Build player_name from first and last name
      player_name = paste(player_first_name, player_last_name)
    ) %>%
    dplyr::select(any_of(c("season", "date", "round", "player_team",
                          "team", "player_name", "brownlow_votes", "match_id", "player_id"))) %>%
    dplyr::filter(brownlow_votes > 0)

  # Update metadata
  enhanced_metadata <- base_data$metadata
  enhanced_metadata$enhanced <- TRUE
  enhanced_metadata$enhancements <- list(
    umpires = add_umpires,
    weather = add_weather,
    enhanced_date = Sys.time()
  )

  if (progress) {
    total_time <- difftime(Sys.time(), start_time, units = "mins")
    cat(sprintf("\nDataset enhancement completed in %.1f minutes\n", as.numeric(total_time)))
    cat(sprintf("Enhanced dataset: %d records with additional features\n", nrow(enhanced_stats)))
  }

  return(list(
    stats = enhanced_stats,
    votes = enhanced_votes,
    metadata = enhanced_metadata
  ))
}

#' Fetch Umpire Data from AFLTables
#'
#' Fetches umpire information for matches using AFLTables source
#'
#' @param seasons Integer vector. Seasons to fetch
#' @param cache_dir Character. Cache directory
#' @param progress Logical. Show progress
#' @param force_reload Logical. Force reload ignoring cache
#'
#' @return DataFrame with umpire information
fetch_umpire_data <- function(seasons, cache_dir, progress = TRUE, force_reload = FALSE) {

  cache_file <- file.path(cache_dir, sprintf("umpire_data_%s.rds",
                                            paste(range(seasons), collapse = "_")))

  # Check cache first (unless force reload)
  if (file.exists(cache_file) && !force_reload) {
    if (progress) cat("Loading cached umpire data...\n")
    return(readRDS(cache_file))
  } else if (force_reload && progress) {
    cat("Force reloading umpire data (ignoring cache)...\n")
  }

  if (progress) cat("Fetching umpire data from AFLTables...\n")

  all_umpire_data <- list()

  for (season in seasons) {
    if (progress) cat(sprintf("  - Season %d...", season))

    tryCatch({
      # Fetch player stats from afltables which includes umpire info
      season_data <- fitzRoy::fetch_player_stats_afltables(
        season = season,
        # source = "afltables",
        # comp = "AFLM"
      )

      if (nrow(season_data) > 0) {
        # Check if umpire columns exist
        umpire_cols <- names(season_data)[stringr::str_detect(names(season_data), "umpire|Umpire")]

        if (length(umpire_cols) > 0) {
          # Extract unique match-umpire combinations with team information
          umpire_info <- season_data %>%
            dplyr::select(
              Date, Season, Round, Venue,
              Home.team, Away.team,
              all_of(umpire_cols)
            ) %>%
            janitor::clean_names() %>%
            dplyr::distinct()

          if (nrow(umpire_info) > 0) {
            all_umpire_data[[as.character(season)]] <- umpire_info
            if (progress) cat(sprintf(" SUCCESS (%d matches)\n", nrow(umpire_info)))
          } else {
            if (progress) cat(" ERROR (no umpire matches)\n")
          }
        } else {
          if (progress) cat(" ERROR (no umpire columns found)\n")
        }
      } else {
        if (progress) cat(" ERROR (no data)\n")
      }

      # Brief pause to be respectful
      Sys.sleep(0.3)

    }, error = function(e) {
      if (progress) cat(sprintf(" ERROR (error: %s)\n", e$message))
    })
  }

  # Combine all seasons
  if (length(all_umpire_data) > 0) {
    combined_umpire_data <- dplyr::bind_rows(all_umpire_data)

    # Cache the results
    saveRDS(combined_umpire_data, cache_file)

    return(combined_umpire_data)
  } else {
    return(NULL)
  }
}

#' Fetch Weather Data from Fryzigg
#'
#' Fetches weather information for matches using Fryzigg source
#'
#' @param seasons Integer vector. Seasons to fetch
#' @param cache_dir Character. Cache directory
#' @param progress Logical. Show progress
#' @param force_reload Logical. Force reload ignoring cache
#'
#' @return DataFrame with weather information
fetch_weather_data <- function(seasons, cache_dir, progress = TRUE, force_reload = FALSE) {

  cache_file <- file.path(cache_dir, sprintf("weather_data_%s.rds",
                                            paste(range(seasons), collapse = "_")))

  # Check cache first (unless force reload)
  if (file.exists(cache_file) && !force_reload) {
    if (progress) cat("Loading cached weather data...\n")
    return(readRDS(cache_file))
  } else if (force_reload && progress) {
    cat("Force reloading weather data (ignoring cache)...\n")
  }

  if (progress) cat("Fetching weather data from Fryzigg...\n")

  all_weather_data <- list()

  for (season in seasons) {
    if (progress) cat(sprintf("  - Season %d...", season))

    tryCatch({
      # Fetch data from fryzigg source which includes weather
      season_data <- fitzRoy::fetch_player_stats(
        season = season,
        source = "fryzigg",
        comp = "AFLM"
      )

      if (nrow(season_data) > 0) {
        # Check if weather columns exist
        weather_cols <- names(season_data)[stringr::str_detect(names(season_data),
                                          "weather|Weather|temp|Temp|wind|Wind|rain|Rain")]

        if (length(weather_cols) > 0) {
          # Extract unique match-weather combinations with team information
          weather_info <- season_data %>%
            dplyr::select(
              match_date, match_round, venue_name,
              match_home_team, match_away_team,
              all_of(weather_cols)
            ) %>%
            dplyr::mutate(
              season = season  # Add the season from the loop variable
            ) %>%
            janitor::clean_names() %>%
            dplyr::distinct()

          if (nrow(weather_info) > 0) {
            all_weather_data[[as.character(season)]] <- weather_info
            if (progress) cat(sprintf(" SUCCESS (%d matches)\n", nrow(weather_info)))
          } else {
            if (progress) cat(" ERROR (no weather matches)\n")
          }
        } else {
          if (progress) cat(" ERROR (no weather columns found)\n")
        }
      } else {
        if (progress) cat(" ERROR (no data)\n")
      }

      # Brief pause to be respectful
      Sys.sleep(0.3)

    }, error = function(e) {
      if (progress) cat(sprintf(" ERROR (error: %s)\n", e$message))
    })
  }

  # Combine all seasons
  if (length(all_weather_data) > 0) {
    combined_weather_data <- dplyr::bind_rows(all_weather_data)

    # Cache the results
    saveRDS(combined_weather_data, cache_file)

    return(combined_weather_data)
  } else {
    return(NULL)
  }
}

#' Merge Umpire Data with Main Dataset
#'
#' Merges umpire information with the main datathon dataset
#'
#' @param main_data DataFrame. Main datathon dataset
#' @param umpire_data DataFrame. Umpire data to merge
#' @param progress Logical. Show progress
#'
#' @return Enhanced DataFrame with umpire columns
merge_umpire_data <- function(main_data, umpire_data, progress = TRUE) {

  if (is.null(umpire_data) || nrow(umpire_data) == 0) {
    return(main_data)
  }

  # Note: Using shared standardize_team_name utility function

  # Apply snake_case to all column names first
  umpire_clean <- umpire_data %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      date = tryCatch({
        if (inherits(date, "Date")) date else as.Date(date)
      }, error = function(e) {
        # Try different date formats
        tryCatch(as.Date(date, format = "%Y-%m-%d"),
                error = function(e2) as.Date(date, format = "%d/%m/%Y"))
      }),
      season = as.numeric(season),
      # Standardize team names to match main dataset
      home_team = standardize_team_name(home_team),
      away_team = standardize_team_name(away_team),
      # Convert round to numeric where possible
      round_num = as.numeric(ifelse(stringr::str_detect(round, "^\\d+$"), round, NA))
    )

  main_clean <- main_data %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      match_home_team = standardize_team_name(match_home_team),
      match_away_team = standardize_team_name(match_away_team),
    )

  # Primary join using date, season, and both team names
  merged_data <- main_clean %>%
    dplyr::left_join(
      umpire_clean,
      by = c("date", "season",
             "match_home_team" = "home_team",
             "match_away_team" = "away_team"),
      suffix = c("", "_umpire")
    )

  # Check success of primary join
  umpire_cols_check <- names(umpire_clean)[stringr::str_detect(names(umpire_clean), "umpire")]
  primary_matches <- if (length(umpire_cols_check) > 0) {
    sum(!is.na(merged_data[[umpire_cols_check[1]]]))
  } else {
    0
  }

  # If primary join has low success, try date + season only with flexible team matching
  if (primary_matches < nrow(main_clean) * 0.5) {
    if (progress) cat("Primary team join had low success, trying flexible matching...\n")

    merged_data <- main_clean %>%
      dplyr::left_join(
        umpire_clean,
        by = c("date", "season"),
        suffix = c("", "_umpire")
      ) %>%
      # Keep only records where teams match (in either order)
      dplyr::filter(
        (match_home_team == home_team & match_away_team == away_team) |
        (match_home_team == away_team & match_away_team == home_team) |
        is.na(home_team)  # Keep unmatched records from main data
      ) %>%
      # Remove duplicate columns
      dplyr::select(-any_of(c("round_umpire", "home_team", "away_team", "round_num")))
  } else {
    # Remove duplicate columns from successful primary join
    merged_data <- merged_data %>%
      dplyr::select(-any_of(c("round_umpire", "round_num")))
  }

  # Report merge success
  if (progress) {
    # Find the first umpire column for match rate calculation
    umpire_cols <- names(merged_data)[stringr::str_detect(names(merged_data), "umpire")]
    if (length(umpire_cols) > 0) {
      n_matched <- sum(!is.na(merged_data[[umpire_cols[1]]]))
      match_rate <- round(n_matched / nrow(merged_data) * 100, 1)
      cat(sprintf("Umpire data match rate: %.1f%% (%d/%d records)\n",
                  match_rate, n_matched, nrow(merged_data)))
    } else {
      cat("No umpire columns found in merged data\n")
    }
  }

  return(merged_data)
}

#' Merge Weather Data with Main Dataset
#'
#' Merges weather information with the main datathon dataset
#'
#' @param main_data DataFrame. Main datathon dataset
#' @param weather_data DataFrame. Weather data to merge
#' @param progress Logical. Show progress
#'
#' @return Enhanced DataFrame with weather columns
merge_weather_data <- function(main_data, weather_data, progress = TRUE) {

  if (is.null(weather_data) || nrow(weather_data) == 0) {
    return(main_data)
  }

  # Note: Using shared standardize_team_name utility function

  # Apply snake_case to all column names first
  weather_clean <- weather_data %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      match_date = tryCatch({
        if (inherits(match_date, "Date")) match_date else as.Date(match_date)
      }, error = function(e) {
        # Try different date formats
        tryCatch(as.Date(match_date, format = "%Y-%m-%d"),
                error = function(e2) as.Date(match_date, format = "%d/%m/%Y"))
      }),
      season = as.numeric(season)
    ) %>%
    # Clean venue names for better matching
    dplyr::mutate(
      venue_clean = stringr::str_trim(stringr::str_to_lower(venue_name)),
      match_home_team = standardize_team_name(match_home_team),
      match_away_team = standardize_team_name(match_away_team)
    )

  main_clean <- main_data %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      venue_clean = stringr::str_trim(stringr::str_to_lower(venue_name)),
      # Ensure date is properly formatted
      date = tryCatch({
        if (inherits(date, "Date")) date else as.Date(date)
      }, error = function(e) {
        # Try different date formats
        tryCatch(as.Date(date, format = "%Y-%m-%d"),
                error = function(e2) as.Date(date, format = "%d/%m/%Y"))
      })
    ) %>%
    dplyr::mutate(
      match_home_team = standardize_team_name(match_home_team),
      match_away_team = standardize_team_name(match_away_team),
    )

  # Primary merge on date, season, and team matchup
  merged_data <- main_clean %>%
    dplyr::left_join(
      weather_clean,
      by = c("date" = "match_date",
             "season",
             "match_home_team",
             "match_away_team"),
      suffix = c("", "_weather")
    ) %>%
    # Remove temporary columns and duplicates
    dplyr::select(-venue_clean, -any_of(c("venue_weather", "round_weather", "roun")))

  # Create derived weather categories if we have weather data
  if (any(stringr::str_detect(names(merged_data), "temp|weather|rain|wind"))) {
    merged_data <- merged_data %>%
      dplyr::mutate(
        weather_category = "Unknown"  # Will be enhanced based on actual columns found
      )
  }

  # Report merge success
  if (progress) {
    weather_cols <- names(merged_data)[stringr::str_detect(names(merged_data), "match_weather")]
    if (length(weather_cols) > 0) {
      n_matched <- sum(!is.na(merged_data[[weather_cols[1]]]))
      match_rate <- round(n_matched / nrow(merged_data) * 100, 1)
      cat(sprintf("Weather data match rate: %.1f%% (%d/%d records)\n",
                  match_rate, n_matched, nrow(merged_data)))
    } else {
      cat("No weather columns found in merged data\n")
    }
  }

  return(merged_data)
}
