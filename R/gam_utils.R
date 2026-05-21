#' GAM Utility Functions for Brownlow Medal Prediction
#'
#' This file contains utility functions for GAM data preparation, model building,
#' and prediction conversion specifically for Brownlow Medal prediction using mgcv.

#' Prepare GAM Data
#'
#' Prepares training and test data for GAM modeling with proper factor handling,
#' player lumping, and target variable conversion for ordered categorical models
#'
#' @param train_data Training data
#' @param validation_data Validation data (combined with training)
#' @param test_data Test data
#' @param target_col Name of target column
#' @param feature_cols Vector of feature column names to use
#' @param top_n_players Number of top players to keep (default: 300)
#' @param min_player_obs Minimum observations per player (default: 10)
#' @param min_team_obs Minimum observations per team (default: 50)
#'
#' @return List with prepared training and test data
#' @export
prepare_gam_data <- function(train_data, 
                             validation_data = NULL,
                             test_data,
                             target_col,
                             feature_cols,
                             top_n_players = 300,
                             min_player_obs = 10,
                             min_team_obs = 50) {
  
  
  # Combine training and validation data if provided
  if (!is.null(validation_data)) {
    all_training_data <- dplyr::bind_rows(train_data, validation_data)
  } else {
    all_training_data <- train_data
  }
  
  # Add umpire features if available
  if (exists("create_umpire_features")) {
    cat("Creating refined umpire features...\n")
    all_training_data <- create_umpire_features(all_training_data, top_n = 20)
    test_data <- create_umpire_features(test_data, top_n = 20)
  }
  
  # Find available features
  available_features <- intersect(feature_cols, names(all_training_data))
  cat(sprintf("Using %d features for GAM model\n", length(available_features)))
  
  # Prepare training data
  gam_data_train <- all_training_data %>%
    dplyr::select(dplyr::all_of(c(target_col, "player_name", "team", "season", available_features)),
           dplyr::any_of(c("match_id", "match_round", "round", "date"))) %>%
    # Convert categorical variables to factors
    dplyr::mutate(
      player_name = as.factor(player_name),
      team = as.factor(team),
      season = as.factor(season),
      match_id = if("match_id" %in% names(.)) as.factor(match_id) else NULL
    ) %>%
    # Remove players with very few observations (for stable random effects)
    dplyr::group_by(player_name) %>%
    dplyr::filter(dplyr::n() >= min_player_obs) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(team) %>%
    dplyr::filter(dplyr::n() >= min_team_obs) %>%
    dplyr::ungroup() %>%
    # Remove missing values
    dplyr::filter(stats::complete.cases(.))
  
  # Prepare test data
  gam_data_test <- test_data %>%
    dplyr::select(dplyr::all_of(c(target_col, "player_name", "team", "season", available_features)),
           dplyr::any_of(c("match_id", "match_round", "round", "date"))) %>%
    dplyr::mutate(
      player_name = as.factor(player_name),
      team = as.factor(team),
      season = as.factor(season),
      match_id = if("match_id" %in% names(.)) as.factor(match_id) else NULL
    ) %>%
    dplyr::filter(stats::complete.cases(.))
  
  # Optimize player factors using fct_lump to keep top N players
  cat(sprintf("Lumping players to top %d most frequent...\n", top_n_players))
  
  # Lump players in training data
  gam_data_train <- gam_data_train %>%
    dplyr::mutate(
      player_name = forcats::fct_lump_n(player_name, n = top_n_players, other_level = "Other_Player")
    )
  
  # Apply same lumping to test data (important: use same levels)
  player_levels <- levels(gam_data_train$player_name)
  gam_data_test <- gam_data_test %>%
    dplyr::mutate(
      player_name = forcats::fct_other(player_name, keep = player_levels, other_level = "Other_Player")
    )
  
  # Handle team factor levels
  common_teams <- intersect(levels(gam_data_train$team), levels(gam_data_test$team))
  gam_data_test <- gam_data_test %>%
    dplyr::filter(team %in% common_teams)
  
  # Convert target variable from 0,1,2,3 to 1,2,3,4 for ocat() family
  cat("Converting target variable for ordered categorical model...\n")
  gam_data_train <- gam_data_train %>%
    dplyr::mutate(!!dplyr::sym(target_col) := .data[[target_col]] + 1)
  gam_data_test <- gam_data_test %>%
    dplyr::mutate(!!dplyr::sym(target_col) := .data[[target_col]] + 1)
  
  # Check final counts
  player_counts <- gam_data_train %>% dplyr::count(player_name, sort = TRUE)
  cat(sprintf("Players after lumping: %d (including Other_Player)\n", nrow(player_counts)))
  if ("Other_Player" %in% player_counts$player_name) {
    cat(sprintf("Other_Player games: %d\n", player_counts$n[player_counts$player_name == "Other_Player"]))
  }

  cat(sprintf("GAM training data: %d observations\n", nrow(gam_data_train)))
  cat(sprintf("GAM test data: %d observations\n", nrow(gam_data_test)))
  cat(sprintf("Unique players: %d\n", dplyr::n_distinct(gam_data_train$player_name)))
  cat(sprintf("Unique teams: %d\n", dplyr::n_distinct(gam_data_train$team)))
  
  return(list(
    train = gam_data_train,
    test = gam_data_test,
    available_features = available_features,
    player_levels = player_levels,
    common_teams = common_teams
  ))
}

#' Build GAM Formula
#'
#' Intelligently builds GAM formula by determining which variables should be
#' smooth terms vs random effects based on their cardinality
#'
#' @param available_features Vector of feature names
#' @param data Training data for checking variable cardinality
#' @param target_col Target column name
#'
#' @return List with formula components
#' @export
build_gam_formula <- function(available_features, data, target_col) {
  
  smooth_terms <- c()
  random_effect_terms <- c()
  
  # Check unique values for a variable
  check_variable <- function(var_name, data) {
    if (var_name %in% names(data)) {
      n_unique <- length(unique(data[[var_name]]))
      return(n_unique)
    }
    return(0)
  }
  
  # High cardinality continuous variables - use smooth terms
  if ("supercoach_score" %in% available_features) smooth_terms <- c(smooth_terms, "s(supercoach_score, k=12)")
  if ("afl_fantasy_score" %in% available_features) smooth_terms <- c(smooth_terms, "s(afl_fantasy_score, k=10)")
  if ("impact_score" %in% available_features) smooth_terms <- c(smooth_terms, "s(impact_score, k=10)")
  if ("disposals" %in% available_features) smooth_terms <- c(smooth_terms, "s(disposals, k=10)")
  if ("metres_gained" %in% available_features) smooth_terms <- c(smooth_terms, "s(metres_gained, k=8)")
  if ("player_career_games" %in% available_features) smooth_terms <- c(smooth_terms, "s(player_career_games, k=8)")
  if ("match_margin" %in% available_features) smooth_terms <- c(smooth_terms, "s(match_margin, k=8)")
  if ("rating_points" %in% available_features) smooth_terms <- c(smooth_terms, "s(rating_points, k=8)")
  
  # Medium cardinality - use smooth terms with lower k
  if ("goals" %in% available_features) smooth_terms <- c(smooth_terms, "s(goals, k=6)")
  if ("marks" %in% available_features) smooth_terms <- c(smooth_terms, "s(marks, k=6)")
  if ("kicks" %in% available_features) smooth_terms <- c(smooth_terms, "s(kicks, k=6)")
  if ("handballs" %in% available_features) smooth_terms <- c(smooth_terms, "s(handballs, k=6)")
  if ("tackles" %in% available_features) smooth_terms <- c(smooth_terms, "s(tackles, k=6)")
  if ("inside_fifties" %in% available_features) smooth_terms <- c(smooth_terms, "s(inside_fifties, k=5)")
  
  # Correlation and ratio variables - smooth with moderate k
  if ("expected_votes_history" %in% available_features) smooth_terms <- c(smooth_terms, "s(expected_votes_history, k=6)")
  if ("disp_vote_correlation" %in% available_features) smooth_terms <- c(smooth_terms, "s(disp_vote_correlation, k=6)")
  if ("avg_votes_per_disposal" %in% available_features) smooth_terms <- c(smooth_terms, "s(avg_votes_per_disposal, k=6)")
  if ("goals_vote_correlation" %in% available_features) smooth_terms <- c(smooth_terms, "s(goals_vote_correlation, k=6)")
  if ("tackles_vote_correlation" %in% available_features) smooth_terms <- c(smooth_terms, "s(tackles_vote_correlation, k=6)")
  if ("score_involvement" %in% available_features) smooth_terms <- c(smooth_terms, "s(score_involvement, k=5)")
  
  # Combine all terms into a formula string
  all_terms <- c(smooth_terms, random_effect_terms)
  
  if (length(all_terms) == 0) {
    # Fallback formula with intercept only
    formula_string <- paste(target_col, "~ 1")
  } else {
    formula_string <- paste(target_col, "~", paste(all_terms, collapse = " + "))
  }
  
  return(formula_string)
}

#' Build Enhanced GAM Formula with Categorical Support
#'
#' Builds GAM formula handling both smooth terms and categorical variables (random effects)
#'
#' @param smooth_features Vector of continuous feature names for smooth terms
#' @param categorical_features Vector of categorical feature names for random effects
#' @param data Training data for checking variable properties
#' @param target_col Target column name
#'
#' @return Formula string
#' @export
build_enhanced_gam_formula <- function(smooth_features, categorical_features, data, target_col) {
  
  smooth_terms <- c()
  random_effect_terms <- c()
  
  # Add smooth terms for continuous variables
  if ("supercoach_score" %in% smooth_features) smooth_terms <- c(smooth_terms, "s(supercoach_score, k=12)")
  if ("afl_fantasy_score" %in% smooth_features) smooth_terms <- c(smooth_terms, "s(afl_fantasy_score, k=10)")
  if ("impact_score" %in% smooth_features) smooth_terms <- c(smooth_terms, "s(impact_score, k=10)")
  if ("disposals" %in% smooth_features) smooth_terms <- c(smooth_terms, "s(disposals, k=10)")
  if ("metres_gained" %in% smooth_features) smooth_terms <- c(smooth_terms, "s(metres_gained, k=8)")
  if ("player_career_games" %in% smooth_features) smooth_terms <- c(smooth_terms, "s(player_career_games, k=8)")
  if ("match_margin" %in% smooth_features) smooth_terms <- c(smooth_terms, "s(match_margin, k=8)")
  if ("rating_points" %in% smooth_features) smooth_terms <- c(smooth_terms, "s(rating_points, k=8)")
  
  # Medium cardinality - use smooth terms with lower k
  if ("goals" %in% smooth_features) smooth_terms <- c(smooth_terms, "s(goals, k=6)")
  if ("marks" %in% smooth_features) smooth_terms <- c(smooth_terms, "s(marks, k=6)")
  if ("kicks" %in% smooth_features) smooth_terms <- c(smooth_terms, "s(kicks, k=6)")
  if ("handballs" %in% smooth_features) smooth_terms <- c(smooth_terms, "s(handballs, k=6)")
  if ("tackles" %in% smooth_features) smooth_terms <- c(smooth_terms, "s(tackles, k=6)")
  if ("inside_fifties" %in% smooth_features) smooth_terms <- c(smooth_terms, "s(inside_fifties, k=5)")
  
  # Correlation and ratio variables - smooth with moderate k
  if ("expected_votes_history" %in% smooth_features) smooth_terms <- c(smooth_terms, "s(expected_votes_history, k=6)")
  if ("disp_vote_correlation" %in% smooth_features) smooth_terms <- c(smooth_terms, "s(disp_vote_correlation, k=6)")
  if ("avg_votes_per_disposal" %in% smooth_features) smooth_terms <- c(smooth_terms, "s(avg_votes_per_disposal, k=6)")
  if ("goals_vote_correlation" %in% smooth_features) smooth_terms <- c(smooth_terms, "s(goals_vote_correlation, k=6)")
  if ("tackles_vote_correlation" %in% smooth_features) smooth_terms <- c(smooth_terms, "s(tackles_vote_correlation, k=6)")
  if ("score_involvement" %in% smooth_features) smooth_terms <- c(smooth_terms, "s(score_involvement, k=5)")
  
  # Add smooth term for coaches_votes (numeric 0-10)
  if ("coaches_votes" %in% smooth_features) {
    smooth_terms <- c(smooth_terms, "s(coaches_votes, k=11)")  # 0-10 values, use smooth term
  }
  if ("career_max_votes" %in% categorical_features) {
    random_effect_terms <- c(random_effect_terms, "s(career_max_votes, bs='re', k=4)")  # 0-3 values
  }
  if ("home_advantage" %in% categorical_features) {
    random_effect_terms <- c(random_effect_terms, "s(home_advantage, bs='re', k=2)")
  }
  if ("is_midfielder" %in% categorical_features) {
    random_effect_terms <- c(random_effect_terms, "s(is_midfielder, bs='re', k=2)")
  }
  if ("is_forward" %in% categorical_features) {
    random_effect_terms <- c(random_effect_terms, "s(is_forward, bs='re', k=2)")
  }
  if ("is_back" %in% categorical_features) {
    random_effect_terms <- c(random_effect_terms, "s(is_back, bs='re', k=2)")
  }
  if ("team" %in% categorical_features) {
    random_effect_terms <- c(random_effect_terms, "s(team, bs='re')")
  }
  if ("player_name" %in% categorical_features) {
    random_effect_terms <- c(random_effect_terms, "s(player_name, bs='re')")
  }
  
  # Combine all terms
  all_terms <- c(smooth_terms, random_effect_terms)
  
  if (length(all_terms) == 0) {
    # Fallback formula with intercept only
    formula_string <- paste(target_col, "~ 1")
  } else {
    formula_string <- paste(target_col, "~", paste(all_terms, collapse = " + "))
  }
  
  return(formula_string)
}

#' Create Factor Variables for GAM
#'
#' Converts discrete/binary variables to factors for use as random effects
#'
#' @param train_data Training data
#' @param test_data Test data
#' @param available_features Available feature names
#'
#' @return List with modified train and test data plus random effect terms
#' @export
create_gam_factors <- function(train_data, test_data, available_features) {
  
  random_effect_terms <- c()
  
  # coaches_votes (0-10): treat as random effect
  if ("coaches_votes" %in% available_features) {
    train_data$coaches_votes_factor <- as.factor(train_data$coaches_votes)
    test_data$coaches_votes_factor <- as.factor(test_data$coaches_votes)
    random_effect_terms <- c(random_effect_terms, "s(coaches_votes_factor, bs='re')")
  }
  
  # career_max_votes (0-3): treat as random effect
  if ("career_max_votes" %in% available_features) {
    train_data$career_max_votes_factor <- as.factor(train_data$career_max_votes)
    test_data$career_max_votes_factor <- as.factor(test_data$career_max_votes)
    random_effect_terms <- c(random_effect_terms, "s(career_max_votes_factor, bs='re')")
  }
  
  # Binary/categorical variables - treat as factors
  if ("home_advantage" %in% available_features) {
    train_data$home_advantage_factor <- as.factor(train_data$home_advantage)
    test_data$home_advantage_factor <- as.factor(test_data$home_advantage)
    random_effect_terms <- c(random_effect_terms, "s(home_advantage_factor, bs='re')")
  }
  
  # Position indicators - treat as factors
  if ("is_midfielder" %in% available_features) {
    train_data$is_midfielder_factor <- as.factor(train_data$is_midfielder)
    test_data$is_midfielder_factor <- as.factor(test_data$is_midfielder)
    random_effect_terms <- c(random_effect_terms, "s(is_midfielder_factor, bs='re')")
  }
  
  if ("is_forward" %in% available_features) {
    train_data$is_forward_factor <- as.factor(train_data$is_forward)
    test_data$is_forward_factor <- as.factor(test_data$is_forward)
    random_effect_terms <- c(random_effect_terms, "s(is_forward_factor, bs='re')")
  }
  
  if ("is_back" %in% available_features) {
    train_data$is_back_factor <- as.factor(train_data$is_back)
    test_data$is_back_factor <- as.factor(test_data$is_back)
    random_effect_terms <- c(random_effect_terms, "s(is_back_factor, bs='re')")
  }
  
  # Goal accuracy grouping
  if ("goal_accuracy" %in% available_features) {
    train_data$goal_accuracy_factor <- cut(
      train_data$goal_accuracy,
      breaks = c(-0.01, 0, 1/3, 1/2, 2/3, 1.01),
      labels = c("0", "<=1/3", "<=1/2", "<=2/3", "<=1"),
      include.lowest = TRUE
    )
    test_data$goal_accuracy_factor <- cut(
      test_data$goal_accuracy,
      breaks = c(-0.01, 0, 1/3, 1/2, 2/3, 1.01),
      labels = c("0", "<=1/3", "<=1/2", "<=2/3", "<=1"),
      include.lowest = TRUE
    )
    random_effect_terms <- c(random_effect_terms, "s(goal_accuracy_factor, bs='re')")
  }
  
  return(list(
    train = train_data,
    test = test_data,
    random_effect_terms = random_effect_terms
  ))
}




#' Prepare GAM Training Data
#'
#' Prepares training data specifically for GAM models with proper factor handling
#' and feature validation.
#'
#' @param all_training_data Raw training data
#' @param target_col Target column name
#' @param feature_cols Available feature columns
#' @param prepared_data Prepared modeling data structure
#'
#' @return Prepared GAM training data
#' @export
prepare_gam_training_data <- function(all_training_data, target_col, feature_cols, prepared_data) {
  gam_data_train <- all_training_data %>%
    dplyr::select(dplyr::all_of(c(target_col, "player_name", "team", "season", feature_cols)),
                  dplyr::any_of(c("match_id", "match_round", "round", "date"))) %>%
    dplyr::mutate(
      player_name = forcats::fct_lump_n(as.factor(player_name), n = 1000, other_level = "Other_Player"),
      team = as.factor(team),
      season = as.factor(season),
      votes = .data[[target_col]] + 1
    ) %>%
    dplyr::filter(!is.na(.data[[target_col]]))

  return(gam_data_train)
}

#' Prepare GAM Test Data
#'
#' Prepares test data for GAM models ensuring factor levels match training data.
#'
#' @param test_data Raw test data
#' @param gam_data_train Prepared training data (for factor level matching)
#' @param target_col Target column name
#' @param feature_cols Available feature columns
#' @param prepared_data Prepared modeling data structure
#'
#' @return Prepared GAM test data
#' @export
prepare_gam_test_data <- function(test_data, gam_data_train, target_col, feature_cols, prepared_data) {
  gam_data_test <- test_data %>%
    dplyr::select(dplyr::all_of(c(target_col, "player_name", "team", "season", feature_cols)),
                  dplyr::any_of(c("match_id", "match_round", "round", "date"))) %>%
    dplyr::mutate(
      player_name = forcats::fct_other(as.factor(player_name),
                                       keep = levels(gam_data_train$player_name),
                                       other_level = "Other_Player"),
      team = as.factor(team),
      season = as.factor(season),
      votes = .data[[target_col]] + 1
    )

  return(gam_data_test)
}

#' Validate GAM Features
#'
#' Checks for missing features and handles missing values in test data.
#'
#' @param gam_data_test Test data for GAM
#' @param model_vars Vector of required model variables
#' @param verbose Logical, whether to show progress messages
#'
#' @return Cleaned test data with complete cases
#' @export
validate_gam_features <- function(gam_data_test, model_vars, verbose = TRUE) {
  missing_vars <- c()

  for(var in model_vars) {
    if(var %in% names(gam_data_test)) {
      missing_count <- sum(is.na(gam_data_test[[var]]))
      if(missing_count > 0) {
        missing_vars <- c(missing_vars, var)
      }
    }
  }

  if(length(missing_vars) > 0) {
    if (verbose) {
      cat(sprintf("Removing %d rows with missing values in: %s\n",
                  sum(apply(gam_data_test[missing_vars], 1, function(x) any(is.na(x)))),
                  paste(missing_vars, collapse = ", ")))
    }

    # Keep only complete cases for the model variables
    complete_rows <- complete.cases(gam_data_test[model_vars])
    gam_data_test <- gam_data_test[complete_rows, ]
  }

  return(gam_data_test)
}

#' Train GAM with Validation
#'
#' Trains a GAM model with specified features and handles errors gracefully.
#'
#' @param gam_data_train Training data
#' @param feature_formula GAM formula with features
#' @param verbose Logical, whether to show progress messages
#'
#' @return Trained GAM model or NULL if failed
#' @export
train_gam_with_validation <- function(gam_data_train, feature_formula, verbose = TRUE) {
  if (verbose) cat("Training GAM model...")

  tryCatch({
    gam_model <- mgcv::bam(
      feature_formula,
      data = gam_data_train,
      family = mgcv::ocat(R = 4),
      discrete = TRUE,
      select = TRUE,
      nthreads = parallel::detectCores() - 1
    )

    if (verbose) cat(" ✓\n")
    return(gam_model)

  }, error = function(e) {
    if (verbose) cat(sprintf(" Error: %s\n", e$message))
    return(NULL)
  })
}

#' Extract Random Effects from GAM Model
#'
#' Extracts player-level random effects (coefficients) from a trained GAM model
#' for use as features in other models like XGBoost.
#'
#' @param gam_model Trained GAM model with player random effects
#' @param add_group_N Logical, whether to add group count information (default: TRUE)
#' @param group_var Name of the grouping variable to extract (default: "player_name")
#'
#' @return Data frame with player names, coefficients, standard errors, and derived features
#' @export
extract_random_effects <- function(gam_model, add_group_N = TRUE, group_var = "player_name") {

  if (is.null(gam_model)) {
    warning("GAM model is NULL, returning empty data frame")
    return(data.frame())
  }

  tryCatch({
    # Use mixedup to extract random effects - much simpler!
    if (!requireNamespace("mixedup", quietly = TRUE)) {
      stop("mixedup package required for random effects extraction")
    }

    # Extract random effects (suppress warnings about non-factor variables)
    ranef_data <- suppressWarnings(mixedup::extract_ranef(gam_model))

    if (is.null(ranef_data) || nrow(ranef_data) == 0) {
      warning("No random effects extracted from GAM model")
      return(data.frame())
    }

    # Filter to player_name group if it exists
    player_effects <- ranef_data %>%
      dplyr::filter(group_var == !!group_var)

    if (nrow(player_effects) == 0) {
      warning(sprintf("No random effects found for group_var: %s", group_var))
      return(data.frame())
    }

    # Convert numeric group IDs back to player names using factor levels
    if (!is.null(gam_model$model) && group_var %in% names(gam_model$model)) {
      player_factor <- gam_model$model[[group_var]]
      if (is.factor(player_factor)) {
        player_levels <- levels(player_factor)

        # Map numeric group IDs to actual player names
        player_effects <- player_effects %>%
          dplyr::mutate(
            group_numeric = as.numeric(group),
            group = ifelse(group_numeric <= length(player_levels),
                          player_levels[group_numeric],
                          paste0("Unknown_", group_numeric))
          ) %>%
          dplyr::select(-group_numeric)
      }
    }

    # Add group counts if requested
    if (add_group_N && !is.null(gam_model$model) && group_var %in% names(gam_model$model)) {
      group_counts <- table(gam_model$model[[group_var]])
      player_effects <- player_effects %>%
        dplyr::left_join(
          data.frame(
            group = names(group_counts),
            n = as.numeric(group_counts),
            stringsAsFactors = FALSE
          ),
          by = "group"
        )
    }

    # Arrange by value (highest first)
    player_effects <- player_effects %>%
      dplyr::arrange(dplyr::desc(value))

    return(player_effects)

  }, error = function(e) {
    warning(sprintf("Failed to extract random effects: %s", e$message))
    return(data.frame())
  })
}