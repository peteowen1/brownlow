# Brownlow Package Architecture

Reference documentation for function domains, design patterns, and competition details. See CLAUDE.md for behavioral rules.

## Modeling Framework

```
Raw Data → Feature Engineering → Individual Models → Ensemble → Predictions
    ↓              ↓                    ↓              ↓           ↓
Datathon      100+ Features        XGBoost         Optimal     Competition
Dataset    (Performance,Historical,  + GAM        Weighting     Submissions
           Context,Rolling Stats)   Models        (Time-aware   (6 votes/match)
                                                  CV splits)
```

## Function Architecture by Domain

### Data Functions (`R/enhanced_datathon_data.R`, `R/datathon_data.R`)
- `enhance_datathon_data()` - Primary data loading with umpire/weather enrichment
- `load_datathon_data()` - Base datathon dataset loading
- Data validation and quality checks

### Feature Engineering (`R/feature_engineering.R`, `R/historical_features.R`, `R/weather_features.R`, etc.)
- **Performance Features**: Impact scores, efficiency ratios, fantasy points
- **Historical Features**: Career voting patterns, over/under-performance metrics
- **Rolling Statistics**: 3/5/10-game rolling averages and trends (with proper data leakage prevention)
- **Context Features**: Team share, home advantage, match situation, weather effects

### Model Utilities (`R/model_utils.R`, `R/xgboost_utils.R`, `R/gam_utils.R`)
- `prepare_modeling_data()` - Train/validation/test splits with time-awareness
- `evaluate_model()` - Comprehensive performance metrics for regression/multinomial
- `tune_xgboost_hyperparameters()` - Parallel hyperparameter optimization
- `apply_voting_constraints()` - Ensures exactly 6 votes per match

### Script Utilities (`R/script_utils.R`)
- `load_features_for_prediction()` - Smart feature loading with fallback tiers
- `load_trained_models()` - Load ensemble models with error handling
- `generate_model_predictions()` - Generate XGBoost, GAM, and ensemble predictions
- `apply_prediction_constraints()` - Apply AFL voting constraints to probability matrices

### Simulation Utilities (`R/simulation_utils.R`)
- `run_parallel_simulations()` - Run thousands of simulations in parallel
- `analyze_simulation_results()` - Extract winner probabilities and confidence intervals

## Utility Function Design Patterns

### Smart Loading Functions
```r
# Pattern: load_[resource]_for_[purpose](target, options, verbose = TRUE)
load_features_for_prediction(target_season, verbose = TRUE)
load_trained_models(verbose = TRUE)
```

### Processing Functions
```r
# Pattern: [action]_[resource](data, parameters, verbose = TRUE)
generate_model_predictions(prediction_data, trained_models, feature_cols, verbose = TRUE)
apply_prediction_constraints(prob_matrices, prediction_data, verbose = TRUE)
```

### Return Structure Consistency
All utility functions return structured lists with:
- **Main Result**: The primary output (e.g., `features`, `predictions`)
- **Metadata**: Supporting information (e.g., `feature_cols`, `source`)
- **Error Handling**: Functions return `NULL` on failure with clear error messages

## Competition-Specific Requirements

### Constraint Compliance
- Exactly 6 votes per match across all players
- Votes must be integers 0, 1, 2, or 3
- Built-in validation prevents invalid submissions

### Performance Targets (Historical Test Data)
- RMSE: ~0.34 (ensemble model)
- Correlation: ~0.67 (with actual votes)
- Vote-getting accuracy: ~89% (predicting who gets votes)

### Submission Format
```r
# Generated automatically by 06_season_predictions.R
# Format: player_name, team, match_date, predicted_votes
# Validation: Ensures exactly 6 votes per match
```
