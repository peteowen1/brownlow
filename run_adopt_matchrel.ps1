$ErrorActionPreference = "Stop"
Set-Location "C:\dev\brownlow"
$log = "C:\dev\brownlow\adopt_matchrel.log"
"=== adopt match-relative chain started $(Get-Date -Format 'HH:mm:ss') ===" | Out-File $log

$steps = @(
  "02_base_feature_engineering.R",
  "03_gam_model.R",
  "04_enhanced_feature_engineering.R",
  "analysis/adopt_rmse_rounds.R",
  "05_xgboost_model.R",
  "analysis/ensemble_weight_walkforward.R",
  "06_ensemble_predictions.R"
)
foreach ($s in $steps) {
  $start = Get-Date
  "`n=== $s started $($start.ToString('HH:mm:ss')) ===" | Out-File $log -Append
  & Rscript $s *>&1 | Out-File $log -Append
  $code = $LASTEXITCODE
  $mins = [math]::Round(((Get-Date) - $start).TotalMinutes, 1)
  if ($code -ne 0) {
    "`n=== $s FAILED (exit $code) after $mins min -- stopping ===" | Out-File $log -Append
    exit $code
  }
  "`n=== $s OK after $mins min ===" | Out-File $log -Append
}
"`n=== ADOPT MATCHREL COMPLETE $(Get-Date -Format 'HH:mm:ss') ===" | Out-File $log -Append
