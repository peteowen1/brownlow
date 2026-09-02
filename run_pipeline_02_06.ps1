# Clean rerun of the feature/model pipeline, stages 02-06.
#
# POSTMORTEM-2025.md Sec 7 item 2: every measurement in that document was taken on
# features built BEFORE the whole-career leak fix (Sec 5i) and the club-name fix
# (Sec 5c). This regenerates them so downstream numbers mean something.
#
# Stops at the first failing stage rather than letting later stages run on stale
# inputs and silently produce plausible nonsense.

$ErrorActionPreference = "Stop"
Set-Location "C:\dev\brownlow"

$stages = @(
  "02_base_feature_engineering.R",
  "03_gam_model.R",
  "04_enhanced_feature_engineering.R",
  "05_xgboost_model.R",
  "06_ensemble_predictions.R"
)

$log = "C:\dev\brownlow\pipeline_rerun.log"
"=== pipeline rerun started $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss') ===" | Out-File $log

foreach ($stage in $stages) {
  $start = Get-Date
  "`n=== $stage started $($start.ToString('HH:mm:ss')) ===" | Out-File $log -Append
  & Rscript $stage *>&1 | Out-File $log -Append
  $code = $LASTEXITCODE
  $mins = [math]::Round(((Get-Date) - $start).TotalMinutes, 1)
  if ($code -ne 0) {
    "`n=== $stage FAILED (exit $code) after $mins min -- stopping ===" | Out-File $log -Append
    exit $code
  }
  "`n=== $stage OK after $mins min ===" | Out-File $log -Append
}

"`n=== ALL STAGES COMPLETE $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss') ===" | Out-File $log -Append
