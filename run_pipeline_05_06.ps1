$ErrorActionPreference = "Stop"
Set-Location "C:\dev\brownlow"
$log = "C:\dev\brownlow\pipeline_05_06.log"
"=== 05/06 rerun on adopted honest params, started $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss') ===" | Out-File $log

foreach ($stage in @("05_xgboost_model.R", "06_ensemble_predictions.R")) {
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
"`n=== 05/06 COMPLETE $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss') ===" | Out-File $log -Append
