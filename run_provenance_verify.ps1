$ErrorActionPreference = "Stop"
Set-Location "C:\dev\brownlow"
$log = "C:\dev\brownlow\provenance_verify.log"
"=== provenance-check verification chain started $(Get-Date -Format 'HH:mm:ss') ===" | Out-File $log
foreach ($s in @("05_xgboost_model.R","analysis/ensemble_weight_walkforward.R","06_ensemble_predictions.R")) {
  $start = Get-Date
  "`n=== $s started $($start.ToString('HH:mm:ss')) ===" | Out-File $log -Append
  & Rscript $s *>&1 | Out-File $log -Append
  $code = $LASTEXITCODE
  $mins = [math]::Round(((Get-Date) - $start).TotalMinutes, 1)
  if ($code -ne 0) { "`n=== $s FAILED (exit $code) after $mins min ===" | Out-File $log -Append; exit $code }
  "`n=== $s OK after $mins min ===" | Out-File $log -Append
}
"`n=== PROVENANCE VERIFY COMPLETE $(Get-Date -Format 'HH:mm:ss') ===" | Out-File $log -Append
