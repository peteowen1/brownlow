$ErrorActionPreference = "Stop"
Set-Location "C:\dev\brownlow"
$log = "C:\dev\brownlow\rmse_sel.log"
"=== extended RMSE-vs-mlogloss round search started $(Get-Date -Format 'HH:mm:ss') ===" | Out-File $log
$start = Get-Date
& Rscript analysis/rmse_vs_mlogloss_selection.R *>&1 | Out-File $log -Append
$code = $LASTEXITCODE
$mins = [math]::Round(((Get-Date) - $start).TotalMinutes, 1)
if ($code -ne 0) { "`n=== FAILED (exit $code) after $mins min ===" | Out-File $log -Append; exit $code }
"`n=== RMSE SELECTION COMPLETE after $mins min ===" | Out-File $log -Append
