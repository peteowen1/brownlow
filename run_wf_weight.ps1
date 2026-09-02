$ErrorActionPreference = "Stop"
Set-Location "C:\dev\brownlow"
$log = "C:\dev\brownlow\wf_weight.log"
"=== walk-forward weight selection started $(Get-Date -Format 'HH:mm:ss') ===" | Out-File $log
$start = Get-Date
& Rscript analysis/ensemble_weight_walkforward.R *>&1 | Out-File $log -Append
$code = $LASTEXITCODE
$mins = [math]::Round(((Get-Date) - $start).TotalMinutes, 1)
if ($code -ne 0) { "`n=== FAILED (exit $code) after $mins min ===" | Out-File $log -Append; exit $code }
"`n=== WF WEIGHT COMPLETE after $mins min ===" | Out-File $log -Append
