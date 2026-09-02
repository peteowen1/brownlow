$ErrorActionPreference = "Stop"
Set-Location "C:\dev\brownlow"
$log = "C:\dev\brownlow\honest_retune.log"
"=== honest retune started $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss') ===" | Out-File $log
$start = Get-Date
& Rscript analysis/honest_retune_267.R *>&1 | Out-File $log -Append
$code = $LASTEXITCODE
$mins = [math]::Round(((Get-Date) - $start).TotalMinutes, 1)
if ($code -ne 0) {
  "`n=== FAILED (exit $code) after $mins min ===" | Out-File $log -Append
  exit $code
}
"`n=== HONEST RETUNE COMPLETE after $mins min ===" | Out-File $log -Append
