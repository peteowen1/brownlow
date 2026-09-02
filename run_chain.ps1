# Run a sequence of pipeline steps, stopping at the first failure.
#
# Replaces twelve near-identical one-off runners accumulated on 2026-09-02/03
# (run_pipeline_02_06, run_rmse_chain, run_seedavg_adopt, ...). They differed
# only in which scripts they ran and what they called the log.
#
# Stops at the first failing step rather than letting later stages run on stale
# inputs and produce plausible nonsense — the failure mode this pipeline is
# most prone to, since every stage writes artifacts the next one reads.
#
# Usage:
#   pwsh -File run_chain.ps1 -Steps "05_xgboost_model.R","06_ensemble_predictions.R"
#   pwsh -File run_chain.ps1 -Steps "02_base_feature_engineering.R" -LogName rebuild
#
# Detached (the pattern used all session, so a long run survives the caller):
#   Start-Process pwsh -ArgumentList '-NoProfile','-NonInteractive','-File',
#     'C:\dev\brownlow\run_chain.ps1','-Steps','05_xgboost_model.R' -WindowStyle Hidden

param(
  [Parameter(Mandatory = $true)][string[]]$Steps,
  [string]$LogName = "chain"
)

$ErrorActionPreference = "Stop"
Set-Location "C:\dev\brownlow"

$log = "C:\dev\brownlow\$LogName.log"
"=== chain started $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss') ===" | Out-File $log
"steps: $($Steps -join ' -> ')" | Out-File $log -Append

foreach ($s in $Steps) {
  if (-not (Test-Path $s)) {
    "`n=== $s NOT FOUND -- stopping ===" | Out-File $log -Append
    exit 1
  }
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

"`n=== CHAIN COMPLETE $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss') ===" | Out-File $log -Append
