# One live-count refresh: pull the AFL feed, re-simulate the unread matches,
# rebuild the artifact page. Run from the repo root.
#   powershell.exe -File analysis/live_refresh.ps1
param([int]$Sims = 20000)
$ErrorActionPreference = "Stop"
$env:LIVE_SIMS = $Sims
$t0 = Get-Date
Rscript "analysis/live_count.R"
Rscript "analysis/build_artifact_data.R"
"refresh took {0:n1} min" -f ((Get-Date) - $t0).TotalMinutes
