# Waits for free RAM to clear a threshold before launching pipeline steps.
# Exists because concurrent sessions on this machine (torpverse, pannaverse)
# have OOM-killed this chain three times at whatever step happened to be
# running when their own jobs spiked. Checking once before launch cannot
# prevent a spike mid-run, but it stops launching straight into a hole.
param(
  [Parameter(Mandatory = $true)][string[]]$Steps,
  [string]$LogName = "chain",
  [int]$MinFreeGB = 7,
  [int]$MaxWaitMinutes = 20
)

$waitLog = "C:\dev\brownlow\$LogName`_wait.log"
$deadline = (Get-Date).AddMinutes($MaxWaitMinutes)
"=== wait started $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss'), need ${MinFreeGB}GB free, cap ${MaxWaitMinutes} min ===" |
  Out-File $waitLog

while ($true) {
  $freeGB = [math]::Round((Get-CimInstance Win32_OperatingSystem).FreePhysicalMemory / 1MB, 1)
  "$(Get-Date -Format 'HH:mm:ss') free=${freeGB}GB" | Out-File $waitLog -Append
  if ($freeGB -ge $MinFreeGB) {
    "=== proceeding at $(Get-Date -Format 'HH:mm:ss'), free=${freeGB}GB ===" | Out-File $waitLog -Append
    break
  }
  if ((Get-Date) -ge $deadline) {
    "=== gave up waiting at $(Get-Date -Format 'HH:mm:ss'), still only ${freeGB}GB free -- launching anyway ===" |
      Out-File $waitLog -Append
    break
  }
  Start-Sleep -Seconds 20
}

& "C:\dev\brownlow\run_chain.ps1" -Steps $Steps -LogName $LogName
