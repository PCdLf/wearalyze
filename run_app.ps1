# Find and kill any process using port 8080
$ProcessID = Get-NetTCPConnection -LocalPort 8080 -ErrorAction SilentlyContinue | Select-Object -ExpandProperty OwningProcess
if ($ProcessID) {
    Write-Host "Port 8080 is in use by process $ProcessID. Terminating..."
    Stop-Process -Id $ProcessID -Force
    Start-Sleep -Seconds 2 # Allow time for the port to be released
}

# Run R script in the background
Start-Process "Rscript.exe" -ArgumentList "app_local.R" -NoNewWindow

# Wait for the server to start
$maxAttempts = 30 # Max wait time (30 seconds)
$attempt = 0
while (-not (Test-NetConnection -ComputerName "127.0.0.1" -Port 8080 -InformationLevel Quiet) -and $attempt -lt $maxAttempts) {
    Start-Sleep -Seconds 1
    $attempt++
}

if ($attempt -eq $maxAttempts) {
    Write-Host "Error: Server did not start within expected time."
    exit 1
}

# Open default browser with the specified URL
Start-Process "http://127.0.0.1:8080"
