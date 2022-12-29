$root = "./data/A20"
$flacFiles = Get-ChildItem $root -Recurse -Filter "*.flac"

$table = @()
foreach ($flacFile in $flacFiles) {
    $row = [PSCustomObject]@{
        "File Name"     = $flacFile.Name
        "Parent Folder" = $flacFile.Directory.Name
    }
    $table += $row
}
# This is in progress TODO
$table | Format-Table -AutoSize
$table | Export-Csv -Path "table.csv" -NoTypeInformation
Write-Output "$table"