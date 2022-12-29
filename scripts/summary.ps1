# Search for .flac files
Get-ChildItem -Recurse -Filter "*.flac" | ForEach-Object {
  # Retrieve the duration of the file
  $duration = & ffprobe -i $_.FullName -show_entries format=duration -v quiet -of csv="p=0"
  
  # Retrieve the creation date and time of the file
  $date_time = & ffprobe -i $_.FullName -show_entries format_tags=RecordingStart -v quiet -of csv="p=0"
  
  # Print the duration and creation date/time of the file
  Write-Output "$duration, $date_time"
}