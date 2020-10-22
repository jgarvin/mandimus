:Start
SET command="C:\Program Files\Nuance\NaturallySpeaking12\Program\natspeak.exe"
start "" /REALTIME /B /W  %command%
echo Program terminated at %Date% %Time% with Error %ErrorLevel% >> c:\windows\desktop\program.log 
echo Press Ctrl-C if you don't want to restart automatically
ping -n 3 localhost

goto Start
