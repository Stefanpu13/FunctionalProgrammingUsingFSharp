@echo off
cls

.paket\paket.bootstrapper.exe
if errorlevel 1 (
  exit /b %errorlevel%
)
REM Can use the following command instead "restore": .paket\paket.exe install --create-new-binding-files
REM  flag "--createnewbindingfiles" is needed to redirect 3rd party deps that use older versions of FSharpCore
.paket\paket.exe restore
if errorlevel 1 (
  exit /b %errorlevel%
)

packages\FAKE\tools\FAKE.exe build.fsx %*
