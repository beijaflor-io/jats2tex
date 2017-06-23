setlocal

stack --install-ghc build
@if errorlevel 1 exit /b
stack exec jats2tex-build-installer
@if errorlevel 1 exit /b

stack path --local-install-root > tmp-local-install-root
set /p BIN_ROOT=<tmp-local-install-root
copy /y %BIN_ROOT%\bin\jats2tex.exe .\jats2tex.exe
copy /y %BIN_ROOT%\bin\jats2tex-web.exe .\jats2tex-web.exe
rem .\tmp-local-install-root

"c:\Program Files (x86)\NSIS\Bin\makensis.exe" -V3 jats2tex-install.nsi
@if errorlevel 1 exit /b
