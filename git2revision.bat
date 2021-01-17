@ECHO OFF
SET revfile=%~1
git --version 1>nul 2>nul
IF %ERRORLEVEL% EQU 0 (CALL :gitrev) else (CALL :emptyrev)
GOTO :EOF

:gitrev
ECHO const>"%revfile%"
FOR /F %%i IN ('git rev-list --count --first-parent HEAD') DO SET revnumber=%%i
FOR /F %%i IN ('git log --pretty^=%%H -1') DO SET revsha=%%i
ECHO   REVISION_NUMBER = '%revnumber%';>>"%revfile%"
ECHO   REVISION_SHA = '%revsha%';>>"%revfile%"
GOTO :EOF

:emptyrev
ECHO const>"%revfile%"
ECHO   REVISION_NUMBER = '';>>"%revfile%"
ECHO   REVISION_SHA = '';>>"%revfile%"
GOTO :EOF
