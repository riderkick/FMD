@ECHO OFF
REM lazbuild must be in environment variable LAZ or set it here
REM SET LAZ=C:\lazarus
TITLE=FMD Release
SET cdir=%CD%
CD /D "%cdir%"
SET repodl=https://github.com/fmd-project-team/FMD/releases/download/
ECHO ; automatically build with make_release_win.bat>update

CALL :makerelease i386-win32 Win32 --no-write-project
ECHO WIN32=%repodl%%fverb%/%oname%>>update

CALL :makerelease x86_64-win64 Win64 --no-write-project
ECHO WIN64=%repodl%%fverb%/%oname%>>update

ECHO VERSION=%fverb%>>update

PAUSE
GOTO :EOF

:makerelease
TITLE make %~1
SET lbuild=%LAZ%\lazbuild --build-mode="%~2" %~3
%lbuild% "%cdir%\updaterslim\updater.lpi"
%lbuild% "%cdir%\mangadownloader\md.lpi"
SET tdir=%cdir%\bin\%~1
SET rdir=%cdir%\Release
SET odir=%rdir%\%~1
CALL :getfileversion "%tdir%\fmd.exe"
SET oname=fmd_%fverb%_%~1.7z
RMDIR /S /Q "%odir%"
MKDIR "%odir%"
call :copycdir config
call :copycdir extras
call :copycdir images
call :copycdir licenses
XCOPY /C /F /Y "%cdir%\languages\*.po" "%odir%\languages\"
XCOPY /E /C /F /Y "%cdir%\dist\%~1" "%odir%\"
XCOPY /F /Y "%cdir%\changelog.txt" "%odir%\"
XCOPY /F /Y "%cdir%\readme.rtf" "%odir%\"
XCOPY /F /Y "%tdir%\fmd.exe" "%odir%\"
XCOPY /F /Y "%tdir%\updater.exe" "%odir%\"
DEL /F "%rdir%\%oname%"
"%cdir%\dist\%~1\7za" a "%rdir%\%oname%" "%odir%\*" -mx9 -ssw -stl -t7z -y
RMDIR /S /Q "%odir%"
ECHO ----------------------------------------------------------
GOTO :EOF

:copycdir
XCOPY /E /C /F "%cdir%\%~1" "%odir%\%~1\"
GOTO :EOF

:getfileversion
SET fname=%~1
SET fname=%fname:\=\\%
SET fver=0.0.0
SET fverb=0.0.0.0
FOR /F "Tokens=1* Delims==" %%A IN (
    'WMIC DATAFILE WHERE "NAME='%fname%'" GET VERSION /VALUE 2^>Nul'
) DO FOR /F "Tokens=*" %%C IN ("%%B") DO SET "fverb=%%C"
FOR /F "delims=. tokens=1-3" %%i IN ("%fverb%") DO SET fver=%%i.%%j.%%k
GOTO :EOF
