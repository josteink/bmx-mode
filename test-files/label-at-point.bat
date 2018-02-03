REM test-case 0
:PROPER0

REM test-case 1 - simple one label invocation
goto :proper1
GOTO :proper2
goto proper3
goto PROPER4

REM test-case 2 - simple one label invocation
call :proper5 "abc"
call :PROPER6 "abc"
call proper7 "abc"
call PROPER8 "abc"
