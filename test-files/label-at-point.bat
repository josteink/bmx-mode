REM test-case 0
:PROPER

REM test-case 1 - simple one label invocation
goto :proper
GOTO :proper
goto proper
goto PROPER

REM test-case 2 - simple one label invocation
call :proper "abc"
call :PROPER "abc"
call proper "abc"
call PROPER "abc"
