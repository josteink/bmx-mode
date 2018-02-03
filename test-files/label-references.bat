:START

REM with spaces afterwards
:MID   

REM
:ABC
:ABCabc_123

goto :ABC
goto :ABCabc_123
call ABC
call ABCabc_123

REM not these ones
goto :NOT_EXIST
goto NOT_EXIST
call :NOT_EXIST
call NOT_EXIST
REM :NOT_EXIST

:END            
