DEF VAR x-linea AS CHAR NO-UNDO.

INPUT FROM d:\tmp\nuevoflete.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    DISPLAY SUBSTRING(x-linea,1,6) DECIMAL(SUBSTRING(x-linea,21)) FORMAT '>>9.9999'.
    PAUSE 0.
    CREATE tabgener.
    ASSIGN
        TabGener.CodCia = 001
        TabGener.Clave  = '%FLETE-IMP'
        TabGener.Codigo = '20060|' + SUBSTRING(x-linea,1,6)
        TabGener.ValorIni = DECIMAL(SUBSTRING(x-linea,21)).
END.
