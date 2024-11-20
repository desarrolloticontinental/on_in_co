DEF VAR x-linea AS CHAR.
DEF VAR s-tabla AS CHAR INIT '%REPOXLCNC'.
DEF VAR s-codcia AS INT INIT 001.
DEF VAR x-codigo AS CHAR.

DELETE FROM tabgener WHERE codcia = s-codcia AND clave = s-tabla.

INPUT FROM c:\tmp\distribucionnew.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    x-codigo = TRIM(SUBSTRING(x-linea,11,3)) + '|' +
        TRIM(SUBSTRING(x-linea,21,3)) + '|' +
        TRIM(SUBSTRING(x-linea,31,5)).
    FIND FIRST tabgener WHERE TabGener.CodCia = s-codcia
        AND TabGener.Clave = s-tabla
        AND TabGener.Codigo = x-codigo
        NO-ERROR.
    IF NOT AVAILABLE tabgener THEN CREATE tabgener.
    ASSIGN
        TabGener.CodCia = s-codcia
        TabGener.Clave = s-tabla
        TabGener.Codigo = x-codigo.
    IF SUBSTRING(x-linea,1,1) = 'S' THEN
        ASSIGN
        TabGener.Parametro[1] = DECIMAL(REPLACE(SUBSTRING(x-linea,41,10),'%',''))
        TabGener.Parametro[2] = DECIMAL(REPLACE(SUBSTRING(x-linea,51,10),'%',''))
        TabGener.Parametro[3] = DECIMAL(REPLACE(SUBSTRING(x-linea,61,10),'%','')).
    IF SUBSTRING(x-linea,1,1) = 'N' THEN
        ASSIGN
        TabGener.Parametro[4] = DECIMAL(REPLACE(SUBSTRING(x-linea,41,10),'%',''))
        TabGener.Parametro[5] = DECIMAL(REPLACE(SUBSTRING(x-linea,51,10),'%',''))
        TabGener.Parametro[6] = DECIMAL(REPLACE(SUBSTRING(x-linea,61,10),'%','')).
END.
