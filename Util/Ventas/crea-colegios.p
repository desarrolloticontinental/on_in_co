DEF VAR x-linea AS CHAR.

INPUT FROM c:\tmp\colegios.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    CREATE gn-clie.
    ASSIGN
        gn-clie.codcia = 000
        gn-clie.codcli = SUBSTRING(x-linea,1,20)
        gn-clie.libre_c01 = "J"
        gn-clie.flgsit = "A"
        gn-clie.rucold = "No"
        gn-clie.libre_l01 = NO
        gn-clie.nombre = SUBSTRING(x-linea,21,40)
        gn-clie.nomcli = SUBSTRING(x-linea,21,40)
        gn-clie.dircli = SUBSTRING(x-linea,61,60)
        gn-clie.coddept = SUBSTRING(x-linea,121,10)
        gn-clie.codprov = SUBSTRING(x-linea,131,10)
        gn-clie.coddist = SUBSTRING(x-linea,141,10)
        gn-clie.canal = '005'
        gn-clie.gircli ='0005'
        gn-clie.clfcli = 'C'
        gn-clie.clfcli2 = 'C'
        gn-clie.Fching = TODAY
        gn-clie.usuario = 'ADMIN'
        .
    RUN lib/logtabla ("gn-clie", STRING(gn-clie.codcia, '999') + '|' +
                    STRING(gn-clie.codcli, 'x(11)'), 'CREATE').
END.
INPUT CLOSE.
