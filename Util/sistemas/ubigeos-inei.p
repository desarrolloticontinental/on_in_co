FOR EACH tabdepto EXCLUSIVE-LOCK WHERE coddepto >= '01' and coddepto <= '99':
    FOR EACH tabprovi OF tabdepto EXCLUSIVE-LOCK:
        DELETE tabprovi.
    END.
    FOR EACH tabdistr OF tabdepto EXCLUSIVE-LOCK:
        DELETE tabdistr.
    END.
    DELETE tabdepto.
END.
MESSAGE 'parte 1 ok'.
/* Cargamos informacion */
DEF VAR x-depto AS CHAR.
DEF VAR x-provi AS CHAR.
DEF VAR x-distr AS CHAR.
DEF VAR x-linea AS CHAR.
DEF VAR x-nomdep AS CHAR.
DEF VAR x-nompro AS CHAR.
DEF VAR x-nomdis AS CHAR.
INPUT FROM d:\tmp\ubigeos.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    ASSIGN
        x-depto = SUBSTRING(x-linea,1,2)
        x-provi = SUBSTRING(x-linea,6,2)
        x-distr = SUBSTRING(x-linea,11,2)
        x-nomdep = CAPS(SUBSTRING(x-linea,16,15))
        x-nompro = CAPS(SUBSTRING(x-linea,31,30))
        x-nomdis = CAPS(SUBSTRING(x-linea,61)).
    FIND FIRST TabDepto WHERE TabDepto.CodDepto = x-depto NO-LOCK NO-ERROR.
    IF NOT AVAILABLE TabDepto THEN DO:
        CREATE TabDepto.
        ASSIGN
        TabDepto.CodDepto = x-depto 
        TabDepto.NomDepto = x-nomdep.
    END.
    FIND FIRST TabProvi WHERE TabProvi.CodDepto = x-depto AND
        TabProvi.CodProvi = x-provi NO-LOCK NO-ERROR.
    IF NOT AVAILABLE TabProvi THEN DO:
        CREATE TabProvi.
        ASSIGN
        TabProvi.CodDepto = x-depto
        TabProvi.CodProvi = x-provi
        TabProvi.NomProvi = x-nompro.
    END.
    FIND FIRST TabDistr WHERE TabDistr.CodDepto = x-depto AND 
        TabDistr.CodProvi = x-provi AND
        TabDistr.CodDistr = x-distr NO-LOCK NO-ERROR.
    IF NOT AVAILABLE TabDistr THEN DO:
        CREATE TabDistr.
        ASSIGN
        TabDistr.CodDepto = x-depto
        TabDistr.CodProvi = x-provi
        TabDistr.CodDistr = x-distr 
        TabDistr.NomDistr = x-nomdis
        TabDistr.CodPos = (IF NOT (x-depto = '15' AND x-provi = '01')
                           THEN 'P0' ELSE '').
    END.
END.
INPUT CLOSE.
