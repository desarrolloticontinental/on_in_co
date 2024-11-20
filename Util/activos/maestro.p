DEF VAR s-codcia AS INT INIT 001.
DEF VAR x-codpar-1 AS CHAR FORMAT 'x(4)'.
DEF VAR x-codpar-2 AS CHAR FORMAT 'x(2)'.
DEF VAR x-codpar-3 AS CHAR FORMAT 'x(4)'.
DEF VAR x-cantidad AS INT.
DEF VAR x-linea AS CHAR FORMAT 'x(200)'.


INPUT FROM c:\tmp\activos.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    ASSIGN
        x-codpar-1 = SUBSTRING(x-linea,1,4)
        x-codpar-2 = SUBSTRING(x-linea,11,2)
        x-codpar-3 = SUBSTRING(x-linea,19,4)
        x-cantidad = INTEGER(SUBSTRING(x-linea,38,5)).
    IF x-codpar-1 <> '' AND x-cantidad = 1 THEN RUN carga-activos.
END.
INPUT CLOSE.

RETURN.

PROCEDURE carga-activos:

    CREATE ac-parti.

    FIND ac-copar WHERE ac-copar.Codcia = s-codcia
        AND ac-copar.codcta = x-codpar-1
        AND ac-copar.ccosto = x-CodPar-2
        AND ac-copar.periodo = INTEGER (x-CodPar-3)
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE ac-copar THEN DO:
        CREATE ac-copar.
        ASSIGN
            ac-copar.codcia = s-codcia
            ac-copar.codcta = x-codpar-1
            ac-copar.ccosto = x-codpar-2
            ac-copar.periodo = INTEGER (x-CodPar-3)
            ac-copar.correlativo = 1.
    END.
    ac-parti.codpar = TRIM (ac-copar.codcta) + 
                      TRIM (ac-copar.ccosto) +
                      STRING(ac-copar.periodo, '9999') +
                      STRING(ac-copar.correlativo, '99999').
    ac-copar.correlativo = ac-copar.correlativo + 1.

    ASSIGN
        AC-PARTI.CodCia = s-codcia
        AC-PARTI.DesPar = SUBSTRING(x-linea,43,58)
        AC-PARTI.Marca = SUBSTRING(x-linea,102,14)
        AC-PARTI.Serie = SUBSTRING(x-linea,116,23)
        ac-parti.codcta-1 = SUBSTRING(x-linea,1,6).

END PROCEDURE.
