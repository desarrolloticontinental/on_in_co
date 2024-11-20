DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF NEW SHARED VAR s-coddiv AS CHAR INIT '00000'.
DEF NEW SHARED VAR s-codalm AS CHAR INIT '11'.
DEF NEW SHARED VAR s-tipmov AS CHAR.
DEF NEW SHARED VAR s-codmov AS INT INIT 96.
DEF NEW SHARED VAR s-user-id AS CHAR INIT 'SYSTEM'.

DEF VAR x-linea AS CHAR NO-UNDO.

DEF VAR x-codmat AS CHAR.
DEF VAR x-ingreso AS DEC.
DEF VAR x-salida AS DEC.
DEF VAR x-preuni AS DEC DECIMALS 4.
DEF VAR x-impcto AS DEC.
DEF VAR x-candes AS DEC.
DEF VAR x-fchdoc AS DATE.
x-fchdoc = DATE(12,31,2018).

INPUT FROM d:\tmp\c043114.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    ASSIGN
        x-codmat = SUBSTRING(x-linea,1,6)
        x-ingreso = ABS(DEC(SUBSTRING(x-linea,11,10)))
        x-salida = ABS(DEC(SUBSTRING(x-linea,21,10)))
        x-preuni = ABS(DEC(SUBSTRING(x-linea,31,10)))
        x-impcto = ABS(DEC(SUBSTRING(x-linea,41,10))).
        /*x-fchdoc = DATE(SUBSTRING(x-linea,51,10)).*/
    IF x-ingreso > 0 THEN DO:
        s-tipmov = "I".
        x-candes = x-ingreso.
        RUN MovAlm.
        NEXT.
    END.
    IF x-salida > 0 THEN DO:
        s-tipmov = 'S'.
        x-candes = x-salida.
        RUN MovAlm.
        NEXT.
    END.
END.
INPUT CLOSE.

PROCEDURE MovAlm:

    FIND almtdocm WHERE almtdocm.codcia = s-codcia
        AND almtdocm.codalm = s-codalm
        AND almtdocm.tipmov = s-tipmov
        AND almtdocm.codmov = s-codmov
        EXCLUSIVE-LOCK.
    CREATE Almcmov.
    ASSIGN 
        Almcmov.CodCia  = Almtdocm.CodCia 
        Almcmov.CodAlm  = Almtdocm.CodAlm 
        Almcmov.TipMov  = Almtdocm.TipMov
        Almcmov.CodMov  = Almtdocm.CodMov
        Almcmov.NroSer  = 000
        Almcmov.NroDoc = Almtdocm.NroDoc.
    ASSIGN
        Almtdocm.NroDoc = Almtdocm.NroDoc + 1.
    ASSIGN
        Almcmov.CodMon = 1
        Almcmov.FchDoc = x-fchdoc
        Almcmov.usuario = s-user-id.
    CREATE Almdmov.
    BUFFER-COPY Almcmov TO Almdmov
        ASSIGN
        Almdmov.nroitm = 1
        Almdmov.codmat = x-codmat
        Almdmov.candes = x-candes
        Almdmov.factor = 1
        Almdmov.preuni = x-preuni
        almdmov.impcto = x-impcto.
    FIND Almmmatg OF Almdmov NO-LOCK.
    ASSIGN
        Almdmov.codund = almmmatg.undstk.

END PROCEDURE.
