DEF VAR x-linea AS CHAR NO-UNDO.
DEF VAR x-nroitm AS INT INIT 1 NO-UNDO.
DEF VAR R-ROWID AS ROWID NO-UNDO.

INPUT FROM d:\tmp\primavera.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    FIND almmmatg WHERE almmmatg.codcia = 001
        AND almmmatg.codmat = SUBSTRING(x-linea,1,6).
    IF DEC(SUBSTRING(x-linea,7)) <= 0 THEN NEXT.
    CREATE almdmov.
    ASSIGN
        Almdmov.CodCia = 001
        Almdmov.CodAlm = '514'
        Almdmov.TipMov = "I"
        Almdmov.CodMov = 01
        Almdmov.NroSer = 000
        Almdmov.NroDoc = 000001
        Almdmov.FchDoc = TODAY
        Almdmov.codmat = SUBSTRING(x-linea,1,6)
        Almdmov.CanDes = DEC(SUBSTRING(x-linea,7))
        Almdmov.CodUnd = almmmatg.undstk
        Almdmov.Factor = 1
        Almdmov.NroItm = x-nroitm
        .
    R-ROWID = ROWID(Almdmov).
    RUN ALM\ALMACSTK (R-ROWID).
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
    RUN ALM\ALMACPR1 (R-ROWID,"U").
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'. 

    x-nroitm = x-nroitm + 1.
END.
INPUT CLOSE.
