DEF VAR s-codalm AS CHAR INIT '10,35' NO-UNDO.
DEF VAR x-codalm AS CHAR NO-UNDO.
DEF VAR j AS INT NO-UNDO.


DEF TEMP-TABLE detalle LIKE almmmate
    FIELD preuni AS DEC DECIMALS 4 FORMAT '>>>,>>>,>>9.99'.

DO  j = 1 TO NUM-ENTRIES(s-codalm):
    x-codalm = ENTRY(j, s-codalm).
    FOR EACH almmmate NO-LOCK WHERE almmmate.codcia = 1
        AND almmmate.codalm = x-codalm
        AND almmmate.stkact > 0:
        CREATE detalle.
        BUFFER-COPY almmmate TO detalle.
        FIND LAST almstkge WHERE almstkge.codcia = almmmate.codcia
            AND almstkge.codmat = almmmate.codmat
            NO-LOCK NO-ERROR.
        IF AVAILABLE almstkge THEN detalle.preuni = AlmStkge.CtoUni.
    END.
END.

OUTPUT TO c:\tmp\stosck-10-35.txt.
FOR EACH detalle, FIRST almmmatg OF detalle:
    DISPLAY
        detalle.codalm
        detalle.codmat
        almmmatg.desmat
        almmmatg.desmar
        almmmatg.undbas
        almmmatg.codfam
        almmmatg.subfam
        detalle.stkact
        detalle.preuni
        WITH STREAM-IO NO-BOX NO-LABELS WIDTH 320.
END.
OUTPUT CLOSE.

