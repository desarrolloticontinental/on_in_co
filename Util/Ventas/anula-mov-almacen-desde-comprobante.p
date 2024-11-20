
DEFINE VAR cCodDoc AS CHAR.
DEFINE VAR cNroDoc AS CHAR.

cCodDoc = 'FAC'.
cNroDoc = '28200000173'.  /*'25200000004'.*/

/* Movimientos de Almacén */
FIND LAST almcmov WHERE almcmov.codcia = 1
    AND almcmov.tipmov = 's'
    AND almcmov.codmov = 02
    AND almcmov.codref = cCodDoc
    AND almcmov.nroref = cNroDoc
    AND almcmov.flgest <> "A"
    NO-ERROR.
IF AVAILABLE almcmov THEN DO:
    almcmov.flgest = 'A'.
    FOR EACH almdmov OF almcmov:
        DISPLAY 'ALMACEN:' almdmov.codalm almdmov.codmat almdmov.candes.
        PAUSE 0.
        DELETE almdmov.
    END.
END.
