DEF TEMP-TABLE detalle LIKE ccbddocu
    FIELD nomcli LIKE ccbcdocu.nomcli
    INDEX llave01 codcia coddoc nrodoc.

DEF VAR x-nrodoc AS CHAR FORMAT 'x(9)'.

INPUT FROM m:\tmp\wong.prn.
REPEAT:
    IMPORT UNFORMATTED x-nrodoc.
    IF x-nrodoc <> '' THEN DO:
        FIND ccbcdocu WHERE codcia = 1
            AND coddoc = 'fac'
            AND nrodoc = x-nrodoc
            NO-LOCK NO-ERROR.
        FOR EACH ccbddocu OF ccbcdocu NO-LOCK:
            CREATE detalle.
            BUFFER-COPY ccbddocu TO detalle
                ASSIGN
                    detalle.fchdoc = ccbcdocu.fchdoc.
        END.
    END.
END.
INPUT CLOSE.

OUTPUT TO m:\tmp\edith.txt.
FOR EACH detalle, FIRST almmmatg OF detalle NO-LOCK:
    DISPLAY
        detalle.coddoc
        detalle.nrodoc
        detalle.fchdoc
        detalle.codmat
        almmmatg.desmat
        almmmatg.desmar
        detalle.undvta
        detalle.candes
        detalle.preuni
        detalle.implin
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.

