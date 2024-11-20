DEF TEMP-TABLE detalle
    FIELD codmat AS CHAR.


FIND almcmov WHERE almcmov.codcia = 1
    AND almcmov.codalm = '21'
    AND almcmov.tipmov = 's'
    AND almcmov.codmov = 41
    AND almcmov.nroser = 21
    AND almcmov.nrodoc = 2003260
    NO-LOCK.
FOR EACH almdmov OF almcmov NO-LOCK:
    RUN carga-temporal.
END.
FIND almcmov WHERE almcmov.codcia = 1
    AND almcmov.codalm = '21f'
    AND almcmov.tipmov = 's'
    AND almcmov.codmov = 41
    AND almcmov.nroser = 21
    AND almcmov.nrodoc = 2003261
    NO-LOCK.
FOR EACH almdmov OF almcmov NO-LOCK:
    RUN carga-temporal.
END.


OUTPUT TO d:\codigos.prn.
FOR EACH detalle NO-LOCK:
    PUT UNFORMATTED detalle.codmat SKIP.
END.
OUTPUT CLOSE.


PROCEDURE carga-temporal:

FIND FIRST detalle WHERE detalle.codmat = almdmov.codmat NO-LOCK NO-ERROR.
IF NOT AVAILABLE detalle THEN DO:
    CREATE detalle.
    ASSIGN detalle.codmat = almdmov.codmat.
END.


END PROCEDURE.
