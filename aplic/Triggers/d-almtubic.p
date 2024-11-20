TRIGGER PROCEDURE FOR DELETE OF AlmTubic.

DEF SHARED VAR s-codalm AS CHAR.

FIND FIRST Almmmate WHERE Almmmate.codcia = Almtubic.codcia
    AND Almmmate.codalm = s-codalm
    AND Almmmate.codubi = Almtubic.codubi
    NO-LOCK NO-ERROR.
IF AVAILABLE Almmmate THEN DO:
    MESSAGE 'El c�digo de ubicaci�n' Almtubic.codubi SKIP
        'ya est� asignado al producto' Almmmate.codmat SKIP
        'Proceso abortado'
        VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.
