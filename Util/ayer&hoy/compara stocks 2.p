FOR EACH hoy.almmmate NO-LOCK WHERE codcia = 1:
    FIND LAST hoy.almstkal OF hoy.almmmate WHERE fecha <= 12/31/07
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE hoy.almstkal THEN NEXT.
    FIND LAST ayer.almstkal OF hoy.almmmate WHERE ayer.almstkal.fecha <= 12/31/07
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ayer.almstkal THEN DO:
        DISPLAY 'no encontrado' hoy.almmmate.codalm hoy.almmmate.codmat.
        NEXT.
    END.
    IF hoy.almstkal.stkact <> ayer.almstkal.stkact THEN DO:
        DISPLAY hoy.almmmate.codalm hoy.almmmate.codmat hoy.almstkal.stkact ayer.almstkal.stkact.
    END.

END.

