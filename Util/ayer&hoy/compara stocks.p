FOR EACH hoy.almmmatg NO-LOCK WHERE codcia = 1:
    FIND LAST hoy.almstkge OF hoy.almmmatg WHERE fecha <= 12/31/07
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE hoy.almstkge THEN NEXT.
    FIND LAST ayer.almstkge OF hoy.almmmatg WHERE ayer.almstkge.fecha <= 12/31/07
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ayer.almstkge THEN DO:
        DISPLAY 'no encontrado' hoy.almmmatg.codmat.
        NEXT.
    END.
    IF hoy.almstkge.stkact <> ayer.almstkge.stkact THEN DO:
        DISPLAY hoy.almmmatg.codmat hoy.almstkge.stkact ayer.almstkge.stkact.
    END.

END.

