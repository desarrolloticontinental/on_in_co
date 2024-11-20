
DISABLE TRIGGERS FOR LOAD OF almmmate.

DEFINE BUFFER b-almmmate FOR almmmate.

    DEFINE VAR x-conteo AS INT.

FOR EACH almmmate NO-LOCK WHERE almmmate.codcia = 1
    AND almmmate.codalm = '11w':

    FIND FIRST vtalistamay WHERE vtalistama.codcia = 1
                AND vtalistamay.codmat = almmmate.codmat
                AND vtalistamay.coddiv = '00524' NO-LOCK NO-ERROR.
    IF AVAILABLE vtalistamay THEN NEXT.

    FIND FIRST vtalistamay WHERE vtalistama.codcia = 1
                AND vtalistamay.codmat = almmmate.codmat
                AND vtalistamay.coddiv = '00525' NO-LOCK NO-ERROR.
    IF AVAILABLE vtalistamay THEN NEXT.

    FIND FIRST b-almmmate WHERE ROWID(b-almmmate) = ROWID(almmmate) EXCLUSIVE-LOCK NO-ERROR.

    IF AVAILABLE b-almmmate THEN DO:
        ASSIGN b-almmmate.codalm = "11wxyz".
    END.

    /* play Por ahora solo pintamos */
    /*    DISPLAY b-almmmate.codmat. */
    /*PAUSE 0.*/
   x-conteo = x-conteo + 1.
END.

RELEASE b-almmmate NO-ERROR.

MESSAGE x-conteo.

/* probemos */
