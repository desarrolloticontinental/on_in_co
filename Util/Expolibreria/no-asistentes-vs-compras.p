DEF VAR x-imptot AS DEC NO-UNDO.

OUTPUT TO c:\tmp\no-asistentes.txt.
PUT UNFORMATTED
    "CLIENTE|PROGRAMACION|CAMP.ANTERIOR S/.|CANAL"
    SKIP.
FOR EACH expasist NO-LOCK WHERE codcia = 1
    AND coddiv = '10015'
    AND expasist.fecpro >= 01/03/2013 
    AND expasist.fecpro <= 01/05/2013
    AND expasist.estado[1] <> "C",
    FIRST gn-clie NO-LOCK WHERE gn-clie.codcia = 000
    AND gn-clie.codcli = expasist.codcli,
    FIRST gn-divi NO-LOCK WHERE gn-divi.codcia = 001
    AND gn-divi.coddiv = gn-clie.coddiv:
    x-imptot = 0.
    FOR EACH w-report NO-LOCK WHERE task-no = 666 AND Llave-C = expasist.CodCli:
        CASE Llave-I:
            WHEN 10 THEN DO:
                x-imptot = x-imptot + Campo-F[1].
            END.
            WHEN 20 THEN DO:
                x-imptot = x-imptot + Campo-F[1].
            END.
        END CASE.
    END.
    PUT UNFORMATTED
        expasist.codcli ' - '
        expasist.nomcli '|'
        fecpro ' - '
        horapro '|'
        x-imptot '|'
        gn-clie.coddiv ' - '
        gn-divi.desdiv
        SKIP.
END.
OUTPUT CLOSE.

