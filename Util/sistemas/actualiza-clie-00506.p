RLOOP:
FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddiv = '00506'
    AND coddoc = 'cot'
    AND codcli BEGINS '20'
    BREAK BY faccpedi.codcli:
    IF FIRST-OF(faccpedi.codcli) THEN DO:
        FIND gn-clie WHERE gn-clie.codcia = 000
            AND gn-clie.codcli = faccpedi.codcli
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE gn-clie THEN NEXT.
        FIND gn-clied OF gn-clie WHERE gn-clied.sede = '@@@' NO-LOCK NO-ERROR.
        IF NOT AVAILABLE gn-clied THEN NEXT.
        IF gn-clie.dircli <> gn-clied.dircli THEN DO:
            FIND CURRENT gn-clie EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF NOT AVAILABLE gn-clie THEN NEXT RLOOP.
            ASSIGN
                gn-clie.dircli = gn-clied.dircli
                gn-clie.coddept = gn-clied.coddept
                gn-clie.codprov = gn-clied.codprov
                gn-clie.coddist = gn-clied.coddist
                gn-clie.codpos = gn-clied.codpos.
            DISPLAY gn-clie.codcli. PAUSE 0.
        END.
    END.
END.


