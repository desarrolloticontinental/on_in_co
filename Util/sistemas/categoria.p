DEF VAR x-md AS DECI.
OUTPUT TO d:\categoria.txt.
FOR EACH vtalistamay NO-LOCK WHERE codcia = 1
    AND coddiv = '20015',
    FIRST almmmatg OF vtalistamay NO-LOCK WHERE almmmatg.codfam = '014':
    FOR EACH clfcli NO-LOCK:
        IF almmmatg.CHR__02 = 'P' THEN x-md = clfcli.pordsc / 100. ELSE x-md = clfcli.pordsc1 / 100.
        PUT UNFORMATTED
            clfcli.categoria '|'
            vtalistamay.codmat '|'
            x-md
            SKIP.
    END.
END.
