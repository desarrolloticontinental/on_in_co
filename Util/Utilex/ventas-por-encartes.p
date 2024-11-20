/* ventas por encartes utilex */
OUTPUT TO c:\tmp\vtas-utilex.txt APPEND.
FOR EACH faccpedi NO-LOCK WHERE codcia = 001
    AND coddoc = 'p/m'
    AND flgest = 'C'
    AND fchped >= 01/01/2015
    AND flgsit = "CD":
    PUT UNFORMATTED
        faccpedi.coddiv '|'
        faccpedi.coddoc '|'
        faccpedi.nroped '|'
        faccpedi.fchped '|'
        faccpedi.flgsit '|'
        faccpedi.libre_c05 '|'
        faccpedi.codcli '|'
        faccpedi.nomcli '|'
        faccpedi.imptot '|'
        faccpedi.usuario
        SKIP.
END.
OUTPUT CLOSE.
MESSAGE 'proceso terminado'.
