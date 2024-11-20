OUTPUT TO d:\tmp\cesados.txt.
FOR EACH gn-clie NO-LOCK WHERE codcia = 0
    AND flgsit = "C":
    FIND gn-divi WHERE gn-divi.codcia = 001
        AND gn-divi.coddiv = gn-clie.coddiv 
        no-lock NO-ERROR.
    PUT UNFORMATTED
        codcli '|'
        nomcli '|'
        dircli '|'
        ruc '|'
        (IF gn-clie.libre_c01 = "J" THEN "JURIDICA" ELSE (IF gn-clie.Libre_c01 = "N" THEN "NATURAL" ELSE "EXTRANJERA")) '|'
        fchces '|'
        (IF AVAILABLE gn-divi THEN gn-divi.coddiv + ' ' + gn-divi.desdiv ELSE gn-clie.coddiv)
        SKIP
        .
END.
