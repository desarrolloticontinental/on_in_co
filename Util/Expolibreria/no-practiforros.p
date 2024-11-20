/* CLIENTES CON PRATIFORRO MENOR A 1.5% */
DEF VAR x-implin AS DEC.
DEF VAR x-imptot AS DEC.

OUTPUT TO c:\tmp\no-practiforros.txt.
PUT UNFORMATTED
    'FECHA|DOC|NUMERO|VENDEDOR|DIVISION|CLIENTE|IMPORTE TOTAL|IMPORTE PRACTIFORRO'
    SKIP.
FOR EACH faccpedi NO-LOCK WHERE codcia = 001
    AND coddiv = '10015'
    AND coddoc = 'cot'
    AND flgest <> 'A',
    FIRST gn-clie NO-LOCK WHERE gn-clie.codcia = 000
    AND gn-clie.codcli = faccpedi.codcli,
    FIRST gn-ven OF faccpedi NO-LOCK,
    FIRST gn-divi NO-LOCK WHERE gn-divi.codcia = 001
    AND gn-divi.coddiv = gn-clie.coddiv:
    ASSIGN
        x-implin = 0
        x-imptot = faccpedi.imptot.
    FOR EACH FacDPedi OF Faccpedi NO-LOCK, FIRST Almmmatg OF FacDPedi NO-LOCK:
        CASE Almmmatg.CodFam:
            WHEN '012' THEN DO:
                IF Almmmatg.SubFam = '063' THEN x-implin = x-implin + FacDPedi.ImpLin.
            END.
        END CASE.
    END.
    IF x-implin < ( x-imptot * 1.5 / 100) THEN DO:
        PUT UNFORMATTED
            faccpedi.fchped '|'
            faccpedi.coddoc '|'
            faccpedi.nroped '|'
            faccpedi.codven ' - '
            gn-ven.nomven '|'
            gn-clie.coddiv ' - '
            gn-divi.desdiv '|'
            faccpedi.codcli ' - '
            faccpedi.nomcli '|'
            x-imptot '|'
            x-implin
            SKIP.

    END.
END.
OUTPUT CLOSE.
