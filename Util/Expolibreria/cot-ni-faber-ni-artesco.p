/* CLIENTES QUE NO HAN COMPRADO NI FABER NI ARTESCO */
DEF VAR s-codpro AS CHAR NO-UNDO.

s-codpro = "10006732,10005035".

OUTPUT TO c:\tmp\ni-faber-ni-artesco.txt.
PUT UNFORMATTED 
    "CODIGO|NUMERO|FECHA|VENDEDOR|DIVISION|CLIENTE|IMPORTE"
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
    FIND FIRST facdpedi OF faccpedi WHERE 
        CAN-FIND(FIRST almmmatg OF facdpedi WHERE LOOKUP(almmmatg.codpr1, s-codpro) > 0 NO-LOCK)
        NO-LOCK NO-ERROR.
    IF AVAILABLE facdpedi THEN NEXT.
    PUT UNFORMATTED
        faccpedi.coddoc '|'
        faccpedi.nroped '|'
        faccpedi.fchped '|'
        faccpedi.codven ' - '
        gn-ven.nomven '|'
        gn-clie.coddiv ' - '
        gn-divi.desdiv '|'
        faccpedi.codcli ' - '
        faccpedi.nomcli '|'
        faccpedi.imptot
        SKIP.
END.
OUTPUT CLOSE.

