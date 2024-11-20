/* familias y subfamilias */
DEF VAR s-codcia AS INT INIT 001.

OUTPUT TO d:\tmp\familias.txt.
PUT UNFORMATTED 'FAMILIA|DESC FAM|SUB FAMILIA|DESC SUB FAM' SKIP.
FOR EACH almtfami NO-LOCK WHERE codcia = s-codcia,
    EACH almsfami OF almtfami NO-LOCK:
    PUT UNFORMATTED
        Almtfami.codfam '|'
        Almtfami.desfam '|'
        AlmSFami.subfam '|'
        AlmSFami.dessub
        SKIP.
END.
OUTPUT CLOSE.

