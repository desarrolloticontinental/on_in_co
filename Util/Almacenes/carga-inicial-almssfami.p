

    DELETE FROM almssfami.

    FOR EACH almtfami NO-LOCK WHERE codcia = 1,
        EACH almsfami OF almtfami NO-LOCK:
        CREATE almssfami.
        ASSIGN
            AlmSSFami.CodCia = 1
            AlmSSFami.CodFam = almtfami.codfam
            AlmSSFami.CodSSFam = '999'
            AlmSSFami.DscSSFam = 'Por Definir'
            AlmSSFami.SubFam = Almsfami.subfam.

    END.
