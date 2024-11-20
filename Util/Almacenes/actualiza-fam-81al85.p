DISABLE TRIGGERS FOR LOAD OF almsfami.
DISABLE TRIGGERS FOR LOAD OF almssfami.

/* limpiamos */
FOR EACH almtfami WHERE codcia = 1
    AND codfam >= '081'
    AND codfam <= '085':
    FOR EACH almsfami OF almtfami:
        DELETE almsfami.
    END.
END.

/* cargamos */
DEF VAR x-linea AS CHAR NO-UNDO.

INPUT FROM c:\tmp\lineas.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    CREATE almsfami.
    ASSIGN
        AlmSFami.CodCia = 001
        AlmSFami.codfam = SUBSTRING(x-linea,1,3)
        AlmSFami.dessub = SUBSTRING(X-LINEA,15)
        AlmSFami.subfam = SUBSTRING(X-LINEA,11,3).
END.
INPUT CLOSE.


    FOR EACH  almssfami.
        DELETE almssfami.
    END.

    FOR EACH almtfami NO-LOCK WHERE codcia = 1,
        EACH almsfami NO-LOCK WHERE almsfami.codcia = 1
        AND almsfami.codfam = almtfami.codfam:
        CREATE almssfami.
        ASSIGN
            AlmSSFami.CodCia = 1
            AlmSSFami.CodFam = almtfami.codfam
            AlmSSFami.SubFam = Almsfami.subfam
            AlmSSFami.CodSSFam = '999'
            AlmSSFami.DscSSFam = 'Por Definir'
            .
    END.

