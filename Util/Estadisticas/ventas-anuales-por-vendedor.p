DEF VAR x-nomcan AS CHAR FORMAT 'x(10)'.
DEF TEMP-TABLE Detalle
    FIELD canal LIKE evtall01.canal
    FIELD codven LIKE evtall01.codven
    FIELD codcli LIKE evtall01.codcli
    FIELD codfam LIKE evtall01.codfam
    FIELD venta AS DEC EXTENT 12
    INDEX llave01 AS PRIMARY codven codcli codfam.

FOR EACH evtall01 NO-LOCK WHERE codcia = 1
    AND coddiv = '00000'
    AND nrofch >= 200901
    AND nrofch <= 200912:
    IF LOOKUP(evtall01.codven, '168,914,952,911,112,915,111,110,185,184,187') = 0 
              THEN NEXT.
    DISPLAY evtall01.nrofch.
    PAUSE 0.
    FIND detalle WHERE detalle.codven = evtall01.codven
        AND detalle.codcli = evtall01.codcli
        AND detalle.codfam = evtall01.codfam
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE detalle THEN DO:
        CREATE detalle.
        ASSIGN
            detalle.codven = evtall01.codven
            detalle.codcli = evtall01.codcli
            detalle.codfam = evtall01.codfam.
    END.
    ASSIGN
        venta[evtall01.codmes] = venta[evtall01.codmes] + EvtALL01.VtaxMesMn.
END.

OUTPUT TO c:\tmp\detalle.txt.
FOR EACH detalle NO-LOCK,
    FIRST gn-ven NO-LOCK WHERE gn-ven.codcia = 001
    AND gn-ven.codven = detalle.codven,
    FIRST gn-clie NO-LOCK WHERE gn-clie.codcia = 000
    AND gn-clie.codcli = detalle.codcli,
    FIRST almtfami NO-LOCK WHERE almtfami.codcia = 001
    AND almtfami.codfam = detalle.codfam:
    x-nomcan = ''.
    FIND almtabla WHERE almtabla.Tabla = 'CN' 
        AND almtabla.Codigo = gn-clie.canal NO-LOCK NO-ERROR.
    IF AVAILABLE almtabla THEN x-nomcan =  almtabla.Nombre.
    DISPLAY
        gn-clie.canal
        '-'
        x-nomcan
        '|'
        detalle.codven
        '-'
        gn-ven.nomven   FORMAT 'x(30)'
        '|'
        detalle.codcli
        '-'
        gn-clie.nomcli FORMAT 'x(50)'
        '|'
        detalle.codfam
        '-'
        Almtfami.desfam FORMAT 'x(30)'
        '|'
        venta[1]    FORMAT '->>>>,>>9'
        '|'
        venta[2]    FORMAT '->>>>,>>9'
        '|'
        venta[3]    FORMAT '->>>>,>>9'
        '|'
        venta[4]    FORMAT '->>>>,>>9'
        '|'
        venta[5]    FORMAT '->>>>,>>9'
        '|'
        venta[6]    FORMAT '->>>>,>>9'
        '|'
        venta[7]    FORMAT '->>>>,>>9'
        '|'
        venta[8]    FORMAT '->>>>,>>9'
        '|'
        venta[9]    FORMAT '->>>>,>>9'
        '|'
        venta[10]    FORMAT '->>>>,>>9'
        '|'
        venta[11]    FORMAT '->>>>,>>9'
        '|'
        venta[12]    FORMAT '->>>>,>>9'
        WITH STREAM-IO NO-BOX NO-LABELS WIDTH 320.
END.
OUTPUT CLOSE.

