DEF TEMP-TABLE detalle
    FIELD codmat LIKE almmmatg.codmat
    FIELD desmat LIKE almmmatg.desmat
    FIELD ventas AS DEC EXTENT 3
    FIELD costo AS DEC EXTENT 3
    FIELD cantidad AS DEC EXTENT 3
    INDEX llave01 AS PRIMARY codmat.



DEF VAR x-codmat AS CHAR FORMAT 'x(6)'.

INPUT FROM c:\tmp\productos.prn.
REPEAT :
    IMPORT UNFORMATTED x-codmat.
    DISPLAY x-codmat.
    PAUSE 0.
    FIND almmmatg WHERE codcia = 1
        AND codmat = x-codmat
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE almmmatg THEN NEXT.
    FIND detalle WHERE detalle.codmat = x-codmat NO-ERROR.
    IF AVAILABLE detalle THEN NEXT.
    CREATE detalle.
    ASSIGN
        detalle.codmat = almmmatg.codmat
        detalle.desmat = almmmatg.desmat.
    FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = 1:
        FOR EACH evtall01 NO-LOCK WHERE evtall01.codcia = 1
            AND evtall01.coddiv = gn-divi.coddiv
            AND evtall01.codmat = x-codmat
            AND evtall01.nrofch >= 200904
            AND evtall01.nrofch <= 200911:
            ASSIGN
                ventas[1] = ventas[1] + EvtALL01.VtaxMesMe
                costo[1] = costo[1] + EvtALL01.CtoxMesMe
                cantidad[1] = cantidad[1] + EvtALL01.CanxMes.
        END.
    END.

END.
INPUT CLOSE.

FOR EACH detalle:
    x-codmat = detalle.codmat.
    DISPLAY x-codmat.
    PAUSE 0.
    FIND almmmatg WHERE almmmatg.codcia = 1
        AND almmmatg.codmat = x-codmat
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE almmmatg THEN NEXT.
    FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = 1:
        FOR EACH evtall01 NO-LOCK WHERE evtall01.codcia = 1
            AND evtall01.coddiv = gn-divi.coddiv
            AND evtall01.codmat = x-codmat
            AND evtall01.nrofch >= 200912
            AND evtall01.nrofch <= 201003:
            ASSIGN
                ventas[2] = ventas[2] + EvtALL01.VtaxMesMe
                costo[2] = costo[2] + EvtALL01.CtoxMesMe
                cantidad[2] = cantidad[2] + EvtALL01.CanxMes.
        END.
    END.

END.


FOR EACH detalle:
    x-codmat = detalle.codmat.
    DISPLAY x-codmat.
    PAUSE 0.
    FIND almmmatg WHERE almmmatg.codcia = 1
        AND almmmatg.codmat = x-codmat
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE almmmatg THEN NEXT.
    FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = 1:
        FOR EACH evtall01 NO-LOCK WHERE evtall01.codcia = 1
            AND evtall01.coddiv = gn-divi.coddiv
            AND evtall01.codmat = x-codmat
            AND evtall01.nrofch >= 200904
            AND evtall01.nrofch <= 201003:
            ASSIGN
                ventas[3] = ventas[3] + EvtALL01.VtaxMesMe
                costo[3] = costo[3] + EvtALL01.CtoxMesMe
                cantidad[3] = cantidad[3] + EvtALL01.CanxMes.
        END.
    END.

END.

OUTPUT TO c:\tmp\resumen.txt.
FOR EACH detalle:
    DISPLAY
        detalle.codmat
        detalle.desmat
        detalle.ventas[1]
        detalle.costo[1]
        detalle.cantidad[1]
        detalle.ventas[2]
        detalle.costo[2]
        detalle.cantidad[2]
        detalle.ventas[3]
        detalle.costo[3]
        detalle.cantidad[3]
        WITH STREAM-IO NO-BOX WIDTH 320.

END.
OUTPUT CLOSE.

