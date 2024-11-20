DEF TEMP-TABLE detalle
    FIELD codcia AS INTE
    field coddoc AS CHAR
    field nroped AS CHAR
    field coddiv AS CHAR
    field codcli AS CHAR
    field codmat AS CHAR
    field canped AS DECI
    field factor AS DECI
    field coduni AS CHAR
    field candes AS DECI
    field preuni AS DECI
    field tipo AS CHAR
    FIELD codmon AS INTE
    field fecha AS DATE 
    field hora AS CHAR
    field usuario AS CHAR
    .

INPUT FROM d:\muestra.d.
REPEAT :
    CREATE detalle.
    IMPORT detalle.
END.

DEF BUFFER pedido FOR faccpedi.
OUTPUT TO d:\facturados.d.
FOR EACH detalle NO-LOCK,
    FIRST logdsctosped NO-LOCK WHERE logdsctosped.codcia = 1
    AND logdsctosped.codmat = detalle.codmat 
    AND logdsctosped.codped = detalle.coddoc
    AND logdsctosped.nroped = detalle.nroped:
    FOR EACH pedido NO-LOCK WHERE pedido.codcia = 1
        AND pedido.coddoc = 'ped'
        AND pedido.codref = detalle.coddoc
        AND pedido.nroref = detalle.nroped,
        EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 1
        AND ccbcdocu.codped = pedido.coddoc
        AND ccbcdocu.nroped = pedido.nroped
        AND LOOKUP(ccbcdocu.coddoc, 'FAC,BOL') > 0:
        EXPORT logdsctosped.
        LEAVE.
    END.
END.
OUTPUT CLOSE.

