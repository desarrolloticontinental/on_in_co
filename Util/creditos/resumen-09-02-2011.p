/* 09.02.11 INFORMACION PARA CREDITOS */

/* 1ro comprobantes, letras y adelantos emitidos desde el 01/10/2010
de provincia con adelnatos emitidos en ese periodo */

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR cl-codcia AS INT INIT 000 NO-UNDO.
DEF VAR x-fchini AS DATE NO-UNDO.
DEF var x-Documentos AS CHAR INIT 'FAC,BOL,TCK,N/D,N/C,A/C,A/R,LET,CHQ,BD' NO-UNDO.

/* Clientes con adelantos */
DEF TEMP-TABLE t-clie LIKE gn-clie.

DEF TEMP-TABLE detalle LIKE ccbcdocu.

DISPLAY 'Inicio del proceso' SKIP WITH STREAM-IO NO-BOX NO-LABELS.
PAUSE 0.
x-fchini = 10/01/2010.
FOR EACH gn-divi NO-LOCK WHERE codcia = s-codcia:
    FOR EACH ccbcdocu NO-LOCK WHERE codcia = s-codcia
        AND LOOKUP(coddoc, 'a/r,a/c') > 0
        AND coddiv = gn-divi.coddiv
        AND flgest <> 'A'
        AND fchdoc >= x-fchini:
        FIND t-clie WHERE t-clie.codcia = cl-codcia
            AND t-clie.codcli = ccbcdocu.codcli
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE t-clie THEN DO:
            CREATE t-clie.
            ASSIGN
                t-clie.codcia = cl-codcia
                t-clie.codcli = ccbcdocu.codcli.
        END.
    END.
END.

/* Rastreamos los documentos de PROVINCIAS */
DISPLAY 'Buscando ventas provincias' SKIP WITH STREAM-IO NO-BOX NO-LABELS.
PAUSE 0.
FOR EACH t-clie NO-LOCK:
    FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia:
        FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia
            AND ccbcdocu.fchdoc >= x-fchini
            AND ccbcdocu.flgest <> 'A'
            AND ccbcdocu.coddiv = gn-divi.coddiv
            AND ccbcdocu.codcli = t-clie.codcli:
            IF LOOKUP(ccbcdocu.codven, '015,173,900,901,902,017') = 0 THEN NEXT.
            IF LOOKUP(ccbcdocu.coddoc, x-Documentos) = 0 THEN NEXT.
            CREATE detalle.
            BUFFER-COPY ccbcdocu TO detalle.
            IF Ccbcdocu.coddoc = 'LET' THEN DO:
                FIND Ccbcmvto WHERE Ccbcmvto.codcia = s-codcia
                    AND Ccbcmvto.coddoc = Ccbcdocu.codref
                    AND Ccbcmvto.nrodoc = Ccbcdocu.nroref
                    NO-LOCK NO-ERROR.
                IF AVAILABLE Ccbcmvto AND Ccbcmvto.fchapr <> ? THEN DO:
                    ASSIGN
                        detalle.fchcie = Ccbcmvto.fchapr.
                END.
            END.
            ELSE detalle.fchcie = ?.
        END.
    END.
END.
/* depuramos informacion */
FOR EACH t-clie NO-LOCK:
    FIND FIRST detalle WHERE detalle.codcli = t-clie.codcli
        AND LOOKUP(detalle.coddoc, 'a/c,a/r') > 0
        NO-LOCK NO-ERROR.
    IF AVAILABLE detalle THEN NEXT.
    FOR EACH detalle WHERE detalle.codcli = t-clie.codcli:
        DELETE detalle.
    END.
END.

/* salida a texto */
DISPLAY 'Salida a texto' SKIP WITH STREAM-IO NO-BOX NO-LABELS.
PAUSE 0.
DEF VAR x-nomven LIKE gn-ven.nomven NO-UNDO.
DEF VAR x-desdiv LIKE GN-DIVI.DesDiv NO-UNDO.

OUTPUT TO c:\tmp\resumen-provincia.txt.
FOR EACH detalle NO-LOCK:
    FIND gn-ven WHERE gn-ven.codcia = s-codcia
        AND gn-ven.codven = detalle.codven NO-LOCK NO-ERROR.
    IF AVAILABLE gn-ven THEN x-nomven = gn-ven.nomven.
    ELSE x-nomven = ''.
    FIND gn-divi WHERE gn-divi.codcia = s-codcia
        AND gn-divi.coddiv = detalle.coddiv NO-LOCK NO-ERROR.
    IF AVAILABLE gn-divi THEN x-desdiv = gn-divi.desdiv.
    ELSE x-desdiv = ''.
    DISPLAY
        detalle.coddiv x-desdiv '|'
        detalle.fchdoc '|'
        detalle.fchvto '|'
        detalle.fchcie '|'
        detalle.coddoc '|'
        detalle.nrodoc FORMAT 'x(15)' '|'
        detalle.codref '|'
        detalle.nroref '|'
        detalle.codcli detalle.nomcli '|'
        detalle.codven x-nomven '|'
        detalle.codmon '|'
        detalle.imptot
        WITH STREAM-IO NO-BOX NO-LABELS WIDTH 320.
END.
OUTPUT CLOSE.

/* Rastreamos los documentos de EXPOLIBRERIA */
FOR EACH detalle:
    DELETE detalle.
END.
DISPLAY 'Buscando ventas expolibreria' SKIP WITH STREAM-IO NO-BOX NO-LABELS.
PAUSE 0.
FOR EACH t-clie NO-LOCK:
    FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia
        AND gn-divi.coddiv = '00015':
        FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia
            AND ccbcdocu.fchdoc >= x-fchini
            AND ccbcdocu.flgest <> 'A'
            AND ccbcdocu.coddiv = gn-divi.coddiv
            AND ccbcdocu.codcli = t-clie.codcli:
            IF LOOKUP(ccbcdocu.coddoc, x-Documentos) = 0 THEN NEXT.
            CREATE detalle.
            BUFFER-COPY ccbcdocu TO detalle.
            IF Ccbcdocu.coddoc = 'LET' THEN DO:
                FIND Ccbcmvto WHERE Ccbcmvto.codcia = s-codcia
                    AND Ccbcmvto.coddoc = Ccbcdocu.codref
                    AND Ccbcmvto.nrodoc = Ccbcdocu.nroref
                    NO-LOCK NO-ERROR.
                IF AVAILABLE Ccbcmvto AND Ccbcmvto.fchapr <> ? THEN DO:
                    ASSIGN
                        detalle.fchcie = Ccbcmvto.fchapr.
                END.
            END.
            ELSE detalle.fchcie = ?.
        END.
    END.
END.
/* depuramos informacion */
FOR EACH t-clie NO-LOCK:
    FIND FIRST detalle WHERE detalle.codcli = t-clie.codcli
        AND LOOKUP(detalle.coddoc, 'a/c,a/r') > 0
        NO-LOCK NO-ERROR.
    IF AVAILABLE detalle THEN NEXT.
    FOR EACH detalle WHERE detalle.codcli = t-clie.codcli:
        DELETE detalle.
    END.
END.

/* salida a texto */
DISPLAY 'Salida a texto' SKIP WITH STREAM-IO NO-BOX NO-LABELS.
PAUSE 0.

OUTPUT TO c:\tmp\resumen-expolibreria.txt.
FOR EACH detalle NO-LOCK:
    FIND gn-ven WHERE gn-ven.codcia = s-codcia
        AND gn-ven.codven = detalle.codven NO-LOCK NO-ERROR.
    IF AVAILABLE gn-ven THEN x-nomven = gn-ven.nomven.
    ELSE x-nomven = ''.
    FIND gn-divi WHERE gn-divi.codcia = s-codcia
        AND gn-divi.coddiv = detalle.coddiv NO-LOCK NO-ERROR.
    IF AVAILABLE gn-divi THEN x-desdiv = gn-divi.desdiv.
    ELSE x-desdiv = ''.
    DISPLAY
        detalle.coddiv x-desdiv '|'
        detalle.fchdoc '|'
        detalle.fchvto '|'
        detalle.fchcie '|'
        detalle.coddoc '|'
        detalle.nrodoc FORMAT 'x(15)' '|'
        detalle.codref '|'
        detalle.nroref '|'
        detalle.codcli detalle.nomcli '|'
        detalle.codven x-nomven '|'
        detalle.codmon '|'
        detalle.imptot
        WITH STREAM-IO NO-BOX NO-LABELS WIDTH 320.
END.
OUTPUT CLOSE.

