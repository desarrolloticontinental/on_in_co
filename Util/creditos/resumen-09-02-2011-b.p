/* 09.02.11 INFORMACION PARA CREDITOS */

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR cl-codcia AS INT INIT 000 NO-UNDO.
DEF VAR x-fchini AS DATE NO-UNDO.
DEF var x-Cargos AS CHAR INIT 'FAC,BOL,TCK' NO-UNDO.
DEF var x-Abonos AS CHAR INIT 'N/C,N/D' NO-UNDO.

DEF TEMP-TABLE detalle LIKE ccbcdocu.
DEF BUFFER B-CDOCU FOR ccbcdocu.

DISPLAY 'Inicio del proceso' SKIP WITH STREAM-IO NO-BOX NO-LABELS.
PAUSE 0.
x-fchini = 10/01/2010.

/* Rastreamos los documentos de PROVINCIAS */
DISPLAY 'Buscando ventas provincias' SKIP WITH STREAM-IO NO-BOX NO-LABELS.
PAUSE 0.
/* documentos de cargo */
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia:
    FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia
        AND ccbcdocu.fchdoc >= x-fchini
        AND ccbcdocu.flgest <> 'A'
        AND ccbcdocu.coddiv = gn-divi.coddiv:
        IF LOOKUP(ccbcdocu.fmapgo, '101,102,103,104') = 0 THEN NEXT.
        IF LOOKUP(ccbcdocu.codven, '015,173,900,901,902,017') = 0 THEN NEXT.
        IF LOOKUP(ccbcdocu.coddoc, x-Cargos) = 0 THEN NEXT.
        CREATE detalle.
        BUFFER-COPY ccbcdocu TO detalle.
        ASSIGN
            detalle.fchcie = ?.
    END.
END.
/* documentos de abono */
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia:
    FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia
        AND ccbcdocu.fchdoc >= x-fchini
        AND ccbcdocu.flgest <> 'A'
        AND ccbcdocu.coddiv = gn-divi.coddiv:
        IF LOOKUP(ccbcdocu.coddoc, x-Abonos) = 0 THEN NEXT.
        FIND B-CDOCU WHERE B-CDOCU.codcia = s-codcia
            AND B-CDOCU.coddoc = ccbcdocu.codref
            AND B-CDOCU.nrodoc = ccbcdocu.nroref
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE B-CDOCU THEN NEXT.
        IF LOOKUP(B-CDOCU.fmapgo, '101,102,103,104') = 0 THEN NEXT.
        IF LOOKUP(B-CDOCU.codven, '015,173,900,901,902,017') = 0 THEN NEXT.
        CREATE detalle.
        BUFFER-COPY ccbcdocu TO detalle.
        ASSIGN
            detalle.fchcie = ?
            detalle.fmapgo = B-CDOCU.fmapgo.
    END.
END.
/* letras */
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia:
    FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia
        AND ccbcdocu.coddoc = 'LET'
        AND ccbcdocu.fchdoc >= x-fchini
        AND ccbcdocu.flgest <> 'A'
        AND ccbcdocu.coddiv = gn-divi.coddiv:
        FIND Ccbcmvto WHERE Ccbcmvto.codcia = s-codcia
            AND Ccbcmvto.coddoc = ccbcdocu.codref
            AND Ccbcmvto.nrodoc = ccbcdocu.nroref
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Ccbcmvto THEN NEXT.
        FOR EACH Ccbdmvto NO-LOCK WHERE ccbdmvto.codcia = ccbcmvto.codcia
            AND ccbdmvto.coddoc = ccbcmvto.coddoc
            AND ccbdmvto.nrodoc = ccbcmvto.nrodoc
            AND CcbDMvto.TpoRef = "O":
            FIND B-CDOCU WHERE B-CDOCU.codcia = s-codcia
                AND B-CDOCU.coddoc = ccbdmvto.codref
                AND B-CDOCU.nrodoc = ccbdmvto.nroref
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE B-CDOCU THEN NEXT.
            IF LOOKUP(B-CDOCU.fmapgo, '101,102,103,104') = 0 THEN NEXT.
            IF LOOKUP(B-CDOCU.codven, '015,173,900,901,902,017') = 0 THEN NEXT.
            CREATE detalle.
            BUFFER-COPY ccbcdocu TO detalle.
            ASSIGN
                detalle.fchcie = Ccbcmvto.fchapr
                detalle.fmapgo = B-CDOCU.fmapgo.
            LEAVE.
        END.
    END.
END.

/* salida a texto */
DISPLAY 'Salida a texto' SKIP WITH STREAM-IO NO-BOX NO-LABELS.
PAUSE 0.
DEF VAR x-nomven LIKE gn-ven.nomven NO-UNDO.
DEF VAR x-desdiv LIKE GN-DIVI.DesDiv NO-UNDO.

OUTPUT TO c:\tmp\resumen-provincia-b.txt.
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
        detalle.fmapgo '|'
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

