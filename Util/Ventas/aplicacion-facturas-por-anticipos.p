DEF BUFFER F-CDOCU FOR ccbcdocu.

DEF TEMP-TABLE detalle
    FIELD codcia AS INT
    FIELD coddoc AS CHAR FORMAT 'x(3)'
    FIELD nrodoc AS CHAR FORMAT 'x(12)'
    FIELD fchdoc AS DATE
    FIELD flgest AS CHAR FORMAT 'x'
    FIELD coddiv AS CHAR FORMAT 'x(5)'
    FIELD imptot AS DEC FORMAT '>>>,>>>,>>9.99'
    FIELD imptot2 AS DEC FORMAT '->>>,>>>,>>9.99'
    FIELD codcli AS CHAR FORMAT 'x(11)'
    FIELD nomcli AS CHAR FORMAT 'x(60)'
    FIELD codref AS CHAR FORMAT 'x(3)'
    FIELD nroref AS CHAR FORMAT 'x(12)'
    FIELD fchapl AS DATE
    FIELD impapl AS DEC FORMAT '>>>,>>>,>>9.99'.


FOR EACH gn-divi NO-LOCK WHERE codcia = 1 AND coddiv = '00000':
    FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
        AND coddiv = gn-divi.coddiv
        AND coddoc = 'fac'
        AND fchdoc >= 11/01/10
        AND fchdoc <= 01/15/11
        AND flgest <> 'a'
        AND tpofac = 'A':
        FOR EACH F-CDOCU NO-LOCK WHERE F-CDOCU.codcia = 1
            AND F-CDOCU.coddoc = 'A/C'
            AND F-CDOCU.codref = ccbcdocu.coddoc
            AND F-CDOCU.nroref = ccbcdocu.nrodoc:
            FOR EACH ccbdcaja NO-LOCK WHERE ccbdcaja.codcia = 1
                AND ccbdcaja.coddoc = F-CDOCU.coddoc
                AND ccbdcaja.nrodoc = F-CDOCU.nrodoc:
                CREATE detalle.
                BUFFER-COPY ccbcdocu 
                    TO detalle
                    ASSIGN
                    detalle.codref = ccbdcaja.codref
                    detalle.nroref = ccbdcaja.nroref
                    detalle.fchapl = ccbdcaja.fchdoc
                    detalle.impapl = ccbdcaja.imptot.
            END.
        END.
        FOR EACH ccbdcaja NO-LOCK WHERE ccbdcaja.codcia = 1
            AND ccbdcaja.coddoc = ccbcdocu.coddoc
            AND ccbdcaja.nrodoc = ccbcdocu.nrodoc:
            CREATE detalle.
            BUFFER-COPY ccbcdocu 
                TO detalle
                ASSIGN
                detalle.codref = ccbdcaja.codref
                detalle.nroref = ccbdcaja.nroref
                detalle.fchapl = ccbdcaja.fchdoc
                detalle.impapl = ccbdcaja.imptot.
        END.
    END.
    
END.

DEF VAR x-canal AS CHAR FORMAT 'x(40)' NO-UNDO.
OUTPUT TO c:\tmp\anticipos.txt.
FOR EACH detalle:
    FIND gn-clie WHERE gn-clie.codcia = 0
        AND gn-clie.codcli = detalle.codcli
        NO-LOCK NO-ERROR.
    x-canal = ''.
    FIND almtabla WHERE almtabla.tabla = 'CN'
        AND almtabla.codigo = gn-clie.canal
        NO-LOCK NO-ERROR.
    IF AVAILABLE almtabla THEN x-canal = almtabla.nombre.
    DISPLAY
        detalle.coddiv
        '|'
        detalle.coddoc
        '|'
        detalle.nrodoc
        '|'
        detalle.fchdoc
        '|'
        detalle.codcli
        '|'
        detalle.nomcli
        '|'
        x-canal
        '|'
        detalle.imptot
        '|'
        detalle.codref
        '|'
        detalle.nroref
        '|'
        detalle.fchapl
        '|'
        detalle.impapl
        '|'
        WITH STREAM-IO NO-BOX WIDTH 320 NO-LABELS.
END.
OUTPUT CLOSE.
