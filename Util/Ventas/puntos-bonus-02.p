def var x-coddoc as char init 'fac,bol,tck,n/c'.
def var i as int.
DEF VAR x-TpoCmbCmp AS DEC NO-UNDO.
DEF VAR x-TpoCmbVta AS DEC NO-UNDO.
DEF VAR x-Propios AS DEC NO-UNDO.
DEF VAR x-Terceros AS DEC NO-UNDO.

DO i = 1 to 4:
    FOR each gn-divi no-lock where codcia = 001:
        FOR each ccbcdocu where codcia = 001
            and coddiv = gn-divi.coddiv
            and coddoc = entry(i,x-coddoc)
            and fchdoc >= 12/15/2009
            /*AND fchdoc <= 03/20/2010*/:
            puntos = 0.
            IF flgest = 'A' then next.
            IF ccbcdocu.coddoc = 'N/C' AND ccbcdocu.cndcre = 'N' THEN NEXT. /* NO Otros */
            IF ccbcdocu.coddiv <> '00000' THEN DO:
                FIND LAST Gn-Tcmb WHERE Gn-Tcmb.Fecha <= CcbCdocu.FchDoc NO-LOCK NO-ERROR.
                IF NOT AVAIL Gn-Tcmb THEN 
                    FIND FIRST Gn-Tcmb WHERE Gn-Tcmb.Fecha >= CcbCdocu.FchDoc NO-LOCK NO-ERROR.
                IF AVAIL Gn-Tcmb THEN 
                    ASSIGN
                        x-TpoCmbCmp = Gn-Tcmb.Compra
                        x-TpoCmbVta = Gn-Tcmb.Venta.
                ASSIGN
                    x-Propios = 0
                    x-Terceros = 0.
                FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK,
                    FIRST Almmmatg OF Ccbddocu NO-LOCK:
                    IF Almmmatg.CHR__02 = 'P' THEN x-Propios = x-Propios + Ccbddocu.ImpLin.
                    ELSE x-Terceros = x-Terceros + Ccbddocu.ImpLin.
                END.
                IF Ccbcdocu.codmon = 2 
                THEN ASSIGN
                        x-Propios = x-Propios * x-TpoCmbVta
                        x-Terceros = x-Terceros * x-TpoCmbVta.
                /* PUNTOS BONUS */
                Ccbcdocu.puntos = TRUNCATE(x-Propios * 1 / 100, 0) + TRUNCATE(x-Terceros *  0.5 / 100, 0).
            END.
            RELEASE ccbcdocu.
        END.            
    END.
END.
