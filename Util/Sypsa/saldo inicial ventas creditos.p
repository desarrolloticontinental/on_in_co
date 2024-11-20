/* saldo inicial por cuentas por cobrar */
DEF VAR s-codcia AS INT INIT 001.
DEF VAR x-codcta AS CHAR.
DEF VAR x-tpomov AS CHAR.

OUTPUT TO c:\tmp\saldoventasconti-xcredito.txt.
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia:
    FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia
        AND ccbcdocu.coddiv = gn-divi.coddiv
        AND LOOKUP(ccbcdocu.coddoc, 'FAC,BOL,LET,N/C,N/D') > 0
        AND flgest = 'P',
        FIRST Facdocum OF Ccbcdocu NO-LOCK:
        FIND Cb-cfgrv WHERE Cb-cfgrv.Codcia = Ccbcdocu.Codcia 
            AND Cb-cfgrv.CodDiv = Ccbcdocu.Coddiv 
            AND Cb-cfgrv.Coddoc = Ccbcdocu.Coddoc 
            AND Cb-cfgrv.Fmapgo = Ccbcdocu.Fmapgo 
            AND Cb-cfgrv.Codmon = Ccbcdocu.Codmon 
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Cb-cfgrv OR Cb-cfgrv.Codcta = "" 
            THEN x-codcta = (IF ccbcdocu.codmon = 1 THEN FacDocum.CodCta[1]
            ELSE FacDocum.CodCta[2]).
        ELSE x-codcta = Cb-cfgrv.codcta.
             x-tpomov = 'C'.

        IF ccbcdocu.coddoc = 'N/C' THEN x-tpomov = 'A'.
        /* Genererar texto */
        FIND LAST gn-tcmb WHERE gn-tcmb.fecha <= ccbcdocu.fchdoc NO-LOCK.
        PUT UNFORMATTED
            ccbcdocu.codcli '|'
            ccbcdocu.fmapgo '|'
            ccbcdocu.coddoc '|'
            ccbcdocu.nrodoc '|'
            ccbcdocu.fchdoc '|'
            ccbcdocu.codmon '|'
            '|'
            '2' '|'
            ccbcdocu.sdoact '|'
            x-codcta '|'
            x-tpomov '|'
            FILL('|', 9)
            ccbcdocu.sdoact '|'
            x-codcta '|'
            (IF x-tpomov = 'C' THEN 'A' ELSE 'C') '|'
            ccbcdocu.fchvto '|'
            gn-tcmb.venta 
            SKIP.
    END.
END.
OUTPUT CLOSE.

