DEF VAR x-divisiones AS CHAR INIT '00000,80014,00021'.
DEF VAR k AS INTE.

OUTPUT TO d:\informacion.txt.
DO k = 1 TO NUM-ENTRIES(x-divisiones):
    FOR EACH di-rutac NO-LOCK WHERE di-rutac.codcia = 1
        AND di-rutac.coddiv = ENTRY(k, x-divisiones)
        AND di-rutac.coddoc = 'H/R'
        AND di-rutac.fchdoc >= DATE(10,01,2022)
        AND di-rutac.flgest <> 'A',
        FIRST gn-prov NO-LOCK WHERE gn-prov.codcia = 0
        AND gn-prov.codpro = DI-RutaC.CodPro,
        EACH di-rutad OF di-rutac NO-LOCK,
        FIRST ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 1
        AND ccbcdocu.coddoc = di-rutad.codref
        AND ccbcdocu.nrodoc = di-rutad.nroref:
        PUT UNFORMATTED
            di-rutac.coddiv FORMAT 'x(10)' ';'
            di-rutac.coddoc ';'
            di-rutac.nrodoc ';'
            di-rutac.fchdoc ';'
            ccbcdocu.coddoc ';'
            ccbcdocu.nrodoc ';'
            ccbcdocu.fchdoc ';'
            ccbcdocu.libre_c01 FORMAT 'x(10)' ';'
            ccbcdocu.libre_c02 FORMAT 'x(20)' ';'
            gn-prov.NomPro ';'
            gn-prov.Ruc ';'
            SKIP.

    END.
END.
OUTPUT CLOSE.
