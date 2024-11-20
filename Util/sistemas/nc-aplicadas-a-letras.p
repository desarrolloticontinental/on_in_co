DEF BUFFER letras FOR ccbcdocu.

OUTPUT TO d:\tmp\nc-aplicadas.txt.
PUT UNFORMATTED
    'Division|Fecha de IC|Numero de IC|Importe Aplicado|Tipo Doc|Numero|Fecha|Importe|Tipo Doc|Numero|Fecha'
    SKIP.
FOR EACH ccbccaja NO-LOCK WHERE codcia = 1
    AND coddoc = 'i/c'
    AND fchdoc >= 01/01/2018
    AND fchdoc <= DATE(03,01,2019)
    AND flgest <> 'A',
    EACH ccbdcaja OF ccbccaja NO-LOCK WHERE ccbdcaja.codref = 'LET',
    FIRST letras NO-LOCK WHERE letras.codcia = 1
    AND letras.coddoc = ccbdcaja.codref
    AND letras.nrodoc = ccbdcaja.nroref:
    FOR EACH ccbdmov NO-LOCK WHERE ccbdmov.codcia = 1
        AND ccbdmov.codref = ccbccaja.coddoc
        AND ccbdmov.nroref = ccbccaja.nrodoc
        AND ccbdmov.coddoc = 'N/C',
        FIRST ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 1
        AND ccbcdocu.coddoc = ccbdmov.coddoc
        AND ccbcdocu.nrodoc = ccbdmov.nrodoc:
        PUT UNFORMATTED
            ccbccaja.coddiv '|'
            ccbccaja.fchdoc '|'
            ccbccaja.nrodoc '|'
            CCBDMOV.ImpTot '|'
            ccbcdocu.coddoc '|'
            ccbcdocu.nrodoc '|'
            ccbcdocu.fchdoc '|'
            ccbcdocu.imptot '|'
            letras.coddoc '|'
            letras.nrodoc '|'
            letras.fchdoc
            SKIP.
    END.
END.
OUTPUT CLOSE.
