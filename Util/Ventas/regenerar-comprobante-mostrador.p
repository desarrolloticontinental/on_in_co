FIND faccpedi WHERE codcia = 001
    AND coddoc = 'P/M'
    AND nroped = '003604544'
    NO-LOCK.
CREATE ccbcdocu.
BUFFER-COPY faccpedi
    TO ccbcdocu
    ASSIGN
    ccbcdocu.coddoc = 'FAC'
    ccbcdocu.nrodoc = '002335096'.
FOR EACH facdpedi OF faccpedi NO-LOCK:
    CREATE ccbddocu.
    BUFFER-COPY facdpedi
        TO ccbddocu
        ASSIGN
        ccbddocu.coddoc = ccbcdocu.coddoc
        ccbddocu.nrodoc = ccbcdocu.nrodoc
        ccbddocu.candes = facdpedi.canped
        ccbddocu.undvta = facdpedi.undvta.
END.
{vta2/graba-totales-factura-cred.i}
ASSIGN
    Ccbcdocu.fchcan = Ccbcdocu.fchdoc
    Ccbcdocu.sdoact = 0
    Ccbcdocu.flgest = 'C'.
/* Descarga de Almacen */
RUN vta2\act_alm (ROWID(CcbCDocu)).


