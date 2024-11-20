/* GENERACION DE CONTROLES DE PERCEPCION */
DEF BUFFER CDOCU FOR Ccbcdocu.
DEF VAR x-ImpTot LIKE Ccbcdocu.imptot NO-UNDO.

DEF VAR FechaD AS DATE NO-UNDO.
DEF VAR FechaH AS DATE NO-UNDO.

ASSIGN
    FechaD = 01/01/2015
    FechaH = 02/28/2015.

FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND coddoc = 'N/C'
    AND fchdoc >= FechaD AND fchdoc <= FechaH
    AND CcbCdocu.cndcre = "D"
    AND flgest <> 'A':

    x-ImpTot = 0.
    FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK:
        x-ImpTot = x-ImpTot + CcbDDocu.ImpDcto_Adelanto[5].
    END.
    IF x-ImpTot <= 0 THEN NEXT.

    FIND FIRST CDOCU WHERE CDOCU.codcia = Ccbcdocu.codcia
        AND CDOCU.coddiv = Ccbcdocu.coddiv
        AND CDOCU.coddoc = "PRA"
        AND CDOCU.codref = Ccbcdocu.coddoc
        AND CDOCU.nroref = Ccbcdocu.nrodoc
        NO-LOCK NO-ERROR.
    IF AVAILABLE CDOCU THEN NEXT.

    FIND FIRST CDOCU WHERE CDOCU.codcia = Ccbcdocu.codcia
        AND CDOCU.coddoc = Ccbcdocu.codref
        AND CDOCU.nrodoc = Ccbcdocu.nroref
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE CDOCU THEN NEXT.
    IF LOOKUP(CDOCU.TpoFac, 'A,S') > 0 THEN NEXT.

    DISPLAY ccbcdocu.fchdoc ccbcdocu.coddiv ccbcdocu.coddoc ccbcdocu.nrodoc.
    PAUSE 0.

    RUN vta2/control-percepcion-abonos ( ROWID(Ccbcdocu) ).

END.
