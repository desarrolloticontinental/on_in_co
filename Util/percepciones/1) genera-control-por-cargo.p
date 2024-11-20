/* GENERACION DE CONTROLES DE PERCEPCION */
DEF VAR x-ImpTot LIKE Ccbcdocu.imptot NO-UNDO.
DEF VAR FechaD AS DATE NO-UNDO.
DEF VAR FechaH AS DATE NO-UNDO.

DEF BUFFER CDOCU FOR Ccbcdocu.

ASSIGN
    FechaD = 01/01/2015
    FechaH = 02/28/2015.

FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND LOOKUP(coddoc, 'FAC,BOL,TCK') > 0
    AND fchdoc >= FechaD AND fchdoc <= FechaH
    AND LOOKUP(CcbCdocu.TpoFac, 'A,S') = 0
    AND flgest <> 'A':

    x-ImpTot = 0.
    FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK:
        x-ImpTot = x-ImpTot + CcbDDocu.ImpDcto_Adelanto[5].
    END.
    IF x-ImpTot <= 0 THEN NEXT.

    FIND FIRST CDOCU WHERE CDOCU.codcia = Ccbcdocu.codcia
        AND CDOCU.coddiv = Ccbcdocu.coddiv
        AND CDOCU.coddoc = "PRC"
        AND CDOCU.codref = Ccbcdocu.coddoc
        AND CDOCU.nroref = Ccbcdocu.nrodoc
        NO-LOCK NO-ERROR.
    IF AVAILABLE CDOCU THEN NEXT.
    DISPLAY ccbcdocu.coddiv ccbcdocu.fchdoc ccbcdocu.coddoc ccbcdocu.nrodoc.
    PAUSE 0.

    RUN vta2/control-percepcion-cargos ( ROWID(Ccbcdocu) ).

END.
