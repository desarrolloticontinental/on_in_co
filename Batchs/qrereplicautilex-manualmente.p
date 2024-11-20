DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF NEW SHARED VAR cl-codcia AS INT INIT 000.
DEF NEW SHARED VAR s-coddiv AS CHAR INIT '00503'.

DEF VAR x-FchDoc-1 AS DATE NO-UNDO.
DEF VAR x-FchDoc-2 AS DATE NO-UNDO.

/* Debe ser el mes pasado */
ASSIGN
    x-FchDoc-1 = 05/06/2015
    x-FchDoc-2 = 05/06/2015.
DISABLE TRIGGERS FOR LOAD OF ccbcdocu.
DISABLE TRIGGERS FOR LOAD OF ccbddocu.

/* 1ro barremos comprobantes */
FOR EACH ccbcdocu WHERE ccbcdocu.codcia = s-codcia
    AND LOOKUP(ccbcdocu.coddiv, s-coddiv) > 0
    AND ccbcdocu.coddoc = 'TCK'
    AND ccbcdocu.fchdoc >= x-fchdoc-1
    AND ccbcdocu.fchdoc <= x-fchdoc-2:
    DISPLAY ccbcdocu.fchdoc ccbcdocu.coddoc ccbcdocu.nrodoc.
    PAUSE 0.
    
/*     FOR EACH ccbdcaja WHERE ccbdcaja.codcia = s-codcia */
/*         AND ccbdcaja.codref = ccbcdocu.coddoc          */
/*         AND ccbdcaja.nroref = ccbcdocu.nrodoc:         */
/*         ASSIGN ccbdcaja.flgcbd = NOT ccbdcaja.flgcbd.  */
/*     END.                                               */
    
/*     ASSIGN                          */
/*         ccbcdocu.libre_f05 = TODAY. */
    {aplic/vtamin/graba-totales-fac.i}

END.
