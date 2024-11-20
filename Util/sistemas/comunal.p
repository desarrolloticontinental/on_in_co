DISABLE TRIGGERS FOR LOAD OF ccbcdocu.
DISABLE TRIGGERS FOR LOAD OF faccpedi.

DEFINE NEW SHARED VAR s-user-id AS CHAR INIT 'MRC-00'.

FIND faccpedi WHERE faccpedi.codcia = 1 
    AND faccpedi.coddoc = 'o/d' 
    AND faccpedi.nroped = '002035456'.
FIND ccbcdocu WHERE ccbcdocu.codcia = 1 
    AND ccbcdocu.coddoc = 'fac' 
    AND ccbcdocu.nrodoc = '28300011940'.
ASSIGN
    faccpedi.flgest = "C".
ASSIGN
    ccbcdocu.libre_c01 = faccpedi.coddoc
    ccbcdocu.libre_c02 = faccpedi.nroped
    ccbcdocu.codped = faccpedi.codref
    ccbcdocu.nroped = faccpedi.nroref.
/* *********************************************************************** */
/* ACTUALIZAMOS ALMACENES */
/* *********************************************************************** */
DEF VAR pMensaje AS CHAR NO-UNDO.

RUN vta2/act_almv2.r ( INPUT ROWID(CcbCDocu), OUTPUT pMensaje ).
IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
    IF TRUE <> (pMensaje > "") THEN pMensaje = "ERROR: NO se pudo actualizar el Kardex".
    MESSAGE pMensaje VIEW-AS ALERT-BOX ERROR.
    UNDO, RETURN ERROR.
END.
/* *********************************************************************** */



