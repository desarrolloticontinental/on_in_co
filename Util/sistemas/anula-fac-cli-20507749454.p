DEF NEW SHARED VAR s-codcia AS INTE INIT 001.
DEF NEW SHARED VAR s-coddiv AS CHAR INIT '00000'.
DEF NEW SHARED VAR s-nroser AS INTE INIT 284.
DEF NEW SHARED VAR s-codalm AS CHAR INIT '11'.

DEF VAR x-Articulo-ICBPer AS CHAR INIT '099268'.

FIND FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK NO-ERROR.
      
FOR EACH ccbcdocu EXCLUSIVE-LOCK WHERE codcia = 1 
    and coddoc = 'fac' 
    and nrodoc >= '28400000062' 
    and nrodoc <= '28400000071':
    ASSIGN 
        ccbcdocu.FlgEst = "A"
        ccbcdocu.SdoAct = 0
        ccbcdocu.UsuAnu = 'SYSTEM'
        ccbcdocu.FchAnu = TODAY
        ccbcdocu.Glosa  = "A N U L A D O".
      
      /* DESCARGA ALMACENES */
      RUN vta2/des_alm (ROWID(CcbCDocu)).
      IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
END.
      
