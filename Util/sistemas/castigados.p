DEF NEW SHARED VAR s-codcia AS INTE INIT 001.
DEF NEW SHARED VAR s-user-id AS CHAR INIT 'GCP-00'.

DEF VAR cCodDoc AS CHAR NO-UNDO.
DEF VAR cNroDoc AS CHAR NO-UNDO.
DEF VAR cLinea AS CHAR NO-UNDO.
DEF VAR dFecha AS DATE NO-UNDO.

dFecha = DATE(12,29,2023).

INPUT FROM d:\castigado.prn.
OUTPUT TO d:\errores-castigado.txt.
REPEAT:
    IMPORT UNFORMATTED cLinea.
    IF TRUE <> (cLinea > '') THEN LEAVE.
    RUN pDudosa (SUBSTRING(cLinea,1,5), SUBSTRING(cLinea,11), dFecha).
END.
INPUT CLOSE.
OUTPUT CLOSE.

/* *************** */
PROCEDURE pDudosa:  
/* *************** */
DEF INPUT PARAMETER pCodDoc AS CHAR.
DEF INPUT PARAMETER pNroDoc AS CHAR.
DEF INPUT PARAMETER pFecha AS DATE.  
  
  FIND ccbcdocu WHERE codcia = s-codcia
    AND coddoc = pCodDoc
    AND nrodoc = pNroDoc
    NO-LOCK NO-ERROR.
  IF NOT AVAILABLE ccbcdocu OR ccbcdocu.flgest <> 'J' THEN DO:
      PUT UNFORMATT pCodDoc ' ' pNroDoc SKIP.
      /*DISPLAY pCodDoc pNroDoc FORMAT 'x(20)'.*/
      RETURN.
  END.

  FIND CURRENT Ccbcdocu EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAILABLE Ccbcdocu THEN DO:
      PUT UNFORMATT pCodDoc ' ' pNroDoc SKIP.
      RETURN.
  END.

  ASSIGN
    CcbCDocu.FchUbiA = CcbCDocu.FchUbi
    CcbCDocu.FchAct = pFecha
    CcbCDocu.FchUbi = pFecha
    CcbCDocu.FlgEst = 'S'
    CcbCDocu.usuario = s-user-id.
  /* RHC 23.11.2010 CONTROL CONTABLE */
  CREATE CcbDMvto.
  ASSIGN
      CcbDMvto.CodCia = Ccbcdocu.codcia
      CcbDMvto.CodCli = Ccbcdocu.codcli
      CcbDMvto.CodDiv = Ccbcdocu.coddiv
      CcbDMvto.CodDoc = "DCA"       /* Dcumento Castigado */
      CcbDMvto.NroDoc = Ccbcdocu.nrodoc
      CcbDMvto.CodRef = Ccbcdocu.coddoc
      CcbDMvto.NroRef = Ccbcdocu.nrodoc
      CcbDMvto.FchEmi = pFecha
      CcbDMvto.ImpTot = Ccbcdocu.imptot
      CcbDMvto.usuario = s-user-id
      CcbDMvto.FlgCbd = NO.
  RELEASE Ccbcmvto.
  /* FIN DE CONTROL CONTABLE */
  RELEASE ccbcdocu.

END PROCEDURE.
