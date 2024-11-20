DEFINE VARIABLE s-aplic-fact-ade AS LOG NO-UNDO.    /* Aplicacion de la factura adelantada */
DEFINE VARIABLE s-asigna-pedido AS CHAR INIT 'NO' NO-UNDO.
DEFINE VARIABLE s-asigna-guia   AS CHAR INIT 'NO' NO-UNDO.
DEFINE VARIABLE s-user-id AS CHAR INIT 'SISTEMAS'.

DEFINE VAR s-codcia AS INT INIT 001.
  
DEF VAR x-linea AS CHAR FORMA 'x(9)'.

DEFINE BUFFER B-CDOCU FOR CcbCDocu.
DEFINE BUFFER B-CCDOCU FOR CcbCDocu.


INPUT FROM c:\tmp\factsaga.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    DISPLAY x-linea.
    PAUSE 0.
    FIND ccbcdocu WHERE codcia = s-codcia
        AND coddoc = 'fac'
        AND nrodoc = x-linea
        NO-LOCK NO-ERROR.
    IF AVAILABLE ccbcdocu THEN DO:
        RUN borra-cabecera.
    END.
END.
INPUT CLOSE.

RETURN.

PROCEDURE borra-cabecera:

  DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN 'ADM-ERROR':

      ASSIGN
          s-asigna-guia = CcbCDocu.Libre_c01
          s-asigna-pedido = CcbCDocu.Libre_c02.
      /* TRACKING FACTURAS */
      IF s-asigna-guia = 'YES' THEN DO:
          FIND Faccpedi WHERE Faccpedi.codcia = s-codcia
              AND Faccpedi.coddoc = Ccbcdocu.codped     /* PED */
              AND Faccpedi.nroped = Ccbcdocu.nroped
              NO-LOCK NO-ERROR.
          IF NOT AVAILABLE Faccpedi THEN UNDO, RETURN 'ADM-ERROR'.
          FIND Almacen OF Ccbcdocu NO-LOCK.
          RUN gn/pTracking-01 (s-CodCia,
                            Almacen.CodDiv,
                            Faccpedi.CodDoc,
                            Faccpedi.NroPed,
                            s-User-Id,
                            'EFAC',
                            'A',
                            DATETIME(TODAY, MTIME),
                            DATETIME(TODAY, MTIME),
                            Ccbcdocu.coddoc,
                            Ccbcdocu.nrodoc,
                            Ccbcdocu.codref,
                            Ccbcdocu.nroref).
          IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
      END.

      /* Eliminamos el detalle */
      RUN Borra-Detalle.
      IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
     
      IF s-asigna-guia = 'YES' THEN DO:
          FOR EACH B-CDOCU WHERE B-CDOCU.CodCia = S-CODCIA 
              AND B-CDOCU.CodDoc = "G/R"    
              AND B-CDOCU.FlgEst = "F"      
              AND B-CDOCU.CodRef = CcbCDocu.CodDoc 
              AND B-CDOCU.NroRef = CcbCDocu.NroDoc:
              ASSIGN 
                 B-CDOCU.FlgEst = "P"
                 B-CDOCU.SdoAct = B-CDOCU.ImpTot.
          END.   
          RELEASE B-CDOCU.
      END.

      IF s-asigna-pedido = 'YES' THEN DO:
         FIND FacCPedi WHERE FacCPedi.CodCia = CcbCDocu.CodCia 
             AND FacCPedi.CodDoc = CcbCDocu.CodPed 
             AND FacCPedi.NroPed = CcbCDocu.NroPed EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE FacCPedi THEN DO:
             ASSIGN FacCPedi.FlgEst = "P".
             FOR EACH FacDPedi OF FacCPedi :
                 Facdpedi.flgest = Faccpedi.flgest.
             END.
             RELEASE FacCPedi.
         END.
      END.

      FIND B-CDOCU WHERE ROWID(B-CDOCU) = ROWID(CcbCDocu) EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABL B-CDOCU THEN UNDO, RETURN 'ADM-ERROR'.
      ASSIGN 
          B-CDOCU.FlgEst = "A"
          B-CDOCU.SdoAct = 0
          B-CDOCU.UsuAnu = S-USER-ID
          B-CDOCU.FchAnu = TODAY
          B-CDOCU.Glosa  = "A N U L A D O".
     RELEASE B-CDOCU.
  END.

END PROCEDURE.


  PROCEDURE borra-detalle:

      FOR EACH CcbDDocu WHERE CcbDDocu.CodCia = CcbCDocu.CodCia 
          AND CcbDDocu.CodDoc = CcbCDocu.CodDoc 
          AND CcbDDocu.NroDoc = CcbCDocu.NroDoc 
          ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN 'ADM-ERROR':

          DELETE CcbDDocu.

      END.


END PROCEDURE.
