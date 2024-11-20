&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 7.35
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DISABLE TRIGGERS FOR LOAD OF faccpedi.
DISABLE TRIGGERS FOR LOAD OF facdpedi.

DEF VAR s-codcia AS INT INIT 001.

DEFINE BUFFER B-DPEDI FOR FacDPedi.
DEFINE BUFFER B-CCOTI FOR FacCPedi.
DEFINE BUFFER B-CPEDI FOR FacCPedi.
DEFINE VAR pFactor AS INT INIT -1.    /* +1 actualiza    -1 desactualiza */
DEFINE VARIABLE I-NRO AS INTEGER INIT 0 NO-UNDO.
DEFINE VARIABLE pMensaje AS CHAR NO-UNDO.
DEFINE VARIABLE s-user-id AS CHAR INIT "SYSTEM" NO-UNDO.

/* RHC 11.06.2012 BORRADO DE COTIZACIONES, PEDIDOS Y ORDENES VENCIDAS */

/* VENTA CONTADO */
RUN Cierra-Pedidos-Contado.

/* CIERRE DE PEDIDOS DE VENTAS */
RUN Cierra-Pedidos-Credito.

/* BORRA PEDIDOS CONTADO CON MAS DE DOS AÑOS DE ANTIGUEDAD */
 RUN Borra-Pedidos-Contado.


QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Borra-Pedidos-Contado) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borra-Pedidos-Contado Procedure 
PROCEDURE Borra-Pedidos-Contado :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Borra todos los pedidos contado con mas de dos años de antiguedad
------------------------------------------------------------------------------*/

DEF VAR x-Fecha-Limite AS DATE NO-UNDO.
x-Fecha-Limite = ADD-INTERVAL(TODAY, -2, 'years').

/* VENTA CONTADO */
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia,
    EACH Faccpedi NO-LOCK WHERE Faccpedi.codcia = s-codcia
    AND Faccpedi.coddiv = gn-divi.coddiv
    AND Faccpedi.coddoc = "P/M"
    AND Faccpedi.fchped < x-Fecha-Limite:
    FIND B-CPEDI WHERE ROWID(B-CPEDI) = ROWID(Faccpedi) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF NOT AVAILABLE B-CPEDI THEN NEXT.
    DISPLAY faccpedi.coddiv faccpedi.fchped faccpedi.coddoc faccpedi.nroped faccpedi.flgest
        WITH STREAM-IO NO-BOX WIDTH 200.
    PAUSE 0.
    FOR EACH Facdpedi OF B-CPEDI EXCLUSIVE-LOCK:
        DELETE Facdpedi.
    END.
    DELETE B-CPEDI.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Cierra-Pedidos-Contado) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cierra-Pedidos-Contado Procedure 
PROCEDURE Cierra-Pedidos-Contado :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* VENTA CONTADO */
FOR EACH GN-DIVI NO-LOCK WHERE gn-divi.codcia = s-codcia,
    EACH Faccpedi NO-LOCK WHERE Faccpedi.codcia = s-codcia
    AND Faccpedi.coddiv = gn-divi.coddiv
    AND Faccpedi.coddoc = "P/M"
    AND LOOKUP(Faccpedi.flgest, "P,A") > 0
    AND Faccpedi.fchped < TODAY:
    FIND B-CPEDI WHERE ROWID(B-CPEDI) = ROWID(Faccpedi) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF NOT AVAILABLE B-CPEDI THEN NEXT.
    DISPLAY faccpedi.coddiv faccpedi.fchped faccpedi.coddoc faccpedi.nroped faccpedi.flgest
        WITH STREAM-IO NO-BOX WIDTH 200.
    PAUSE 0.
    FOR EACH Facdpedi OF B-CPEDI EXCLUSIVE-LOCK:
        DELETE Facdpedi.
    END.
    DELETE B-CPEDI.
END.
/* RHC 29.11.2010 Borramos pre-pedidos vitrinas PPV */
FOR EACH faccpedi NO-LOCK WHERE faccpedi.codcia = s-codcia
    AND faccpedi.coddoc = 'PPV'
    AND faccpedi.fchped < TODAY:
    FIND B-CPEDI WHERE ROWID(B-CPEDI) = ROWID(Faccpedi) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF NOT AVAILABLE B-CPEDI THEN NEXT.
    DISPLAY faccpedi.coddiv faccpedi.fchped faccpedi.coddoc faccpedi.nroped faccpedi.flgest
        WITH STREAM-IO NO-BOX WIDTH 200.
    PAUSE 0.
    FOR EACH facdpedi OF faccpedi EXCLUSIVE-LOCK:
        DELETE facdpedi.
    END.
    DELETE B-CPEDI. 
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Cierra-Pedidos-Credito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cierra-Pedidos-Credito Procedure 
PROCEDURE Cierra-Pedidos-Credito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

PEDIDOS:
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia,
    EACH FacCPedi EXCLUSIVE-LOCK WHERE faccpedi.codcia = s-codcia AND 
    faccpedi.coddiv = gn-divi.coddiv AND 
    LOOKUP(faccpedi.flgest, 'G,P,X,W,WX,WL') > 0 AND
    faccpedi.coddoc = 'PED' AND 
    FacCPedi.FchVen < TODAY:
    FIND FIRST Facdpedi OF Faccpedi WHERE Facdpedi.canate > 0 NO-LOCK NO-ERROR.
    IF AVAILABLE Facdpedi THEN NEXT.
    DISPLAY faccpedi.coddiv faccpedi.fchped faccpedi.coddoc faccpedi.nroped faccpedi.flgest
        WITH STREAM-IO NO-BOX WIDTH 200.
    PAUSE 0.
    ASSIGN
        /*FacCPedi.FlgEst = "E"*/   /* 10/07/23 S.Leon CERRADO POR VENCIMIENTO */
        FacCPedi.FlgEst = "V"
        FacCPedi.Libre_f01 = TODAY.
    FOR EACH Facdpedi OF Faccpedi EXCLUSIVE-LOCK WHERE Facdpedi.Libre_c05 <> "OF":
        /* BORRAMOS SALDO EN LAS COTIZACIONES */
        FIND FIRST B-DPEDI WHERE B-DPEDI.CodCia = Faccpedi.CodCia 
            AND B-DPEDI.CodDiv = Faccpedi.CodDiv
            AND B-DPEDI.CodDoc = Faccpedi.CodRef    /* COT */
            AND B-DPEDI.NroPed = Faccpedi.NroRef
            AND B-DPEDI.CodMat = Facdpedi.CodMat 
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF NOT AVAILABLE B-DPEDI THEN DO:
            DISPLAY 'ERROR:' faccpedi.coddiv faccpedi.coddoc faccpedi.nroped facdpedi.codmat
                WITH STREAM-IO NO-BOX WIDTH 200.
            PAUSE 0.
            UNDO PEDIDOS, NEXT PEDIDOS.
        END.
        ASSIGN
            B-DPEDI.FlgEst = 'P'
            B-DPEDI.CanAte = B-DPEDI.CanAte - (Facdpedi.CanPed - Facdpedi.CanAte).
        ASSIGN
            Facdpedi.FlgEst = Faccpedi.FlgEst.   /* <<< OJO <<< */
    END.
    FIND FIRST B-CCOTI WHERE B-CCOTI.CodCia = Faccpedi.CodCia 
        AND B-CCOTI.CodDiv = Faccpedi.CodDiv
        AND B-CCOTI.CodDoc = Faccpedi.CodRef        /* COT */
        AND B-CCOTI.NroPed = Faccpedi.NroRef
        EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF NOT AVAILABLE B-CCOTI THEN DO:
        DISPLAY 'ERROR:' faccpedi.coddiv faccpedi.coddoc faccpedi.nroped 
            faccpedi.codref faccpedi.nroref
            WITH STREAM-IO NO-BOX WIDTH 200.
        PAUSE 0.
        UNDO PEDIDOS, NEXT.
    END.
    IF AVAILABLE B-CCOTI THEN B-CCOTI.FlgEst = "P".
    IF AVAILABLE(B-DPEDI) THEN RELEASE B-DPEDI.
    IF AVAILABLE(B-CCOTI) THEN RELEASE B-CCOTI.
END.
/* VENCIMIENTO DE COTIZACIONES */
/* VENTA CONTADO */
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia,
    EACH FacCPedi EXCLUSIVE-LOCK WHERE faccpedi.codcia = s-codcia AND 
    faccpedi.coddiv = gn-divi.coddiv AND 
    faccpedi.flgest = "P" AND
    faccpedi.coddoc = 'C/M' AND 
    FacCPedi.FchVen < TODAY:
    FIND FIRST Facdpedi OF Faccpedi WHERE Facdpedi.CanAte > 0 NO-LOCK NO-ERROR.
    IF AVAILABLE Facdpedi THEN NEXT.
    DISPLAY faccpedi.coddiv faccpedi.fchped faccpedi.coddoc faccpedi.nroped faccpedi.flgest
        WITH STREAM-IO NO-BOX WIDTH 200.
    PAUSE 0.
    ASSIGN
        Faccpedi.FlgEst = "V"
        FacCPedi.Libre_f01 = TODAY.
END.
/* VENTA CREDITO */
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia,
    EACH FacCPedi NO-LOCK WHERE faccpedi.codcia = s-codcia AND 
    faccpedi.coddiv = gn-divi.coddiv AND 
    faccpedi.flgest = "P" AND
    faccpedi.coddoc = 'COT' AND 
    FacCPedi.FchVen < TODAY:
    FIND FIRST Facdpedi OF Faccpedi WHERE Facdpedi.CanAte > 0 NO-LOCK NO-ERROR.
    IF AVAILABLE Facdpedi THEN NEXT.
    FIND B-CPEDI WHERE ROWID(B-CPEDI) = ROWID(Faccpedi) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF NOT AVAILABLE B-CPEDI THEN NEXT.
    DISPLAY faccpedi.coddiv faccpedi.fchped faccpedi.coddoc faccpedi.nroped faccpedi.flgest
        WITH STREAM-IO NO-BOX WIDTH 200.
    PAUSE 0.
    ASSIGN
        B-CPEDI.FlgEst = "V"
        B-CPEDI.Libre_f01 = TODAY.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pactualizacotizacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pactualizacotizacion Procedure 
PROCEDURE pactualizacotizacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER pRowid AS ROWID.
DEF INPUT PARAMETER pTipo  AS CHAR.     /* D: descarga(-)  C: carga(+) */
DEF OUTPUT PARAMETER pError AS CHAR NO-UNDO.

/* ACTUALIZA LA COTIZACION EN BASE AL PEDIDO AL CREDITO */
DEFINE BUFFER B-DPEDI FOR FacDPedi.
DEFINE BUFFER B-CPEDI FOR FacCPedi.

FIND Faccpedi WHERE ROWID(Faccpedi) = pRowid NO-LOCK NO-ERROR.
IF NOT AVAILABLE Faccpedi THEN RETURN 'ADM-ERROR'.
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
      {lib/lock-wait.i &Tabla=B-CPedi &Condicion="B-CPedi.CodCia=FacCPedi.CodCia ~
          AND B-CPedi.CodDiv=FacCPedi.CodDiv ~
          AND B-CPedi.CodDoc=FacCPedi.CodRef ~
          AND B-CPedi.NroPed=FacCPedi.NroRef"}

      /* NO ACTUALIZAMOS LAS PROMOCIONES */
      FOR EACH Facdpedi OF Faccpedi NO-LOCK WHERE Facdpedi.Libre_c05 <> "OF",
          FIRST B-DPedi OF B-CPedi WHERE B-DPedi.CodMat = Facdpedi.CodMat:
          CASE pTipo:
              WHEN "D" THEN B-DPedi.CanAte = B-DPedi.CanAte - Facdpedi.CanPed.
              WHEN "C" THEN DO:
                  B-DPedi.CanAte = B-DPedi.CanAte + Facdpedi.CanPed.
                  /* CONTROL DE ATENCIONES */
                  IF B-DPEDI.CanAte > B-DPEDI.CanPed THEN DO:
                      pError = 'Se ha detectado un error el el producto ' + B-DPEDI.codmat + CHR(10) +
                      'Los despachos superan a lo cotizado' + CHR(10) +
                      'Cant. cotizada: ' + STRING(B-DPEDI.CanPed, '->>>,>>9.99') + CHR(10) +
                      'Total pedidos : ' + STRING(B-DPEDI.CanAte, '->>>,>>9.99') + CHR(10) +
                      'FIN DEL PROCESO'.
                      UNDO, RETURN "ADM-ERROR".
                  END.
              END.
          END CASE.
      END.
      /* RHC 11/08/2014 Solicitu de Transferencia STR */
      IF B-CPEDI.CodDoc = "STR" THEN DO:
          /* Actualizamos R/A */
          FIND Almcrepo WHERE Almcrepo.codcia = Faccpedi.codcia
              AND Almcrepo.codalm = Faccpedi.CodCli
              AND Almcrepo.nroser = INTEGER(SUBSTRING(B-CPEDI.NroRef,1,3))
              AND Almcrepo.nrodoc = INTEGER(SUBSTRING(B-CPEDI.NroRef,4))
              EXCLUSIVE-LOCK NO-ERROR.
          IF NOT AVAILABLE Almcrepo THEN DO:
              pError = 'No se pudo actualizar la ' + B-CPEDI.CodRef + ' ' + B-CPEDI.NroRef + CHR(10) +
                      'FIN DEL PROCESO'.
                      UNDO, RETURN "ADM-ERROR".
          END.
          FOR EACH Facdpedi OF Faccpedi NO-LOCK, FIRST Almdrepo OF Almcrepo WHERE Almdrepo.codmat = Facdpedi.codmat:
              CASE pTipo:
                  WHEN "D" THEN Almdrepo.CanAten = Almdrepo.CanAten - Facdpedi.CanPed.
                  WHEN "C" THEN Almdrepo.CanAten = Almdrepo.CanAten + Facdpedi.CanPed.
              END CASE.
          END.
          ASSIGN
              Almcrepo.FlgEst = "C".
          FIND FIRST Almdrepo OF Almcrepo WHERE Almdrepo.CanAte < Almdrepo.CanApr NO-LOCK NO-ERROR.
          IF AVAILABLE Almdrepo THEN Almcrepo.FlgEst = "P".
      END.
      /* ACTUALIZAMOS FLAG DE LA COTIZACION */
      B-CPedi.FlgEst = "C".
      FIND FIRST B-DPedi OF B-CPedi WHERE B-DPedi.CanAte < B-DPedi.CanPed NO-LOCK NO-ERROR.
      IF AVAILABLE B-DPedi THEN B-CPedi.FlgEst = "P".
      RELEASE B-CPedi.
      IF AVAILABLE Almcrepo THEN RELEASE Almcrepo.
END.
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

