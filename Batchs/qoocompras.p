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

DEF VAR s-codcia AS INTE INIT 001 NO-UNDO.
DEF VAR pv-codcia AS INTE INIT 000 NO-UNDO.
DEF VAR s-TpoDoc AS CHAR INIT 'N' NO-UNDO.      /* Normal */
DEF VAR s-FlgSit AS CHAR INIT 'T' NO-UNDO.      /* Atención Total */
DEF VAR s-CodDiv AS CHAR INIT '00000' NO-UNDO.
DEF VAR s-CodDoc AS CHAR INIT 'O/C' NO-UNDO.
DEF VAR s-CodAlm AS CHAR NO-UNDO.
DEF VAR x-NroRf2 AS CHAR NO-UNDO.
DEF VAR x-NroRf3 AS CHAR NO-UNDO.
DEF VAR s-user-id AS CHAR NO-UNDO.
DEF VAR pRowid AS ROWID NO-UNDO.

DEF TEMP-TABLE T-COCMP LIKE Lg-cocmp INDEX llave01 AS PRIMARY UNIQUE NroDoc.
DEF TEMP-TABLE T-DOCMP LIKE Lg-docmp INDEX llave01 AS PRIMARY NroDoc CodMat.

DISABLE TRIGGERS FOR LOAD OF almdmov.
DISABLE TRIGGERS FOR LOAD OF almcmov.
DISABLE TRIGGERS FOR LOAD OF almmmate.
DISABLE TRIGGERS FOR LOAD OF almacen.
DISABLE TRIGGERS FOR LOAD OF Faccpedi.
DISABLE TRIGGERS FOR LOAD OF Facdpedi.
DISABLE TRIGGERS FOR LOAD OF Faccorre.

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
         HEIGHT             = 7.15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DISPLAY 'Inicio:' TODAY STRING(TIME, 'HH:MM:SS') SKIP
    WITH STREAM-IO NO-BOX NO-LABELS.
PAUSE 0.
FOR EACH OpenCCompras WHERE OpenCCompras.CodCia = s-codcia
    AND OpenCCompras.TpoDoc = s-TpoDoc
    AND OpenCCompras.CodDiv = s-CodDiv
    AND OpenCCompras.FlagMigracion = 'N'
    TRANSACTION ON ERROR UNDO, NEXT ON STOP UNDO, NEXT:
    FIND FIRST Lg-corr WHERE LG-CORR.CodCia = S-CODCIA
        AND LG-CORR.CodDiv = s-CodDiv
        AND LG-CORR.CodDoc = s-CodDoc
        EXCLUSIVE-LOCK NO-WAIT.
    IF NOT AVAILABLE Lg-corr THEN NEXT.
    DISPLAY
        'PROCESANDO: Orden' OpenCCompras.NroDoc FORMAT '999999999' SKIP
        WITH STREAM-IO NO-BOX NO-LABELS.
    PAUSE 0.
    /* Creamos Cabecera de Compras */
    EMPTY TEMP-TABLE T-COCMP.
    EMPTY TEMP-TABLE T-DOCMP.
    CREATE Lg-cocmp.
    BUFFER-COPY OpenCCompras
        TO Lg-cocmp
        ASSIGN
        LG-COCmp.NroDoc = LG-CORR.NroDoc
        LG-COCmp.FlgSit = s-FlgSit
        LG-COCmp.TpoDoc = s-TpoDoc.
    ASSIGN
        LG-CORR.NroDoc = LG-CORR.NroDoc + 1.
    ASSIGN
        Lg-cocmp.Libre_c03 = STRING(OpenCCompras.NroDoc)
        pRowid = ROWID(Lg-cocmp).
    CREATE T-COCMP.
    BUFFER-COPY Lg-cocmp TO T-COCMP.
    /* Creamos Detalle */
    FOR EACH OpenDCompras OF OpenCCompras NO-LOCK:
        CREATE Lg-docmp.
        BUFFER-COPY OpenDCompras
            TO Lg-docmp
            ASSIGN Lg-docmp.nrodoc = Lg-cocmp.nrodoc.
        ASSIGN
            Lg-docmp.canaten = Lg-docmp.canpedi.
        CREATE T-DOCMP.
        BUFFER-COPY Lg-docmp TO T-DOCMP.
    END.
    /* Ingreso al Almacen */
    /*MESSAGE 'ingreso almacen' VIEW-AS ALERT-BOX.*/
    RUN Ingreso-Almacen NO-ERROR.
    IF ERROR-STATUS:ERROR = YES THEN UNDO, NEXT.

    RUN Orden-de-Transferencia.
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, NEXT.

    /* Cerramos */
    /*MESSAGE 'cerramos' VIEW-AS ALERT-BOX.*/
    ASSIGN
        OpenCCompras.Libre_c01 = STRING(Lg-cocmp.nrodoc)
        OpenCCompras.FlagMigracion = 'S'.
END.
IF AVAILABLE(OpenCCompra) THEN RELEASE OpenCCompra.
IF AVAILABLE(OpenDCompra) THEN RELEASE OpenDCompra.
IF AVAILABLE(Lg-cocmp) THEN RELEASE Lg-cocmp.
IF AVAILABLE(Lg-docmp) THEN RELEASE Lg-docmp.
IF AVAILABLE(Lg-corr) THEN RELEASE Lg-corr.

DISPLAY 'Final:' TODAY STRING(TIME, 'HH:MM:SS') SKIP
    WITH STREAM-IO NO-BOX NO-LABELS.
PAUSE 0.
QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ALMACPR1-ING) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ALMACPR1-ING Procedure 
PROCEDURE ALMACPR1-ING :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER R-DMov AS ROWID.
DEFINE INPUT PARAMETER C-UD   AS CHAR.
DEFINE VARIABLE I-CODMAT   AS CHAR NO-UNDO.
DEFINE VARIABLE C-CODALM   AS CHAR NO-UNDO.
DEFINE VARIABLE F-CANDES   AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-IMPCTO   AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-STKSUB   AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-STKACT   AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-VCTOMN   AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-VCTOME   AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-TCTOMN   AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-TCTOME   AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PCTOMN   AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PCTOME   AS DECIMAL NO-UNDO.
DEFINE VARIABLE L-INGRESO  AS LOGICAL NO-UNDO.

/* Ubicamos el detalle a Actualizar */
FIND integral.AlmDMov WHERE ROWID(integral.AlmDMov) = R-DMov NO-LOCK NO-ERROR.
IF NOT AVAILABLE integral.AlmDMov THEN RETURN 'OK'.
/* Inicio de Transaccion */
DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN 'ADM-ERROR':
    /* ************** RHC 30.03.04 ******************* */
    FIND integral.AlmDMov WHERE ROWID(integral.AlmDMov) = r-dmov NO-LOCK NO-ERROR.
    ASSIGN I-CODMAT = integral.AlmDMov.CodMaT
           C-CODALM = integral.AlmDMov.CodAlm
           F-CANDES = integral.AlmDMov.CanDes
           F-IMPCTO = integral.AlmDMov.ImpCto.
    IF integral.AlmDMov.Factor > 0 THEN ASSIGN F-CANDES = integral.AlmDMov.CanDes * integral.AlmDMov.Factor.
    /* Buscamos el stock inicial */
    FIND PREV integral.AlmDMov USE-INDEX ALMD03 WHERE integral.AlmDMov.codcia = s-codcia
        AND integral.AlmDMov.codmat = i-codmat
        AND integral.AlmDMov.codalm = c-codalm
        NO-LOCK NO-ERROR.
    IF AVAILABLE integral.AlmDMov
    THEN f-StkSub = integral.AlmDMov.stksub.
    ELSE f-StkSub = 0.
    /* actualizamos el kardex por almacen */
    FIND integral.AlmDMov WHERE ROWID(integral.AlmDMov) = r-dmov EXCLUSIVE-LOCK NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, RETURN 'ADM-ERROR'.
    IF C-UD = 'D'       /* Eliminar */
    THEN integral.AlmDMov.candes = 0.        /* OJO */
    REPEAT WHILE AVAILABLE integral.AlmDMov:
        L-INGRESO = LOOKUP(integral.AlmDMov.TipMov,"I,U") <> 0.
        F-CANDES = integral.AlmDMov.CanDes.
        IF integral.AlmDMov.Factor > 0 THEN F-CANDES = integral.AlmDMov.CanDes * integral.AlmDMov.Factor.
        IF L-INGRESO 
        THEN F-STKSUB = F-STKSUB + F-CANDES.
        ELSE F-STKSUB = F-STKSUB - F-CANDES.
        integral.AlmDMov.StkSub = F-STKSUB.      /* OJO */
        FIND NEXT integral.AlmDMov USE-INDEX ALMD03 WHERE integral.AlmDMov.codcia = s-codcia
            AND integral.AlmDMov.codmat = i-codmat
            AND integral.AlmDMov.codalm = c-codalm
            NO-LOCK NO-ERROR.
        IF AVAILABLE integral.AlmDMov
        THEN DO:
            FIND CURRENT integral.AlmDMov EXCLUSIVE-LOCK NO-ERROR.
            IF ERROR-STATUS:ERROR THEN UNDO, RETURN "ADM-ERROR".
        END.
    END.
END.        
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ALMACSTK) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ALMACSTK Procedure 
PROCEDURE ALMACSTK :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER R-DMov AS ROWID.
DEFINE VARIABLE I-CODMAT AS CHAR NO-UNDO.
DEFINE VARIABLE C-CODALM AS CHAR NO-UNDO.
DEFINE VARIABLE F-CANDES AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PREUNI AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PREUMN AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PREUME AS DECIMAL NO-UNDO.

DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    /* Ubicamos el detalle a Actualizar */
    FIND AlmDMov WHERE ROWID(AlmDMov) = R-DMov NO-LOCK NO-ERROR.
    IF NOT AVAILABLE AlmDMov THEN RETURN.

    ASSIGN I-CODMAT = AlmDMov.CodMaT
           C-CODALM = AlmDMov.CodAlm
           F-CANDES = AlmDMov.CanDes
           F-PREUNI = AlmDMov.PreUni.
    IF AlmDMov.CodMon = 1 
    THEN DO:
         F-PREUMN = AlmDMov.PreUni.
         IF AlmDMov.TpoCmb > 0 
         THEN F-PREUME = ROUND(AlmDMov.PreUni / AlmDMov.TpoCmb,4).
         ELSE F-PREUME = 0.
    END.
    ELSE ASSIGN F-PREUMN = ROUND(AlmDMov.PreUni * AlmDMov.TpoCmb,4)
                F-PREUME = AlmDMov.PreUni.
    IF AlmDMov.Factor > 0 THEN ASSIGN F-CANDES = AlmDMov.CanDes * AlmDMov.Factor
                                      F-PREUNI = AlmDMov.PreUni / AlmDMov.Factor
                                      F-PREUMN = F-PREUMN / AlmDMov.Factor
                                      F-PREUME = F-PREUME / AlmDMov.Factor.

    /* Actualizamos a los Materiales por Almacen */
    FIND FIRST Almmmate WHERE Almmmate.CodCia = S-CODCIA AND
         Almmmate.CodAlm = C-CODALM AND 
         Almmmate.CodMat = I-CODMAT EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE Almmmate THEN UNDO, RETURN 'ADM-ERROR'.
    ASSIGN Almmmate.StkAct = Almmmate.StkAct + F-CANDES.
END.

RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Genera-Pedido) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Genera-Pedido Procedure 
PROCEDURE Genera-Pedido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE I-NT-DOCMP AS INTEGER NO-UNDO INIT 0.

  FOR EACH T-DOCMP OF T-COCMP, FIRST Almmmatg OF T-DOCMP NO-LOCK: 
      I-NT-DOCMP = I-NT-DOCMP + 1.
      CREATE Facdpedi.
      BUFFER-COPY T-DOCMP 
          TO Facdpedi
          ASSIGN
              Facdpedi.CodCia = Faccpedi.CodCia
              Facdpedi.CodDiv = Faccpedi.CodDiv
              Facdpedi.coddoc = Faccpedi.coddoc
              Facdpedi.NroPed = Faccpedi.NroPed
              Facdpedi.FchPed = Faccpedi.FchPed
              Facdpedi.Hora   = Faccpedi.Hora 
              Facdpedi.FlgEst = Faccpedi.FlgEst
              FacDPedi.CanPed = LG-DOCmp.CanPedi
              FacDPedi.CanPick = FacDPedi.CanPed
              Facdpedi.NroItm = I-NT-DOCMP.
  END.
  /* verificamos que al menos exista 1 item grabado */
  FIND FIRST Facdpedi OF Faccpedi NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Facdpedi 
  THEN RETURN 'ADM-ERROR'.
  ELSE RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Ingreso-Almacen) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ingreso-Almacen Procedure 
PROCEDURE Ingreso-Almacen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* UN INGRESO POR CADA GUIA DE REMISION */
DEF VAR F-PesUnd AS DECIMAL NO-UNDO.

trloop:
DO TRANSACTION ON ERROR UNDO, RETURN ERROR ON STOP UNDO, RETURN ERROR:
    FIND FIRST T-COCMP.
    ASSIGN
        s-CodAlm = T-COCMP.CodAlm
        x-NroRf3 = T-COCMP.Libre_c05
        s-user-id =  T-COCMP.Userid-com.
    FIND Almacen WHERE Almacen.CodCia = S-CODCIA 
        AND Almacen.CodAlm = s-CodAlm EXCLUSIVE-LOCK NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        DISPLAY
            'ERROR: Orden' OpenCCompras.NroDoc FORMAT '999999999' SKIP
            'ALMACEN:' s-codalm
            WITH STREAM-IO NO-BOX NO-LABELS.
        PAUSE 0.
        UNDO, RETURN ERROR.        
    END.
    FIND gn-prov WHERE gn-prov.CodCia = pv-codcia 
        AND gn-prov.Codpro = T-COCMP.codpro
        NO-LOCK NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        DISPLAY
            'ERROR: Orden' OpenCCompras.NroDoc FORMAT '999999999' SKIP
            'PROVEEDOR:' T-COCMP.codpro
            WITH STREAM-IO NO-BOX NO-LABELS.
        PAUSE 0.
        UNDO, RETURN ERROR.        
    END.
    FIND LAST gn-tcmb NO-LOCK NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        DISPLAY
            'ERROR: Orden' OpenCCompras.NroDoc FORMAT '999999999' SKIP
            'TIPO DE CAMBIO'
            WITH STREAM-IO NO-BOX NO-LABELS.
        PAUSE 0.
        UNDO, RETURN ERROR.        
    END.

    /*MESSAGE 'cabecera de almacenes' VIEW-AS ALERT-BOX.*/
    CREATE Almcmov.
    ASSIGN 
        Almcmov.CodCia  = s-CodCia 
        Almcmov.CodAlm  = s-CodAlm 
        Almcmov.TipMov  = "I"
        Almcmov.CodMov  = 02
        Almcmov.NroSer  = 000
        Almcmov.NroDoc = Almacen.CorrIng
        Almacen.CorrIng = Almacen.CorrIng + 1
        Almcmov.FchDoc  = T-COCMP.FchDoc
        Almcmov.HorRcp  = STRING(TIME,"HH:MM:SS")
        Almcmov.CodPro  = T-COCMP.codpro
        Almcmov.NomRef  = gn-prov.nompro
        Almcmov.CodMon  = T-COCMP.codmon
        Almcmov.TpoCmb  = gn-tcmb.venta
        Almcmov.NroRf1  = STRING(T-COCMP.NroDoc, '999999')
        Almcmov.NroRf2  = x-NroRf2
        Almcmov.NroRf3  = x-NroRf3
        Almcmov.Observ  = T-COCMP.Observaciones
        Almcmov.ModAdq  = T-COCMP.ModAdq.
    ASSIGN 
        Almcmov.usuario = S-USER-ID
        Almcmov.ImpIgv  = 0
        Almcmov.ImpMn1  = 0
        Almcmov.ImpMn2  = 0.
    /*MESSAGE 'detalle de almacenes' VIEW-AS ALERT-BOX.*/
    FOR EACH T-DOCMP:
        CREATE Almdmov.
        ASSIGN
            Almdmov.CodCia = S-CODCIA
            Almdmov.CodAlm = S-CODALM
            Almdmov.TipMov = Almcmov.TipMov 
            Almdmov.CodMov = Almcmov.CodMov 
            Almdmov.NroSer = Almcmov.NroSer 
            Almdmov.NroDoc = Almcmov.NroDoc 
            Almdmov.CodMon = Almcmov.CodMon 
            Almdmov.FchDoc = Almcmov.FchDoc 
            Almdmov.TpoCmb = Almcmov.TpoCmb
            Almdmov.Codmat = T-DOCMP.Codmat 
            Almdmov.CodUnd = T-DOCMP.UndCmp
            Almdmov.CanDes = T-DOCMP.CanPedi
            /*Almdmov.CanDev = T-DOCMP.CanPedi*/
            Almdmov.PreLis = T-DOCMP.PreUni
            Almdmov.Dsctos[1] = T-DOCMP.Dsctos[1]
            Almdmov.Dsctos[2] = T-DOCMP.Dsctos[2]
            Almdmov.Dsctos[3] = T-DOCMP.Dsctos[3]
            Almdmov.IgvMat = T-DOCMP.IgvMat
            Almdmov.PreUni = ROUND(T-DOCMP.PreUni * (1 - (T-DOCMP.Dsctos[1] / 100)) * 
                                            (1 - (T-DOCMP.Dsctos[2] / 100)) * 
                                            (1 - (T-DOCMP.Dsctos[3] / 100)),4)
            Almdmov.ImpCto = ROUND(Almdmov.CanDes * Almdmov.PreUni,2)
            Almdmov.CodAjt = 'A'
            Almdmov.HraDoc = Almcmov.HorRcp.
        FIND Almmmatg WHERE Almmmatg.CodCia = S-CODCIA 
            AND Almmmatg.codmat = Almdmov.codmat  
            NO-LOCK NO-ERROR.
        FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas 
            AND Almtconv.Codalter = Almdmov.CodUnd 
            NO-LOCK NO-ERROR.
        Almdmov.Factor = Almtconv.Equival / Almmmatg.FacEqu.
        IF NOT Almmmatg.AftIgv THEN Almdmov.IgvMat = 0.
        IF Almcmov.codmon = 1 
            THEN Almcmov.ImpMn1 = Almcmov.ImpMn1 + Almdmov.ImpMn1.
        ELSE Almcmov.ImpMn2 = Almcmov.ImpMn2 + Almdmov.ImpMn2.
        /* *** */
        RUN ALMACSTK (ROWID(Almdmov)).
        IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
            DISPLAY
                'ERROR: Orden' OpenCCompras.NroDoc FORMAT '999999999' SKIP
                'ALMACEN:' s-codalm SKIP
                'STOCK:' Almdmov.codmat
                    WITH STREAM-IO NO-BOX NO-LABELS.
                PAUSE 0.
            UNDO trloop, RETURN ERROR.
        END.
        RUN ALMACPR1-ING (ROWID(Almdmov), "U").
        IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
            DISPLAY
                'ERROR: Orden' OpenCCompras.NroDoc FORMAT '999999999' SKIP
                'ALMACEN:' s-codalm SKIP
                'KARDEX:' Almdmov.codmat
                    WITH STREAM-IO NO-BOX NO-LABELS.
                PAUSE 0.
            UNDO trloop, RETURN ERROR.
        END.
    END.
    
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Orden-de-Transferencia) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Orden-de-Transferencia Procedure 
PROCEDURE Orden-de-Transferencia :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF T-COCMP.Libre_c04 = '' OR T-COCMP.Libre_c04 = ?
    OR T-COCMP.CodAlm = SUBSTRING(T-COCMP.Libre_c04,1,2) THEN RETURN 'OK'.
FIND Almacen WHERE Almacen.codcia = s-codcia AND Almacen.codalm = T-COCMP.CodAlm
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almacen THEN DO:
    DISPLAY
       'ERROR: OTR: Almacén' T-COCMP.CodAlm SKIP 'NO configurado'
        WITH STREAM-IO NO-BOX NO-LABELS WIDTH 320.
    PAUSE 0.
    RETURN 'ADM-ERROR'.
END.

/* Code placed here will execute PRIOR to standard behavior. */
DEF VAR s-CodDoc AS CHAR INIT "OTR" NO-UNDO.
DEF VAR s-NroSer AS INT NO-UNDO.
DEF VAR s-codref   AS CHAR INITIAL "R/A".    /* Reposiciones Automáticas */
DEF VAR s-TpoPed AS CHAR NO-UNDO.
DEF VAR s-coddiv AS CHAR NO-UNDO.

DEF VAR iLocalCounter AS INTEGER INITIAL 0 NO-UNDO.
PRINCIPAL:
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    ASSIGN s-coddiv = Almacen.coddiv.
    FIND FIRST FacCorre WHERE FacCorre.CodCia = S-CODCIA 
        AND FacCorre.CodDiv = s-coddiv
        AND FacCorre.CodDoc = S-CODDOC 
        AND FacCorre.FlgEst = YES
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE FacCorre THEN DO:
      DISPLAY
         'ERROR: Correlativo de' s-coddoc SKIP 'NO configurado en la división' s-coddiv
          WITH STREAM-IO NO-BOX NO-LABELS WIDTH 320.
      PAUSE 0.
      UNDO, RETURN 'ADM-ERROR'.
    END.
    ASSIGN
      s-NroSer = Faccorre.nroser.
    /* Bloqueamos el correlativo para controlar las actualizaciones multiusaurio */
    GetLock:
    DO ON STOP UNDO GetLock, RETRY GetLock:
      IF RETRY THEN DO:
          iLocalCounter = iLocalCounter + 1.
          IF iLocalCounter = 5 THEN LEAVE GetLock.
      END.
      FIND FacCorre WHERE FacCorre.CodCia = s-CodCia AND
          FacCorre.CodDoc = s-coddoc AND
          FacCorre.NroSer = s-nroser EXCLUSIVE-LOCK NO-ERROR.
    END. 
    IF iLocalCounter = 5 OR NOT AVAILABLE FacCorre THEN UNDO, RETURN "ADM-ERROR".

    /* Code placed here will execute AFTER standard behavior.    */
    {vta2/icorrelativosecuencial.i &Codigo = s-coddoc &Serie = s-nroser}

    CREATE Faccpedi.
    ASSIGN 
      Faccpedi.CodCia = S-CODCIA
      Faccpedi.CodDiv = S-CODDIV
      Faccpedi.CodDoc = s-coddoc 
      Faccpedi.NroPed = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
      Faccpedi.codalm = T-COCMP.CodAlm
      Faccpedi.codcli = SUBSTRING(T-COCMP.Libre_C04, 1, 2)
      Faccpedi.usuario = s-user-id
      Faccpedi.CodRef = s-codref    /* R/A */
      Faccpedi.FchPed = T-COCMP.FchDoc
      Faccpedi.FchVen = T-COCMP.FchDoc + 7
      Faccpedi.FlgEst = "P"         /* APROBADO */
      Faccpedi.FlgSit = "C"         /* CHEQUEADO */
      FacCPedi.TpoPed = s-TpoPed
      FacCPedi.FlgEnv = YES
      FacCPedi.Libre_c01 = s-User-id + '|' + STRING(DATETIME(TODAY, MTIME), '99/99/9999 HH:MM').
    ASSIGN
    FacCorre.Correlativo = FacCorre.Correlativo + 1.
    /* Actualizamos la hora cuando lo vuelve a modificar */
    ASSIGN
      Faccpedi.Usuario = S-USER-ID
      Faccpedi.Hora   = STRING(TIME,"HH:MM").
    /* Division destino */
    FIND Almacen OF Faccpedi NO-LOCK NO-ERROR.
    IF AVAILABLE Almacen THEN FacCPedi.DivDes = Almacen.CodDiv.
    /* Detalle del Pedido */
    RUN Genera-Pedido.    /* Detalle del pedido */
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
      DISPLAY
          'NO se pudo generar la OTR. NO hay stock suficiente en los almacenes'
          WITH STREAM-IO NO-BOX.
      PAUSE 0.
      UNDO, RETURN 'ADM-ERROR'.
    END.
END.
IF AVAILABLE(FacCorre) THEN RELEASE FacCorre.
IF AVAILABLE(Facdpedi) THEN RELEASE Facdpedi.
RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

