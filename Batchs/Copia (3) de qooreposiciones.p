&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/


/* Temp-Table and Buffer definitions                                    */
DEFINE BUFFER CMOV FOR Almcmov.
DEFINE BUFFER CREPO FOR almcrepo.
DEFINE BUFFER DREPO FOR almdrepo.
DEFINE TEMP-TABLE ITEM LIKE Almdmov.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     : Cargar automáticamente los pedidos de reposición de mercadería
                  para el almacén 500 usado por OpenOrange

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Valores preestablecidos */
DEF VAR s-codcia AS INT  INIT 001   NO-UNDO.
DEF VAR s-codalm AS CHAR            NO-UNDO.
DEF VAR s-tipmov AS CHAR            NO-UNDO.
DEF VAR s-coddoc AS CHAR            NO-UNDO.
DEF VAR s-user-id AS CHAR INIT "ADMIN" NO-UNDO.
DEF VAR r-Rowid AS ROWID            NO-UNDO.

DEFINE BUFFER B-MATE FOR Almmmate.
DEFINE BUFFER B-OOReposiciones FOR OOReposiciones.
DEFINE TEMP-TABLE T-DREPO LIKE almdrepo
    FIELD Campo-R AS ROWID
    INDEX llave01 IS PRIMARY codmat.

DISABLE TRIGGERS FOR LOAD OF almcmov.
DISABLE TRIGGERS FOR LOAD OF almdmov.
DISABLE TRIGGERS FOR LOAD OF almmmatg.
DISABLE TRIGGERS FOR LOAD OF almmmate.
DISABLE TRIGGERS FOR LOAD OF almacen.
DISABLE TRIGGERS FOR LOAD OF almcrepo.
DISABLE TRIGGERS FOR LOAD OF almdrepo.
DISABLE TRIGGERS FOR LOAD OF almtdocm.
DISABLE TRIGGERS FOR LOAD OF faccorre.

DISPLAY 'Inicio:' TODAY STRING(TIME, 'HH:MM:SS') SKIP
    WITH STREAM-IO NO-BOX NO-LABELS.
PAUSE 0.

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
   Temp-Tables and Buffers:
      TABLE: CMOV B "?" ? INTEGRAL Almcmov
      TABLE: CREPO B "?" ? INTEGRAL almcrepo
      TABLE: DREPO B "?" ? INTEGRAL almdrepo
      TABLE: ITEM T "?" ? INTEGRAL Almdmov
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 9.96
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/*REPEAT:*/
    RUN Pedido-Reposicion-Automatica.
    RUN Salida-por-Transferencia.
    RUN Ingreso-por-Transferencia.    
    /*
    PAUSE 300.      /* espera 5 minutos */
END.  */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-almacpr1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE almacpr1 Procedure 
PROCEDURE almacpr1 :
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
FIND AlmDMov WHERE ROWID(AlmDMov) = R-DMov NO-LOCK NO-ERROR.
IF NOT AVAILABLE AlmDMov THEN RETURN 'OK'.

/* Inicio de Transaccion */
DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN 'ADM-ERROR':
    /* ************** RHC 30.03.04 ******************* */
    FIND almdmov WHERE ROWID(almdmov) = r-dmov NO-LOCK NO-ERROR.
    ASSIGN I-CODMAT = AlmDMov.CodMaT
           C-CODALM = AlmDMov.CodAlm
           F-CANDES = AlmDMov.CanDes
           F-IMPCTO = AlmDMov.ImpCto.
    IF AlmDMov.Factor > 0 THEN ASSIGN F-CANDES = AlmDMov.CanDes * AlmDMov.Factor.
    /* Buscamos el stock inicial */
    FIND PREV almdmov USE-INDEX ALMD03 WHERE almdmov.codcia = s-codcia
        AND almdmov.codmat = i-codmat
        AND almdmov.codalm = c-codalm
        NO-LOCK NO-ERROR.
    IF AVAILABLE almdmov
    THEN f-StkSub = almdmov.stksub.
    ELSE f-StkSub = 0.
    /* actualizamos el kardex por almacen */
    FIND almdmov WHERE ROWID(almdmov) = r-dmov EXCLUSIVE-LOCK NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, RETURN 'ADM-ERROR'.
    IF C-UD = 'D'       /* Eliminar */
    THEN almdmov.candes = 0.        /* OJO */
    REPEAT WHILE AVAILABLE almdmov:
        L-INGRESO = LOOKUP(AlmDMov.TipMov,"I,U") <> 0.
        F-CANDES = AlmDMov.CanDes.
        IF AlmDMov.Factor > 0 THEN F-CANDES = AlmDMov.CanDes * AlmDMov.Factor.
        IF L-INGRESO 
        THEN F-STKSUB = F-STKSUB + F-CANDES.
        ELSE F-STKSUB = F-STKSUB - F-CANDES.
        AlmDMov.StkSub = F-STKSUB.      /* OJO */
        FIND NEXT almdmov USE-INDEX ALMD03 WHERE almdmov.codcia = s-codcia
            AND almdmov.codmat = i-codmat
            AND almdmov.codalm = c-codalm
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almdmov
        THEN DO:
            FIND CURRENT almdmov EXCLUSIVE-LOCK NO-ERROR.
            IF ERROR-STATUS:ERROR THEN UNDO, RETURN "ADM-ERROR".
        END.
    END.
    RELEASE almdmov.
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
    FIND Almmmate WHERE Almmmate.CodCia = S-CODCIA AND
         Almmmate.CodAlm = C-CODALM AND 
         Almmmate.CodMat = I-CODMAT EXCLUSIVE-LOCK NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, RETURN 'ADM-ERROR'.

    ASSIGN Almmmate.StkAct = Almmmate.StkAct + F-CANDES.
    RELEASE Almmmate.
END.
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-almdcstk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE almdcstk Procedure 
PROCEDURE almdcstk :
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

/* Ubicamos el detalle a Actualizar */
FIND AlmDMov WHERE ROWID(AlmDMov) = R-DMov NO-LOCK NO-ERROR.
IF NOT AVAILABLE AlmDMov THEN RETURN 'OK'.

DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    ASSIGN 
      I-CODMAT = AlmDMov.CodMaT
      C-CODALM = AlmDMov.CodAlm
      F-CANDES = AlmDMov.CanDes
      F-PREUNI = AlmDMov.PreUni.
    IF AlmDMov.Factor > 0 
    THEN ASSIGN 
              F-CANDES = AlmDMov.CanDes * AlmDMov.Factor
              F-PREUNI = AlmDMov.PreUni / AlmDMov.Factor.
    /* Des-Actualizamos a los Materiales por Almacen */
    FIND Almmmate WHERE Almmmate.CodCia = S-CODCIA AND
          Almmmate.CodAlm = C-CODALM AND 
          Almmmate.CodMat = I-CODMAT EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE Almmmate THEN DO:
        MESSAGE 'Codigo' i-codmat 'NO asignado en el almacen' c-codalm
            VIEW-AS ALERT-BOX ERROR.
        UNDO, RETURN 'ADM-ERROR'.
    END.
    Almmmate.StkAct = Almmmate.StkAct - F-CANDES.
    RELEASE Almmmate.
END.

RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Genera-Detalle-Ingreso) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Genera-Detalle-Ingreso Procedure 
PROCEDURE Genera-Detalle-Ingreso :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FOR EACH ITEM WHERE ITEM.codmat <> "" ON ERROR UNDO, RETURN "ADM-ERROR":
      CREATE almdmov.
      ASSIGN Almdmov.CodCia = Almcmov.CodCia 
             Almdmov.CodAlm = Almcmov.CodAlm 
             Almdmov.TipMov = Almcmov.TipMov 
             Almdmov.CodMov = Almcmov.CodMov 
             Almdmov.NroSer = Almcmov.NroSer 
             Almdmov.NroDoc = Almcmov.NroDoc 
             Almdmov.CodMon = Almcmov.CodMon 
             Almdmov.FchDoc = Almcmov.FchDoc 
             Almdmov.TpoCmb = Almcmov.TpoCmb 
             Almdmov.codmat = ITEM.codmat 
             Almdmov.CanDes = ITEM.CanDes 
             Almdmov.CodUnd = ITEM.CodUnd 
             Almdmov.Factor = ITEM.Factor 
             Almdmov.ImpCto = ITEM.ImpCto 
             Almdmov.PreUni = ITEM.PreUni 
             Almdmov.AlmOri = Almcmov.AlmDes 
             Almdmov.CodAjt = '' 
             Almdmov.HraDoc = Almcmov.HorRcp
                    R-ROWID = ROWID(Almdmov).

      RUN ALMACSTK (R-ROWID).
      IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
      
      RUN almacpr1 (R-ROWID, 'U').
      IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Genera-Detalle-Salida) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Genera-Detalle-Salida Procedure 
PROCEDURE Genera-Detalle-Salida :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR r-Rowid AS ROWID NO-UNDO.
DEF VAR pComprometido AS DEC.
DEF VAR x-Item AS INT INIT 0 NO-UNDO.
DEF VAR x-Items_Guias LIKE FacCfgGn.Items_Guias NO-UNDO.

FIND FIRST FacCfgGn WHERE FacCfgGn.codcia = s-codcia NO-LOCK.
x-Items_Guias = FacCfgGn.Items_Guias.
x-Items_Guias = 999999. /* OJO: Sin límite de items */
DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN "ADM-ERROR":
    FOR EACH ITEM BY ITEM.NroItm:
        IF x-Item >= x-Items_Guias THEN LEAVE.
        CREATE almdmov.
        ASSIGN Almdmov.CodCia = Almcmov.CodCia 
               Almdmov.CodAlm = Almcmov.CodAlm 
               Almdmov.TipMov = Almcmov.TipMov 
               Almdmov.CodMov = Almcmov.CodMov 
               Almdmov.NroSer = Almcmov.NroSer
               Almdmov.NroDoc = Almcmov.NroDoc 
               Almdmov.CodMon = Almcmov.CodMon 
               Almdmov.FchDoc = Almcmov.FchDoc 
               Almdmov.HraDoc = Almcmov.HraDoc
               Almdmov.TpoCmb = Almcmov.TpoCmb
               Almdmov.codmat = ITEM.codmat
               Almdmov.CanDes = ITEM.CanDes
               Almdmov.CodUnd = ITEM.CodUnd
               Almdmov.Factor = ITEM.Factor
               Almdmov.ImpCto = ITEM.ImpCto
               Almdmov.PreUni = ITEM.PreUni
               Almdmov.AlmOri = Almcmov.AlmDes 
               Almdmov.CodAjt = ''
               Almdmov.HraDoc = HorSal
               Almdmov.NroItm = x-Item
               R-ROWID = ROWID(Almdmov).
        x-Item = x-Item + 1.
        RUN almdcstk (R-ROWID).
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
        RUN almacpr1 (R-ROWID, "U").
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
        /* Se anulan los items que se pueden descargar */
        DELETE ITEM.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Generar-Pedidos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Generar-Pedidos Procedure 
PROCEDURE Generar-Pedidos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR x-CanApro AS DEC NO-UNDO.
DEF VAR x-CanAte  AS DEC NO-UNDO.

DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN "ADM-ERROR":
    FOR EACH T-DREPO WHERE T-DREPO.CanReq > 0 BREAK BY T-DREPO.AlmPed:
        IF FIRST-OF(AlmPed) THEN DO:
            CREATE Almcrepo.
            ASSIGN
                almcrepo.AlmPed = T-DREPO.Almped
                almcrepo.CodAlm = s-codalm
                almcrepo.CodCia = s-codcia
                almcrepo.FchDoc = TODAY
                almcrepo.FchVto = TODAY + 7
                almcrepo.Fecha = TODAY
                almcrepo.Hora = STRING(TIME, 'HH:MM')
                almcrepo.NroDoc = Faccorre.correlativo
                almcrepo.NroSer = Faccorre.nroser
                almcrepo.TipMov = s-tipmov
                almcrepo.Usuario = "AUTOMATICO".
                /*almcrepo.Glosa = fill-in-Glosa.*/
            ASSIGN
                Faccorre.correlativo = Faccorre.correlativo + 1.
            /* APROBADO AUTOMATICAMENTE */
            ASSIGN
                almcrepo.FchApr = TODAY
                Almcrepo.FlgEst = 'P'
                almcrepo.FlgSit = 'A'
                almcrepo.HorApr = STRING(TIME, 'HH:MM')
                almcrepo.UsrApr = "AUTOMATICO".
        END.
        CREATE Almdrepo.
        BUFFER-COPY T-DREPO TO Almdrepo
            ASSIGN
            almdrepo.CodCia = almcrepo.codcia
            almdrepo.CodAlm = almcrepo.codalm
            almdrepo.TipMov = almcrepo.tipmov
            almdrepo.NroSer = almcrepo.nroser
            almdrepo.NroDoc = almcrepo.nrodoc
            almdrepo.CanApro = Almdrepo.canreq.
    END.
    FOR EACH T-DREPO:
        FIND B-OOReposiciones WHERE ROWID(B-OOReposiciones) = T-DREPO.Campo-R EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF NOT AVAILABLE B-OOReposiciones THEN UNDO, RETURN "ADM-ERROR".
        ASSIGN
            B-OOReposiciones.FlagMigracion = 'X'
            B-OOReposiciones.CanAte = T-DREPO.CanReq.
    END.
END.
RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Ingreso-por-Transferencia) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ingreso-por-Transferencia Procedure 
PROCEDURE Ingreso-por-Transferencia :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

GENERAL:
FOR EACH Almacen NO-LOCK WHERE Almacen.codcia = s-codcia,
    EACH CMOV WHERE CMOV.CodCia = S-CODCIA 
    AND CMOV.CodAlm = Almacen.CodAlm
    AND CMOV.AlmDes = "500" 
    AND CMOV.TipMov = "S" 
    AND CMOV.CodMov = 03 
    AND CMOV.FlgEst <> "A" 
    AND CMOV.FlgSit = "T"               /* En Tránsito */
    AND CMOV.usuario = S-USER-ID        /* Usuario automático */
    ON ERROR UNDO, NEXT ON STOP UNDO, NEXT.
    s-CodAlm = CMOV.AlmDes.
    FIND Almtdocm WHERE Almtdocm.CodCia = S-CODCIA 
        AND Almtdocm.CodAlm = S-CODALM 
        AND Almtdocm.TipMov = "I" 
        AND Almtdocm.CodMov = 03
        EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF ERROR-STATUS:ERROR THEN UNDO, NEXT GENERAL.
    /* Detalle */
    EMPTY TEMP-TABLE ITEM.
    FOR EACH Almdmov NO-LOCK WHERE Almdmov.CodCia = CMOV.CodCia 
        AND Almdmov.CodAlm = CMOV.CodAlm 
        AND Almdmov.TipMov = CMOV.TipMov 
        AND Almdmov.CodMov = CMOV.CodMov 
        AND Almdmov.NroSer = CMOV.NroSer
        AND Almdmov.NroDoc = CMOV.NroDoc:
        CREATE ITEM.
        ASSIGN 
            ITEM.CodCia = Almdmov.CodCia
            ITEM.CodAlm = Almdmov.CodAlm
            ITEM.codmat = Almdmov.codmat 
            ITEM.PreUni = Almdmov.PreUni 
            ITEM.CanDes = Almdmov.CanDes 
            ITEM.Factor = Almdmov.Factor 
            ITEM.ImpCto = Almdmov.ImpCto
            ITEM.CodUnd = Almdmov.CodUnd.
    END.  
    /* Cabecera */
    CREATE Almcmov.
    ASSIGN
        Almcmov.CodCia  = Almtdocm.CodCia 
        Almcmov.CodAlm  = Almtdocm.CodAlm 
        Almcmov.AlmDes  = CMOV.CodAlm
        Almcmov.TipMov  = Almtdocm.TipMov 
        Almcmov.CodMov  = Almtdocm.CodMov 
        Almcmov.NroSer  = 000
        Almcmov.NroDoc  = Almtdocm.NroDoc
        Almtdocm.NroDoc = Almtdocm.NroDoc + 1
        Almcmov.FlgSit  = ""
        Almcmov.HorRcp  = STRING(TIME,"HH:MM:SS")
        Almcmov.NroRf1 = STRING(CMOV.NroSer,"999") + STRING(CMOV.NroDoc)
        Almcmov.usuario = S-USER-ID. 

    RUN Genera-Detalle-Ingreso.
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, NEXT.

    ASSIGN 
        CMOV.FlgSit  = "R" 
        CMOV.HorRcp  = STRING(TIME,"HH:MM:SS")
        CMOV.NroRf2  = STRING(Almcmov.NroDoc).
END.
IF AVAILABLE(Almtdocm) THEN RELEASE Almtdocm.
IF AVAILABLE(CMOV) THEN RELEASE CMOV.
IF AVAILABLE(Almcmov) THEN RELEASE Almcmov.
IF AVAILABLE(Almdmov) THEN RELEASE Almdmov.
IF AVAILABLE(Almmmate) THEN RELEASE Almmmate.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Pedido-Reposicion-Automatica) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pedido-Reposicion-Automatica Procedure 
PROCEDURE Pedido-Reposicion-Automatica :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN
    s-CodAlm = "500"    /* Almacén Web */
    s-TipMov = "A"     /* Automático */
    s-CodDoc = "R/A".   /* Reposición Automática */
/* Control del Correlativo */
FIND FIRST Faccorre WHERE Faccorre.codcia = s-codcia
    AND Faccorre.coddoc = s-coddoc
    AND Faccorre.flgest = YES
    AND Faccorre.codalm = s-codalm
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE FacCorre THEN DO:
    DISPLAY
        'NO configurado el correlativo para:' s-coddoc 'almacén:' s-codalm SKIP
        WITH STREAM-IO NO-BOX NO-LABELS.
    PAUSE 0.
    RETURN.
END.
/* Variables locales */
DEF VAR x-StockMinimo   AS DEC NO-UNDO.
DEF VAR x-StkAct        AS DEC NO-UNDO.
DEF VAR k               AS INT NO-UNDO.
DEF VAR pReposicion     AS DEC NO-UNDO.
DEF VAR x-Item          AS INT NO-UNDO.
DEF VAR x-TipMat        AS CHAR NO-UNDO.
DEF VAR pComprometido   AS DEC NO-UNDO.
DEF VAR x-StockDisponible AS DEC NO-UNDO.
DEF VAR x-CanReq        AS DEC NO-UNDO.
/* Buscamos los valores generales */
FIND FIRST AlmCfgGn WHERE almcfggn.codcia = s-codcia NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almcfggn THEN DO:
    DISPLAY 'Debe configurar los parámetros generales de reposición' SKIP WITH STREAM-IO NO-BOX NO-LABELS.
    PAUSE 0.
    RETURN.
END.
FIND Almacen WHERE Almacen.codcia = s-codcia AND Almacen.codalm = s-codalm NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almacen THEN DO:
    DISPLAY 'Almacén:' s-codalm 'no registrado' SKIP WITH STREAM-IO NO-BOX NO-LABELS.
    PAUSE 0.
    RETURN.
END.
FIND FIRST Faccorre WHERE Faccorre.codcia = s-codcia
    AND Faccorre.coddoc = s-coddoc
    AND Faccorre.flgest = YES
    AND Faccorre.codalm = s-codalm
    EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
IF NOT AVAILABLE Faccorre THEN DO:
    DISPLAY 'Correlativo para:' s-coddoc 'almacen:' s-codalm 'no registrado' SKIP 
        WITH STREAM-IO NO-BOX NO-LABELS.
    PAUSE 0.
    RETURN.
END.

/* SE VA A GENERAR UNA ORDEN POR CADA REQUERIMIENTO DE OPENORANGE */
/* Control */
DEF VAR c-ErrorItem AS LOG NO-UNDO.
/* SI un item no se puede despachar => no se despacha nada */
FOR EACH OOReposiciones WHERE OOReposiciones.CodCia = s-codcia
    AND OOReposiciones.CodAlm = s-codalm
    AND OOReposiciones.FlagMigracion = 'N',
    FIRST Almmmatg OF OOReposiciones NO-LOCK,
    FIRST Almmmate OF OOReposiciones NO-LOCK
    BREAK BY OOReposiciones.SerNr BY OOReposiciones.CodMat 
    ON ERROR UNDO, NEXT ON STOP UNDO, NEXT:

    IF FIRST-OF(OOReposiciones.SerNr) THEN DO:
        c-ErrorItem = NO.
        EMPTY TEMP-TABLE T-DREPO.
    END.
        
    DISPLAY
        'Orden' OOReposiciones.SerNr 'Producto' OOReposiciones.CodMat 
        'Cantidad' OOReposiciones.CanPed SKIP
        WITH STREAM-IO NO-BOX NO-LABELS.
    PAUSE 0.
    FIND FIRST T-DREPO WHERE T-DREPO.codmat = OOReposiciones.codmat NO-ERROR.
    IF AVAILABLE T-DREPO THEN NEXT.
    CREATE T-DREPO.
    ASSIGN 
        T-DREPO.Origen = 'AUT'
        T-DREPO.CodCia = s-codcia 
        T-DREPO.CodAlm = s-codalm 
        T-DREPO.AlmPed = "XXX"  /* Temporalmente */
        T-DREPO.CodMat = Almmmate.codmat
        T-DREPO.Campo-R = ROWID(OOReposiciones).
    /* Stock Minimo */
    ASSIGN
        /* <<<< OJO: Tomamos este valor <<< */
        x-StockMinimo = OOReposiciones.CanPed
        x-StkAct = Almmmate.StkAct.
    /* Cantidad de Reposicion */
    pReposicion = x-StockMinimo.            /* <<<< OJO: Tomamos este valor <<< */
    /* distribuimos el pedido entre los almacenes de despacho */
    IF Almmmatg.Chr__02 = "P" THEN x-TipMat = "P". ELSE x-TipMat = "T".
    FOR EACH Almrepos NO-LOCK WHERE  almrepos.CodCia = s-codcia
        AND almrepos.CodAlm = Almmmate.codalm 
        AND almrepos.AlmPed <> Almmmate.codalm
        AND almrepos.TipMat = x-TipMat      /* Propios o Terceros */
        AND pReposicion > 0 /* <<< OJO <<< */
        BY almrepos.Orden:
        FIND B-MATE WHERE B-MATE.codcia = s-codcia
            AND B-MATE.codmat = Almmmate.codmat
            AND B-MATE.codalm = Almrepos.almped
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE B-MATE THEN NEXT.
        RUN Stock-Comprometido (Almmmate.CodMat, Almrepos.AlmPed, OUTPUT pComprometido).
        x-StockDisponible = B-MATE.StkAct - pComprometido.
        IF x-StockDisponible <= 0 THEN NEXT.
        /* Se solicitará la reposición de acuerdo al empaque del producto */
        x-CanReq = MINIMUM(x-StockDisponible, pReposicion).
        IF Almacen.Campo[7] = "REE" AND Almmmatg.CanEmp > 0 THEN DO:
            x-CanReq = ROUND(x-CanReq / Almmmatg.CanEmp, 0) * Almmmatg.CanEmp.
        END.
        IF x-CanReq <= 0 THEN NEXT.    /* Menos que la cantidad por empaque */
        /* Redondeamos la cantidad a enteros */
        IF TRUNCATE(x-CanReq,0) <> x-CanReq THEN DO:
            x-CanReq = TRUNCATE(x-CanReq,0) + 1.
        END.
        /* ********************************* */
        ASSIGN
            T-DREPO.AlmPed = Almrepos.almped
            T-DREPO.Item = x-Item
            T-DREPO.CanReq = x-CanReq
            T-DREPO.CanGen = x-CanReq
            T-DREPO.StkAct = x-StockDisponible.
        ASSIGN
            x-Item = x-Item + 1
            pReposicion = pReposicion - T-DREPO.CanReq.
    END.
    /* GENERAMOS LA ORDEN DE REPOSICION */
    IF LAST-OF(OOReposiciones.SerNr) THEN DO:
        RUN Generar-Pedidos.
        IF RETURN-VALUE = "ADM-ERROR" THEN UNDO, NEXT.
    END.
END.
IF AVAILABLE(FacCorre) THEN RELEASE FacCorre.
IF AVAILABLE(Almcrepo) THEN RELEASE almcrepo.
IF AVAILABLE(Almdrepo) THEN RELEASE almdrepo.
IF AVAILABLE(OOReposiciones) THEN RELEASE OOReposiciones.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Salida-por-Transferencia) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Salida-por-Transferencia Procedure 
PROCEDURE Salida-por-Transferencia :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR x-Stock-Disponible AS DEC NO-UNDO.
DEF VAR pComprometido AS DEC NO-UNDO.
DEF VAR x-Nrodoc LIKE Almacen.CorrSal NO-UNDO.
DEF VAR s-NroSer AS INT INIT 000 NO-UNDO.
DEF VAR s-CodRef AS CHAR INIT "R/A" NO-UNDO.
DEF VAR s-CodAlm AS CHAR NO-UNDO.

EMPTY TEMP-TABLE ITEM.
GENERAL:
FOR EACH Almcrepo WHERE Almcrepo.CodCia = S-CODCIA 
    AND Almcrepo.CodAlm = "500"     /* Almacén Venta Web */
    AND Almcrepo.flgest = "P" 
    AND Almcrepo.flgsit = "A",
    FIRST Almtdocm NO-LOCK WHERE Almtdocm.CodCia = s-codcia
    AND Almtdocm.CodAlm = almcrepo.AlmPed
    AND Almtdocm.TipMov = "S" 
    AND Almtdocm.CodMov = 03
    ON ERROR UNDO, NEXT ON STOP UNDO, NEXT:
    /* Almacén de descarga */
    s-CodAlm = Almcrepo.AlmPed.
    /* Bloqueamos Almacén */
    FIND Almacen WHERE Almacen.CodCia = S-CODCIA 
        AND Almacen.CodAlm = s-CodAlm 
        EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF NOT AVAILABLE Almacen THEN NEXT.
    /* Cargamos el archivo Temporal */
    EMPTY TEMP-TABLE ITEM.
    FOR EACH Almdrepo OF Almcrepo NO-LOCK WHERE ( almdrepo.CanApro - almdrepo.CanAten > 0 ):
        FIND Almmmate WHERE Almmmate.CodCia = Almdrepo.CodCia  
            AND  Almmmate.CodAlm = s-CodAlm 
            AND  Almmmate.CodMat = Almdrepo.CodMat 
            NO-LOCK NO-ERROR. 
        IF NOT AVAILABLE Almmmate THEN NEXT GENERAL.
        RUN stock-comprometido (Almdrepo.codmat, s-CodAlm, OUTPUT pComprometido).
        /* Como en el cálculo del stock comprometido YA está incluido la cantidad
          a reponer, entonces hay que volverlo a quitar */
        pComprometido = pComprometido - ( almdrepo.CanApro - almdrepo.CanAten ).

        IF (Almmmate.stkact - pComprometido) <= 0 THEN NEXT GENERAL.
        x-Stock-Disponible = MINIMUM ( (Almmmate.stkact - pComprometido), (Almdrepo.canapro - Almdrepo.canaten) ).
        FIND Almmmatg WHERE Almmmatg.CodCia = Almdrepo.CodCia  
            AND Almmmatg.CodMat = Almdrepo.CodMat 
            NO-LOCK NO-ERROR. 
        /* GENERAMOS MOVIMIENTO PARA ALMACEN ORIGEN */
        CREATE ITEM.
        ASSIGN 
            ITEM.CodCia = Almtdocm.CodCia
            ITEM.CodAlm = Almtdocm.CodAlm
            ITEM.CodMat = Almdrepo.CodMat
            ITEM.CodAjt = ""
            ITEM.Factor = 1
            ITEM.CodUnd = Almmmatg.UndStk
            ITEM.AlmOri = Almcrepo.CodAlm
            ITEM.CanDes = x-Stock-Disponible
            ITEM.StkAct = (Almdrepo.canapro - Almdrepo.canaten).  /* OJO */
    END.
    /* Generamos la Cabecera */
    ASSIGN 
        x-Nrodoc  = Almacen.CorrSal
        Almacen.CorrSal = Almacen.CorrSal + 1.
    CREATE Almcmov.
    ASSIGN 
        Almcmov.CodCia = Almtdocm.CodCia 
        Almcmov.CodAlm = Almtdocm.CodAlm 
        Almcmov.AlmDes = Almcrepo.CodAlm
        Almcmov.TipMov = Almtdocm.TipMov
        Almcmov.CodMov = Almtdocm.CodMov
        Almcmov.FlgSit = "T"      /* Transferido pero no Recepcionado */
        Almcmov.FchDoc = TODAY
        Almcmov.HorSal = STRING(TIME,"HH:MM")
        Almcmov.HraDoc = STRING(TIME,"HH:MM")
        Almcmov.NroSer = s-nroser
        Almcmov.NroDoc = x-NroDoc
        Almcmov.CodRef = s-CodRef
        Almcmov.usuario = S-USER-ID.
    FOR EACH ITEM WHERE ITEM.codmat = '':
        DELETE ITEM.
    END.
    RUN Genera-Detalle-Salida.
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, NEXT.
    /* DAMOS MAS VUELTAS EN CASO QUEDEN ITEMS POR GENERAR */
    FIND FIRST ITEM NO-ERROR.
    REPEAT WHILE AVAILABLE ITEM:
        IF AVAILABLE Faccorre THEN DO:
            ASSIGN
                x-NroDoc = Faccorre.Correlativo
                Faccorre.correlativo = Faccorre.correlativo + 1.
        END.
        ELSE DO:
            ASSIGN 
                x-Nrodoc  = Almacen.CorrSal
                Almacen.CorrSal = Almacen.CorrSal + 1.
        END.
        CREATE CMOV.
        BUFFER-COPY Almcmov TO CMOV
            ASSIGN 
              CMOV.NroDoc  = x-NroDoc
              CMOV.usuario = S-USER-ID.

        FIND Almcmov WHERE ROWID(Almcmov) = ROWID(CMOV) EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE Almcmov THEN UNDO GENERAL, NEXT GENERAL.

        RUN Genera-Detalle-Salida.
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, NEXT GENERAL.

        FIND FIRST ITEM NO-ERROR.
    END.
    /* Actualizamos el Pedido Completo */
    FOR EACH Almdrepo OF Almcrepo:
        Almdrepo.CanAten = Almdrepo.CanApro.
    END.
    ASSIGN
        almcrepo.FlgEst = 'C'     /* Atendido */
        almcrepo.HorAct = STRING(TIME, 'HH:MM')
        almcrepo.FecAct = TODAY
        almcrepo.UsrAct = s-user-id.
END.
/* DESBLOQUEAMOS LOS CORRELATIVOS */
IF AVAILABLE(Almacen) THEN RELEASE Almacen.
IF AVAILABLE(Almdmov) THEN RELEASE Almdmov.
IF AVAILABLE(Almmmate) THEN RELEASE Almmmate.
IF AVAILABLE(Almcrepo) THEN RELEASE Almcrepo.
IF AVAILABLE(Almdrepo) THEN RELEASE Almdrepo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Stock-Comprometido) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Stock-Comprometido Procedure 
PROCEDURE Stock-Comprometido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pCodMat AS CHAR.
DEF INPUT PARAMETER pCodAlm AS CHAR.
DEF OUTPUT PARAMETER pComprometido AS DEC.

/* CALCULO DEL STOCK COMPROMETIDO */

FIND FacCfgGn WHERE faccfggn.codcia = s-codcia NO-LOCK NO-ERROR.
IF NOT AVAILABLE Faccfggn THEN RETURN.

/* Stock comprometido por PEDIDOS ACTIVOS MOSTRADOR */
DEF VAR TimeOut AS INTEGER NO-UNDO.
DEF VAR TimeNow AS INTEGER NO-UNDO.

/* Tiempo por defecto fuera de campaña */
TimeOut = (FacCfgGn.Dias-Res * 24 * 3600) +
          (FacCfgGn.Hora-Res * 3600) + 
          (FacCfgGn.Minu-Res * 60).

pComprometido = 0.
DEF VAR i AS INT NO-UNDO.
DEF VAR x-CodAlm AS CHAR NO-UNDO.

DO i = 1 TO NUM-ENTRIES(pCodAlm):
    x-CodAlm = ENTRY(i, pCodAlm).
    /**********   Barremos para los PEDIDOS MOSTRADOR ***********************/ 
    FOR EACH Facdpedi USE-INDEX Llave04 NO-LOCK WHERE Facdpedi.CodCia = s-codcia
        AND Facdpedi.AlmDes = x-CodAlm
        AND Facdpedi.codmat = pcodmat
        AND Facdpedi.coddoc = 'P/M'
        AND Facdpedi.FlgEst = "P" :
        FIND FIRST Faccpedi OF Facdpedi WHERE Faccpedi.FlgEst = "P" NO-LOCK NO-ERROR.
        IF NOT AVAIL Faccpedi THEN NEXT.

        TimeNow = (TODAY - FacCPedi.FchPed) * 24 * 3600.
        TimeNow = TimeNow + TIME - ( (INTEGER(SUBSTRING(FacCPedi.Hora, 1, 2)) * 3600) +
                  (INTEGER(SUBSTRING(FacCPedi.Hora, 4, 2)) * 60) ).
        IF TimeOut > 0 THEN DO:
            IF TimeNow <= TimeOut   /* Dentro de la valides */
            THEN DO:
                /* cantidad en reservacion */
                pComprometido = pComprometido + FacDPedi.Factor * FacDPedi.CanPed.
            END.
        END.
    END.
    /**********   Barremos para los PEDIDOS AL CREDITO   ***********************/ 
    FOR EACH Facdpedi USE-INDEX Llave04 NO-LOCK WHERE Facdpedi.codcia = s-codcia
            AND Facdpedi.almdes = x-CodAlm
            AND Facdpedi.codmat = pCodMat
            AND Facdpedi.coddoc = 'PED'
            AND Facdpedi.flgest = 'P':
        /* RHC 12.12.2011 agregamos los nuevos estados */
        FIND FIRST Faccpedi OF Facdpedi WHERE LOOKUP(Faccpedi.FlgEst, "G,X,P,W,WX,WL") > 0 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Faccpedi THEN NEXT.
        pComprometido = pComprometido + FacDPedi.Factor * (FacDPedi.CanPed - FacDPedi.canate).
    END.
    /* ORDENES DE DESPACHO CREDITO */
    FOR EACH Facdpedi USE-INDEX Llave04 NO-LOCK WHERE Facdpedi.codcia = s-codcia
            AND Facdpedi.almdes = x-CodAlm
            AND Facdpedi.codmat = pCodMat
            AND Facdpedi.coddoc = 'O/D'
            AND Facdpedi.flgest = 'P':
        FIND FIRST Faccpedi OF Facdpedi WHERE Faccpedi.flgest = 'P' NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Faccpedi THEN NEXT.
        pComprometido = pComprometido + FacDPedi.Factor * (FacDPedi.CanPed - FacDPedi.canate).
    END.
    /* Stock Comprometido por Pedidos por Reposicion Automatica */
    FOR EACH CREPO NO-LOCK WHERE CREPO.codcia = s-codcia
        AND CREPO.TipMov = 'A'
        AND CREPO.AlmPed = x-CodAlm
        AND CREPO.FlgEst = 'P'
        AND CREPO.FlgSit = 'A',
        EACH DREPO OF CREPO NO-LOCK WHERE DREPO.codmat = pCodMat
        AND DREPO.CanApro > DREPO.CanAten:
        pComprometido = pComprometido + (DREPO.CanApro - DREPO.CanAten).
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

