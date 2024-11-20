&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
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
DEF VAR s-codalm AS CHAR INIT '500' NO-UNDO.
DEF VAR cl-codcia AS INT INIT 000   NO-UNDO.
DEF VAR s-user-id AS CHAR INIT 'ADMIN' NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF gn-clie.
DISABLE TRIGGERS FOR LOAD OF ccbcdocu.

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
         HEIGHT             = 9
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DEF TEMP-TABLE T-Comprobantes NO-UNDO 
    LIKE OOComprobantes 
    FIELD t-Rowid AS ROWID.

DEF BUFFER B-Comprobantes FOR OOComprobantes.
/* SOLO FACTURAS */
FOR EACH OOComprobantes WHERE LOOKUP(OOComprobantes.CodDoc, "FAC,N/C") > 0
    AND OOComprobantes.FlagMigracion = "N"
    BREAK BY OOComprobantes.CodDoc BY OOComprobantes.NroDoc:
    IF FIRST-OF(OOComprobantes.CodDoc) OR FIRST-OF(OOComprobantes.NroDoc)
        THEN EMPTY TEMP-TABLE T-Comprobantes.
    CREATE T-Comprobantes.
    BUFFER-COPY OOComprobantes TO T-Comprobantes
        ASSIGN 
        T-Comprobantes.LinPreUni = ABS(OOComprobantes.LinPreUni)
        T-Comprobantes.LinPorPer = ABS(OOComprobantes.LinPorPer) 
        T-Comprobantes.LinPorDto = ABS(OOComprobantes.LinPorDto) 
        T-Comprobantes.LinImpPer = ABS(OOComprobantes.LinImpPer) 
        T-Comprobantes.LinImpLin = ABS(OOComprobantes.LinImpLin) 
        T-Comprobantes.LinImpImp = ABS(OOComprobantes.LinImpImp) 
        T-Comprobantes.LinImpIgv = ABS(OOComprobantes.LinImpIgv) 
        T-Comprobantes.LinImpDto = ABS(OOComprobantes.LinImpDto) 
        T-Comprobantes.ImpVta = ABS(OOComprobantes.ImpVta) 
        T-Comprobantes.ImpTot = ABS(OOComprobantes.ImpTot) 
        T-Comprobantes.ImpPer = ABS(OOComprobantes.ImpPer) 
        T-Comprobantes.ImpImp = ABS(OOComprobantes.ImpImp) 
        T-Comprobantes.ImpIgv = ABS(OOComprobantes.ImpIgv) 
        T-Comprobantes.ImpExo = ABS(OOComprobantes.ImpExo) 
        T-Comprobantes.ImpBrt = ABS(OOComprobantes.ImpBrt) 
        T-Comprobantes.PorIgv = ABS(OOComprobantes.PorIgv) 
        T-Comprobantes.TpoCmb = ABS(OOComprobantes.TpoCmb) 
        T-Comprobantes.PorPer = ABS(OOComprobantes.PorPer) 
        T-Comprobantes.CodMon = ABS(OOComprobantes.CodMon)
        T-Comprobantes.SdoAct = ABS(OOComprobantes.SdoAct)
        T-Comprobantes.NroDoc = REPLACE(T-Comprobantes.NroDoc, '-', '')
        T-Comprobantes.T-Rowid = ROWID(OOComprobantes).

    IF LAST-OF(OOComprobantes.CodDoc) OR LAST-OF(OOComprobantes.NroDoc) THEN DO:
        /* Clientes */
        RUN Crea-Cliente.
        /* Comprobantes */
        RUN Crea-Cabecera-Detalle.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-act_alm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE act_alm Procedure 
PROCEDURE act_alm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER X-ROWID AS ROWID.

FIND ccbcdocu WHERE ROWID(ccbcdocu) = X-ROWID NO-LOCK NO-ERROR.
IF NOT AVAILABLE ccbcdocu THEN RETURN "OK".
IF LOOKUP(Ccbcdocu.coddoc, 'FAC,BOL,TCK') = 0 THEN RETURN.

DEF VAR i AS INTEGER INITIAL 1 NO-UNDO.
DEF VAR LocalCounter AS INTEGER INITIAL 0 NO-UNDO.

PRINCIPAL:
DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN 'ADM-ERROR':
    FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK WHERE Ccbddocu.implin >= 0
        BREAK BY Ccbddocu.AlmDes:
        IF FIRST-OF(Ccbddocu.AlmDes) THEN DO:
            /* CABECERA */
            REPEAT:
                FIND Almacen WHERE Almacen.CodCia = Ccbddocu.codcia
                    AND Almacen.CodAlm = Ccbddocu.almdes       /* OJO */
                    EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
                IF NOT AVAILABLE Almacen THEN DO:
                    IF LOCKED Almacen THEN UNDO, RETRY.
                    ELSE DO:
                        DISPLAY
                            'ERROR: Almacén:' Ccbddocu.almdes '<< NO registrado'
                            'Se ha abortado la grabación' 
                            WITH STREAM-IO NO-BOX NO-LABELS.
                        UNDO PRINCIPAL, RETURN 'ADM-ERROR'.
                    END.
                END.
                LEAVE.
            END.
            /* ************************* */
            CREATE almcmov.
            ASSIGN Almcmov.CodCia  = CcbDDocu.CodCia 
                   Almcmov.CodAlm  = CcbDDocu.AlmDes
                   Almcmov.TipMov  = "S"
                   Almcmov.CodMov  = 02     /* FIJO */
                   Almcmov.NroSer  = 0 /* INTEGER(SUBSTRING(ccbcdocu.NroDoc,1,3)) */
                   Almcmov.NroDoc  = Almacen.CorrSal 
                   Almacen.CorrSal = Almacen.CorrSal + 1
                   Almcmov.FchDoc  = CcbCDocu.FchDoc
                   Almcmov.HorSal  = STRING(TIME, "HH:MM:SS")
                   Almcmov.CodVen  = ccbcdocu.CodVen
                   Almcmov.CodCli  = ccbcdocu.CodCli
                   Almcmov.Nomref  = ccbcdocu.NomCli
                   Almcmov.CodRef  = ccbcdocu.CodDoc
                   Almcmov.NroRef  = ccbcdocu.nrodoc
                   Almcmov.NroRf1  = STRING(CcbCDocu.CodDoc, 'x(3)') + CcbCDocu.NroDoc
                   Almcmov.NroRf2  = CcbCDocu.CodPed + CcbCDocu.NroPed
                   Almcmov.usuario = s-user-id
                NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                DISPLAY 
                    'ERROR: Ha ocurrido un error inesperado al grabar los movimientos de almacén' 
                    WITH STREAM-IO NO-BOX NO-LABELS.
                UNDO PRINCIPAL, RETURN "ADM-ERROR".
            END.
        END.
        /* DETALLE */
        CREATE Almdmov.
        ASSIGN Almdmov.CodCia = Almcmov.CodCia
               Almdmov.CodAlm = Almcmov.CodAlm
               Almdmov.CodMov = Almcmov.CodMov 
               Almdmov.NroSer = almcmov.nroser
               Almdmov.NroDoc = almcmov.nrodoc
               Almdmov.AftIgv = ccbddocu.aftigv
               Almdmov.AftIsc = ccbddocu.aftisc
               Almdmov.CanDes = ccbddocu.candes
               Almdmov.codmat = ccbddocu.codmat
               Almdmov.CodMon = ccbcdocu.codmon
               Almdmov.CodUnd = ccbddocu.undvta
               Almdmov.Factor = ccbddocu.factor
               Almdmov.FchDoc = CcbCDocu.FchDoc
               Almdmov.ImpDto = ccbddocu.impdto
               Almdmov.ImpIgv = ccbddocu.impigv
               Almdmov.ImpIsc = ccbddocu.impisc
               Almdmov.ImpLin = ccbddocu.implin
               Almdmov.NroItm = i
               Almdmov.PorDto = ccbddocu.pordto
               Almdmov.PreBas = ccbddocu.prebas
               Almdmov.PreUni = ccbddocu.preuni
               Almdmov.TipMov = "S"
               Almdmov.TpoCmb = ccbcdocu.tpocmb
               Almcmov.TotItm = i
               Almdmov.HraDoc = Almcmov.HorSal
               i = i + 1.
        /* EXISTE UNA RUTINA QUE ACTUALIZA LOS STOCKS DE LOS ALMACEN DE UTILEX */
        RUN almdcstk (ROWID(almdmov)).
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO PRINCIPAL, RETURN 'ADM-ERROR'.
        /* RHC 05.04.04 ACTIVAMOS KARDEX POR ALMACEN */
        RUN almacpr1 (ROWID(almdmov), 'U').
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO PRINCIPAL, RETURN 'ADM-ERROR'.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

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
DEF BUFFER B-DMOV FOR Almdmov.

FIND B-DMOV WHERE ROWID(B-DMOV) = R-DMov NO-LOCK NO-ERROR.
IF NOT AVAILABLE B-DMOV THEN RETURN 'OK'.

/* Inicio de Transaccion */
CICLO:
DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN 'ADM-ERROR':
    /* ************** RHC 30.03.04 ******************* */
    FIND B-DMOV WHERE ROWID(B-DMOV) = r-dmov NO-LOCK NO-ERROR.
    ASSIGN I-CODMAT = B-DMOV.CodMaT
           C-CODALM = B-DMOV.CodAlm
           F-CANDES = B-DMOV.CanDes
           F-IMPCTO = B-DMOV.ImpCto.
    IF B-DMOV.Factor > 0 THEN ASSIGN F-CANDES = B-DMOV.CanDes * B-DMOV.Factor.
    /* Buscamos el stock inicial */
    FIND PREV B-DMOV USE-INDEX ALMD03 WHERE B-DMOV.codcia = s-codcia
        AND B-DMOV.codmat = i-codmat
        AND B-DMOV.codalm = c-codalm
        NO-LOCK NO-ERROR.
    IF AVAILABLE B-DMOV
    THEN f-StkSub = B-DMOV.stksub.
    ELSE f-StkSub = 0.
    /* actualizamos el kardex por almacen */
    FIND B-DMOV WHERE ROWID(B-DMOV) = r-dmov EXCLUSIVE-LOCK NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, RETURN 'ADM-ERROR'.
    IF C-UD = 'D'       /* Eliminar */
    THEN B-DMOV.candes = 0.        /* OJO */
    REPEAT WHILE AVAILABLE B-DMOV:
        L-INGRESO = LOOKUP(B-DMOV.TipMov,"I,U") <> 0.
        F-CANDES = B-DMOV.CanDes.
        IF B-DMOV.Factor > 0 THEN F-CANDES = B-DMOV.CanDes * B-DMOV.Factor.
        IF L-INGRESO 
        THEN F-STKSUB = F-STKSUB + F-CANDES.
        ELSE F-STKSUB = F-STKSUB - F-CANDES.
        B-DMOV.StkSub = F-STKSUB.      /* OJO */
        FIND NEXT B-DMOV USE-INDEX ALMD03 WHERE B-DMOV.codcia = s-codcia
            AND B-DMOV.codmat = i-codmat
            AND B-DMOV.codalm = c-codalm
            NO-LOCK NO-ERROR.
        IF AVAILABLE B-DMOV
        THEN DO:
            FIND CURRENT B-DMOV EXCLUSIVE-LOCK NO-ERROR.
            IF ERROR-STATUS:ERROR THEN UNDO CICLO, RETURN "ADM-ERROR".
        END.
    END.
END.        
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-almacstk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE almacstk Procedure 
PROCEDURE almacstk :
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
    IF NOT AVAILABLE AlmDMov THEN RETURN 'ADM-ERROR'.

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

CICLO:
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
    IF NOT AVAILABLE Almmmate THEN UNDO, RETURN 'ADM-ERROR'.
/*     IF NOT AVAILABLE Almmmate THEN DO:                                 */
/*         MESSAGE 'Codigo' i-codmat 'NO asignado en el almacen' c-codalm */
/*             VIEW-AS ALERT-BOX ERROR.                                   */
/*         UNDO, RETURN 'ADM-ERROR'.                                      */
/*     END.                                                               */
    Almmmate.StkAct = Almmmate.StkAct - F-CANDES.
END.

RETURN 'OK'.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Crea-Cabecera-Detalle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Crea-Cabecera-Detalle Procedure 
PROCEDURE Crea-Cabecera-Detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR x-Cto1 AS DEC NO-UNDO.
DEF VAR x-Cto2 AS DEC NO-UNDO.
DEF VAR x-NroDoc AS CHAR NO-UNDO.
DEF VAR x-CodDoc AS CHAR NO-UNDO.

CICLO:
DO TRANSACTION ON ERROR UNDO, RETURN ON STOP UNDO, RETURN:
    ASSIGN
        x-CodDoc = OOComprobantes.CodDoc
        x-NroDoc = REPLACE(OOComprobantes.NroDoc, '-', '').     /* <<< OJO CON ESTO */

    IF OOComprobantes.CodDoc = "NC" THEN x-CodDoc = "N/C".

    FIND FIRST Ccbcdocu  WHERE Ccbcdocu.codcia = s-codcia
        AND Ccbcdocu.coddiv =  OOComprobantes.CodDiv
        AND Ccbcdocu.coddoc = x-CodDoc 
        AND Ccbcdocu.nrodoc = x-NroDoc
        NO-LOCK NO-ERROR.
    IF AVAILABLE Ccbcdocu THEN DO:
        FIND CURRENT Ccbcdocu EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF NOT AVAILABLE Ccbcdocu THEN UNDO CICLO, RETURN.
        /* DESCARGA ALMACENES */
        RUN des_alm (ROWID(CcbCDocu)).
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO CICLO, RETURN.
    END.
    ELSE CREATE Ccbcdocu.

    BUFFER-COPY OOComprobantes
        TO CcbCDocu
        ASSIGN
        CcbCDocu.CodCia = s-codcia
        CcbCDocu.CodDoc = x-CodDoc
        CcbCDocu.NroDoc = x-NroDoc
        CcbCDocu.ImpIsc = OOComprobantes.ImpImp.

    FOR EACH T-Comprobantes:
        FIND Ccbddocu OF Ccbcdocu WHERE Ccbddocu.codmat = T-Comprobantes.CodMat
            NO-LOCK NO-ERROR.
        IF AVAILABLE Ccbddocu THEN DO:
            FIND CURRENT Ccbddocu EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF NOT AVAILABLE Ccbddocu THEN UNDO CICLO, RETURN.
        END.
        ELSE CREATE Ccbddocu.
        BUFFER-COPY Ccbcdocu
            TO Ccbddocu
            ASSIGN
            CcbDDocu.AlmDes = T-Comprobantes.CodAlm
            CcbDDocu.CodMat = T-Comprobantes.CodMat 
            CcbDDocu.CanDes = T-Comprobantes.CanDes 
            CcbDDocu.UndVta = T-Comprobantes.UndVta
            CcbDDocu.Factor = T-Comprobantes.Factor
            CcbDDocu.PreUni = T-Comprobantes.LinPreUni
            CcbDDocu.Por_Dsctos[1]  = T-Comprobantes.LinPorDto 
            CcbDDocu.ImpDto = T-Comprobantes.LinImpDto 
            CcbDDocu.ImpIgv = T-Comprobantes.LinImpIgv 
            CcbDDocu.ImpIsc = T-Comprobantes.LinImpImp 
            CcbDDocu.ImpLin = T-Comprobantes.LinImpLin 
            ccbddocu.PorDcto_Adelanto[5] = T-Comprobantes.LinPorPer 
            ccbddocu.ImpDcto_Adelanto[5] = T-Comprobantes.LinImpPer
            CcbDDocu.AftIgv = (IF CcbDDocu.ImpIgv > 0 THEN YES ELSE NO)
            CcbDDocu.AftIsc = (IF CcbDDocu.ImpIsc > 0 THEN YES ELSE NO).
        /* Importe Costo */
        FIND Almmmatg OF Ccbddocu NO-LOCK NO-ERROR.
        IF AVAILABLE Almmmatg AND NEW Ccbddocu THEN DO:
            IF almmmatg.monvta = 1 THEN DO:
                x-cto1 = ROUND( Almmmatg.Ctotot * Ccbddocu.CanDes * Ccbddocu.Factor , 2 ).
                x-cto2 = ROUND(( Almmmatg.Ctotot * Ccbddocu.CanDes * Ccbddocu.Factor ) /  Almmmatg.Tpocmb , 2 ).
            END.
            IF almmmatg.monvta = 2 THEN DO:
                x-cto1 = ROUND( Almmmatg.Ctotot * Ccbddocu.CanDes * Ccbddocu.Factor * Almmmatg.TpoCmb, 2 ).
                x-cto2 = ROUND(( Almmmatg.Ctotot * Ccbddocu.CanDes * Ccbddocu.Factor ) , 2 ).
            END.
            ASSIGN
                Ccbddocu.ImpCto = ( IF Ccbcdocu.codmon = 1 THEN x-cto1 ELSE x-cto2 )
                CcbDDocu.Pesmat = Almmmatg.Pesmat * (CcbDDocu.Candes * CcbDDocu.Factor).
        END.
    END.

    RUN act_alm( ROWID(Ccbcdocu) ).
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO CICLO, RETURN.
    
    /* Transferido */
    FOR EACH T-Comprobantes:
        FIND B-Comprobantes WHERE ROWID(B-Comprobantes ) = T-Comprobantes.t-rowid
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF ERROR-STATUS:ERROR THEN UNDO CICLO, RETURN.
        ASSIGN
            B-Comprobantes.FlagMigracion = "S".
    END.
END.

END PROCEDURE.

/*
DO TRANSACTION ON ERROR UNDO, RETURN ERROR ON STOP UNDO, RETURN ERROR:
    FIND FIRST Ccbcdocu  WHERE Ccbcdocu.codcia = s-codcia
        AND Ccbcdocu.coddiv =  OOComprobantes.CodDiv
        AND Ccbcdocu.coddoc = OOComprobantes.CodDoc 
        AND Ccbcdocu.nrodoc = OOComprobantes.NroDoc
        NO-LOCK NO-ERROR.
    IF AVAILABLE Ccbcdocu THEN DO:
        FIND CURRENT Ccbcdocu EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF NOT AVAILABLE Ccbcdocu THEN UNDO, RETURN ERROR.
    END.
    ELSE CREATE Ccbcdocu.
    BUFFER-COPY OOComprobantes
        TO CcbCDocu
        ASSIGN
        CcbCDocu.CodCia = s-codcia
        CcbCDocu.ImpIsc = OOComprobantes.ImpImp.

    IF OOComprobantes.CodMat = "" THEN RETURN.

    FIND Ccbddocu OF Ccbcdocu WHERE Ccbddocu.codmat = OOComprobantes.CodMat
        NO-LOCK NO-ERROR.
    IF AVAILABLE Ccbddocu THEN DO:
        FIND CURRENT Ccbddocu EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF NOT AVAILABLE Ccbddocu THEN UNDO, RETURN ERROR.
    END.
    ELSE CREATE Ccbddocu.
    
    BUFFER-COPY Ccbcdocu
        TO Ccbddocu
        ASSIGN
        CcbDDocu.AlmDes = OOComprobantes.CodAlm
        CcbDDocu.CodMat = OOComprobantes.CodMat 
        CcbDDocu.CanDes = OOComprobantes.CanDes 
        CcbDDocu.UndVta = OOComprobantes.UndVta
        CcbDDocu.Factor = OOComprobantes.Factor
        CcbDDocu.PreUni = OOComprobantes.LinPreUni
        CcbDDocu.Por_Dsctos[1]  = OOComprobantes.LinPorDto 
        CcbDDocu.ImpDto = OOComprobantes.LinImpDto 
        CcbDDocu.ImpIgv = OOComprobantes.LinImpIgv 
        CcbDDocu.ImpIsc = OOComprobantes.LinImpImp 
        CcbDDocu.ImpLin = OOComprobantes.LinImpLin 
        ccbddocu.PorDcto_Adelanto[5] = OOComprobantes.LinPorPer 
        ccbddocu.ImpDcto_Adelanto[5] = OOComprobantes.LinImpPer
        CcbDDocu.AftIgv = (IF CcbDDocu.ImpIgv > 0 THEN YES ELSE NO)
        CcbDDocu.AftIsc = (IF CcbDDocu.ImpIsc > 0 THEN YES ELSE NO).
    /* Importe Costo */
    FIND Almmmatg OF Ccbddocu NO-LOCK NO-ERROR.
    IF AVAILABLE Almmmatg AND NEW Ccbddocu THEN DO:
        IF almmmatg.monvta = 1 THEN DO:
            x-cto1 = ROUND( Almmmatg.Ctotot * Ccbddocu.CanDes * Ccbddocu.Factor , 2 ).
            x-cto2 = ROUND(( Almmmatg.Ctotot * Ccbddocu.CanDes * Ccbddocu.Factor ) /  Almmmatg.Tpocmb , 2 ).
        END.
        IF almmmatg.monvta = 2 THEN DO:
            x-cto1 = ROUND( Almmmatg.Ctotot * Ccbddocu.CanDes * Ccbddocu.Factor * Almmmatg.TpoCmb, 2 ).
            x-cto2 = ROUND(( Almmmatg.Ctotot * Ccbddocu.CanDes * Ccbddocu.Factor ) , 2 ).
        END.
        ASSIGN
            Ccbddocu.ImpCto = ( IF Ccbcdocu.codmon = 1 THEN x-cto1 ELSE x-cto2 )
            CcbDDocu.Pesmat = Almmmatg.Pesmat * (CcbDDocu.Candes * CcbDDocu.Factor).
    END.
    /* Transferido */
    ASSIGN
        OOComprobantes.FlagMigracion = "S".
END.
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Crea-Cliente) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Crea-Cliente Procedure 
PROCEDURE Crea-Cliente :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND FIRST gn-clie WHERE gn-clie.codcia = cl-codcia
    AND gn-clie.codcli = OOComprobantes.codcli
    NO-LOCK NO-ERROR NO-WAIT.
IF AVAILABLE gn-clie THEN RETURN.

/* SOLO SON EMPRESAS */
CREATE gn-clie.
ASSIGN
    gn-clie.CodCia = cl-codcia
    gn-clie.CodCli = OOComprobantes.CodCli
    gn-clie.NomCli = OOComprobantes.NomCli
    gn-clie.DirCli = OOComprobantes.DirCli
    gn-clie.Ruc    = OOComprobantes.RucCli
    gn-clie.Libre_C01 = 'J'
    gn-clie.Nombre = OOComprobantes.NomCli
/*     gn-clie.DNI    = OOComprobantes.CodAnt */
/*     gn-clie.ApeMat = OOComprobantes.ApeMat */
/*     gn-clie.ApePat = OOComprobantes.ApePat */
/*     gn-clie.Nombre = OOComprobantes.Nombre */
    gn-clie.clfCli = 'C'
    gn-clie.CodDept = OOComprobantes.CodDept
    gn-clie.CodDist = OOComprobantes.CodDist
    gn-clie.CodProv = OOComprobantes.CodProv
    gn-clie.DirEnt = OOComprobantes.LugEnt
    gn-clie.Libre_L02 = YES.        /* Cliente SOLO OPENORANGE */
ASSIGN
    gn-clie.rucold = (IF OOComprobantes.AgeRet = 1 THEN 'Si' ELSE 'No')
    gn-clie.libre_l01 = (IF OOComprobantes.AgePer = 1 THEN YES ELSE NO).

RELEASE gn-clie.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-des_alm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE des_alm Procedure 
PROCEDURE des_alm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER X-ROWID AS ROWID.

DEF BUFFER B-CDOCU FOR Ccbcdocu.
CICLO:
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    FIND B-CDOCU WHERE ROWID(B-CDOCU) = X-ROWID NO-LOCK NO-ERROR.
    IF NOT AVAILABLE B-CDOCU THEN RETURN "OK".
    FOR EACH almcmov WHERE almcmov.codcia = B-CDOCU.codcia
        AND almcmov.codref = B-CDOCU.coddoc
        AND almcmov.nroref = B-CDOCU.nrodoc
        AND almcmov.tipmov = 'S'
        AND almcmov.codmov = 02:    /* SALIDA POR VENTAS */
        FOR EACH almdmov OF almcmov:
            RUN almacstk (ROWID(almdmov)).
            IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO CICLO, RETURN 'ADM-ERROR'.
            /* RHC 05.04.04 ACTIVAMOS KARDEX POR ALMACEN */
            RUN almacpr1 (ROWID(almdmov), 'D').
            IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO CICLO, RETURN 'ADM-ERROR'.
            DELETE almdmov.
        END.
        ASSIGN almcmov.flgest = "A".
    END.
END.
RETURN "OK".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Elimina-Comprobante) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Elimina-Comprobante Procedure 
PROCEDURE Elimina-Comprobante :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ELIMINACION:
  DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN 'ADM-ERROR':
      /* DESCARGA ALMACENES */
      RUN vta2/des_alm (ROWID(CcbCDocu)).
      IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

