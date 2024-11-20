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
         HEIGHT             = 5.23
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DISABLE TRIGGERS FOR LOAD OF almcmov.
DISABLE TRIGGERS FOR LOAD OF almdmov.

/* RHC 03.06.2011 Reclasificar productos con stock negativo */

DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-codalm AS CHAR INIT '10'.
DEF VAR x-Control AS LOG INIT YES.
DEF VAR s-codmov AS INT INIT 16.
DEF VAR s-nroser AS INT INIT 000.
DEF VAR f-TpoCmb AS DEC INIT 1.
DEF VAR N-Itm AS INT.

DEF BUFFER B-CMOV FOR almcmov.
DEF BUFFER B-MATG FOR almmmatg.
DEF BUFFER B-MATE FOR Almmmate.
DEF VAR r-Rowid AS ROWID.

/* Consistencia del movimiento */
FIND Almtmovm WHERE Almtmovm.codcia = s-codcia
    AND Almtmovm.tipmov = 'I'
    AND Almtmovm.codmov = s-codmov
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almtmovm THEN DO:
    DISPLAY '** ERROR: Movimiento de ingreso' s-codmov 'no configurado'
        WITH STREAM-IO NO-BOX NO-LABELS.
    PAUSE 0.
    RETURN.
END.
FIND Almtmovm WHERE Almtmovm.codcia = s-codcia
    AND Almtmovm.tipmov = 'S'
    AND Almtmovm.codmov = s-codmov
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almtmovm THEN DO:
    DISPLAY '** ERROR: Movimiento de salida' s-codmov 'no configurado'
        WITH STREAM-IO NO-BOX NO-LABELS.
    PAUSE 0.
    RETURN.
END.
FIND Almtdocm WHERE Almtdocm.CodCia = s-codcia
    AND Almtdocm.CodAlm = s-codalm
    AND Almtdocm.TipMov = 'I'
    AND Almtdocm.CodMov = s-codmov
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almtdocm THEN DO:
    DISPLAY '** ERROR: Movimiento de ingreso' s-codmov 'no configurado an el almacén' s-codalm
        WITH STREAM-IO NO-BOX NO-LABELS.
    PAUSE 0.
    RETURN.
END.

DO TRANSACTION ON ERROR UNDO, RETURN ON STOP UNDO, RETURN:
    N-Itm = 0.
    FOR EACH AlmMatEqu NO-LOCK WHERE AlmMatEqu.codcia = s-codcia,
        FIRST Almmmatg OF Almmatequ NO-LOCK,
        FIRST B-MATG WHERE B-MATG.codcia = s-codcia
        AND B-MATG.codmat =  AlmMatEqu.codmat2,
        EACH B-MATE NO-LOCK WHERE B-MATE.codcia = s-codcia
        AND B-MATE.codalm = s-codalm
        AND B-MATE.codmat = AlmMatEqu.codmat
        AND B-MATE.stkact < 0:
        IF x-Control = YES THEN DO:
            /* CABECERAS */
            FIND Almtdocm WHERE Almtdocm.CodCia = s-codcia
                AND Almtdocm.CodAlm = s-codalm
                AND Almtdocm.TipMov = 'S'
                AND Almtdocm.CodMov = s-codmov
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Almtdocm THEN DO:
                DISPLAY '** ERROR: Movimiento de salida' s-codmov 'no configurado en el almacén' s-codalm
                    WITH STREAM-IO NO-BOX NO-LABELS.
                PAUSE 0.
                UNDO, RETURN.
            END.
            FIND Almacen WHERE Almacen.CodCia = S-CODCIA 
                AND Almacen.CodAlm = s-CodAlm 
                EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF ERROR-STATUS:ERROR THEN DO:
                DISPLAY '** ERROR: No se pudo bloquear el movimiento de salida'
                    WITH STREAM-IO NO-BOX NO-LABELS.
                PAUSE 0.
                UNDO, RETURN.
            END.
            /* MOVIMIENTO DE SALIDA */
            CREATE Almcmov.
            ASSIGN 
                Almcmov.CodCia = Almtdocm.CodCia 
                Almcmov.CodAlm = Almtdocm.CodAlm 
                Almcmov.TipMov = Almtdocm.TipMov
                Almcmov.CodMov = Almtdocm.CodMov
                Almcmov.NroSer = S-NROSER
                Almcmov.Nrodoc  = Almacen.CorrSal
                Almcmov.HorSal = STRING(TIME,"HH:MM:SS")
                Almcmov.TpoCmb  = F-TPOCMB.
            /* MOVIMIENTO DE ENTRADA */
            CREATE B-CMOV.
            BUFFER-COPY Almcmov TO B-CMOV
                ASSIGN
                    B-CMOV.TipMov = "I"
                    B-CMOV.Nrodoc = Almacen.CorrIng
                    B-CMOV.NroRef = STRING(Almcmov.nroser, '999') + STRING(Almcmov.nrodoc, '9999999').
            ASSIGN 
                Almcmov.NroRef = STRING(B-CMOV.nroser, '999') + STRING(B-CMOV.nrodoc, '9999999')
                Almacen.CorrSal = Almacen.CorrSal + 1
                Almacen.CorrIng = Almacen.CorrIng + 1.

            x-Control = NO.
        END.
        /* ************************** DETALLE ************************** */
        /* SALIDAS */
        N-Itm = N-Itm + 1.
        CREATE almdmov.
        ASSIGN 
            Almdmov.CodCia = Almcmov.CodCia 
            Almdmov.CodAlm = Almcmov.CodAlm 
            Almdmov.TipMov = Almcmov.TipMov 
            Almdmov.CodMov = Almcmov.CodMov 
            Almdmov.NroSer = Almcmov.NroSer 
            Almdmov.NroDoc = Almcmov.NroDoc 
            Almdmov.CodMon = Almcmov.CodMon 
            Almdmov.FchDoc = Almcmov.FchDoc 
            Almdmov.TpoCmb = Almcmov.TpoCmb
            Almdmov.codmat = AlmMatEqu.codmat2
            Almdmov.codant = AlmMatEqu.codmat   /* OJO */
            Almdmov.CanDes = ABSOLUTE(B-MATE.StkAct) *  AlmMatEqu.Factor
            Almdmov.CanDev = ABSOLUTE(B-MATE.StkAct)
            Almdmov.CodUnd = B-MATG.UndStk
            Almdmov.Factor = 1
    /*         Almdmov.PreBas = ITEM.PreBas */
    /*         Almdmov.ImpCto = ITEM.ImpCto */
    /*         Almdmov.PreUni = ITEM.PreUni */
            Almdmov.NroItm = N-Itm
            Almdmov.CodAjt = ''
            Almdmov.HraDoc = Almcmov.HorSal
            R-ROWID = ROWID(Almdmov).
        RUN almdcstk (R-ROWID).
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN.
        RUN ALMACPR1 (R-ROWID,"U").
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN.

        /* ENTRADAS */
        CREATE almdmov.
        ASSIGN 
            Almdmov.CodCia = B-CMOV.CodCia 
            Almdmov.CodAlm = B-CMOV.CodAlm 
            Almdmov.TipMov = B-CMOV.TipMov 
            Almdmov.CodMov = B-CMOV.CodMov 
            Almdmov.NroSer = B-CMOV.NroSer 
            Almdmov.NroDoc = B-CMOV.NroDoc 
            Almdmov.CodMon = B-CMOV.CodMon 
            Almdmov.FchDoc = B-CMOV.FchDoc 
            Almdmov.TpoCmb = B-CMOV.TpoCmb
            Almdmov.codmat = AlmMatEqu.codmat
            Almdmov.codant = AlmMatEqu.codmat2
            Almdmov.CodUnd = Almmmatg.UndStk
            Almdmov.Factor = 1
            Almdmov.NroItm = N-Itm
            Almdmov.CodAjt = 'A'
            Almdmov.HraDoc = B-CMOV.HorRcp
            R-ROWID = ROWID(Almdmov).
        ASSIGN 
            Almdmov.CanDes = ABSOLUTE(B-MATE.StkAct)
            Almdmov.CanDev = ABSOLUTE(B-MATE.StkAct) *  AlmMatEqu.Factor.
    /*         Almdmov.ImpCto = Almdmov.ImpCto + ITEM.ImpCto     */
    /*         Almdmov.PreUni = Almdmov.ImpCto / Almdmov.CanDes. */

        FIND FIRST Almtmovm WHERE Almtmovm.CodCia = B-CMOV.CodCia 
            AND  Almtmovm.Tipmov = B-CMOV.TipMov 
            AND  Almtmovm.Codmov = B-CMOV.CodMov 
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almtmovm AND Almtmovm.PidPCo 
            THEN ASSIGN Almdmov.CodAjt = "A".
        ELSE ASSIGN Almdmov.CodAjt = ''.
        RUN ALMACSTK (R-ROWID).
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
        RUN ALMACPR1 (R-ROWID,"U").
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'. 
    END.
    IF AVAILABLE(Almacen) THEN RELEASE Almacen.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ALMACPR1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ALMACPR1 Procedure 
PROCEDURE ALMACPR1 :
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
    IF NOT AVAILABLE AlmDMov THEN RETURN.
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
        FIND PREV almdmov USE-INDEX ALMD02 WHERE almdmov.codcia = s-codcia
            AND almdmov.codmat = i-codmat
            AND almdmov.codalm = c-codalm
            NO-LOCK NO-ERROR.
        IF AVAILABLE almdmov
        THEN f-StkSub = almdmov.stksub.
        ELSE f-StkSub = 0.
        /* actualizamos el kardex por almacen */
        FIND almdmov WHERE ROWID(almdmov) = r-dmov EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
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
            FIND NEXT almdmov USE-INDEX ALMD02 WHERE almdmov.codcia = s-codcia
                AND almdmov.codmat = i-codmat
                AND almdmov.codalm = c-codalm
                NO-LOCK NO-ERROR.
            IF AVAILABLE Almdmov
            THEN DO:
                FIND CURRENT almdmov EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
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
         Almmmate.CodMat = I-CODMAT EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
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

DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    /* Ubicamos el detalle a Actualizar */
    FIND AlmDMov WHERE ROWID(AlmDMov) = R-DMov NO-LOCK NO-ERROR.
    IF NOT AVAILABLE AlmDMov THEN RETURN.
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
          Almmmate.CodMat = I-CODMAT EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF NOT AVAILABLE Almmmate THEN DO:
        DISPLAY '** ERROR: Codigo' i-codmat 'NO asignado en el almacen' c-codalm
            WITH STREAM-IO NO-BOX NO-LABELS.
        PAUSE 0.
        RETURN 'ADM-ERROR'.
    END.
    Almmmate.StkAct = Almmmate.StkAct - F-CANDES.
    RELEASE Almmmate.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

