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

DISABLE TRIGGERS FOR LOAD OF almcmov.
DISABLE TRIGGERS FOR LOAD OF almdmov.
DISABLE TRIGGERS FOR LOAD OF almmmate.
DISABLE TRIGGERS FOR LOAD OF almmmatg.
DISABLE TRIGGERS FOR LOAD OF almacen.

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-codmov AS INT INIT 03 NO-UNDO.        /* TRansferencia */
DEF VAR s-user-id AS CHAR INIT 'AUTOMATICO'.

DEF BUFFER B-CMOV FOR Almcmov.
DEF BUFFER B-DMOV FOR Almdmov.

&SCOPED-DEFINE CONDICION (B-CMOV.CodCia = S-CODCIA AND B-CMOV.CodAlm = Almacen.codalm AND ~
B-CMOV.TipMov = "S" AND B-CMOV.CodMov = S-CODMOV AND B-CMOV.FlgEst <> "A" AND ~
B-CMOV.FlgSit = "T")

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
         HEIGHT             = 9.35
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

IF TODAY < 01/01/2012 THEN RUN Recepciona-Transferencia.
ELSE RUN Anula-Transferencia.

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
FIND B-DMOV WHERE ROWID(B-DMOV) = R-DMov NO-LOCK NO-ERROR.
IF NOT AVAILABLE B-DMOV THEN RETURN.
/* Inicio de Transaccion */
DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN 'ADM-ERROR':
    /* ************** RHC 30.03.04 ******************* */
    FIND B-DMOV WHERE ROWID(B-DMOV) = r-dmov NO-LOCK NO-ERROR.
    ASSIGN I-CODMAT = B-DMOV.CodMaT
           C-CODALM = B-DMOV.CodAlm
           F-CANDES = B-DMOV.CanDes
           F-IMPCTO = B-DMOV.ImpCto.
    IF B-DMOV.Factor > 0 THEN ASSIGN F-CANDES = B-DMOV.CanDes * B-DMOV.Factor.
    /* Buscamos el stock inicial */
    FIND PREV B-DMOV USE-INDEX ALMD02 WHERE B-DMOV.codcia = s-codcia
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
        FIND NEXT B-DMOV USE-INDEX ALMD02 WHERE B-DMOV.codcia = s-codcia
            AND B-DMOV.codmat = i-codmat
            AND B-DMOV.codalm = c-codalm
            NO-LOCK NO-ERROR.
        IF AVAILABLE B-DMOV
        THEN DO:
            FIND CURRENT B-DMOV EXCLUSIVE-LOCK NO-ERROR.
            IF ERROR-STATUS:ERROR THEN UNDO, RETURN "ADM-ERROR".
        END.
    END.
    RELEASE B-DMOV.
END.        

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


DEFINE VARIABLE I-CODMAT AS CHAR NO-UNDO.
DEFINE VARIABLE C-CODALM AS CHAR NO-UNDO.
DEFINE VARIABLE F-CANDES AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PREUNI AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PREUMN AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PREUME AS DECIMAL NO-UNDO.

    /* Ubicamos el detalle a Actualizar */
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
         Almmmate.CodMat = I-CODMAT EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, RETURN 'ADM-ERROR'.

    ASSIGN Almmmate.StkAct = Almmmate.StkAct + F-CANDES.
    RELEASE Almmmate.

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
          Almmmate.CodMat = I-CODMAT EXCLUSIVE-LOCK NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, RETURN 'ADM-ERROR'.
    Almmmate.StkAct = Almmmate.StkAct - F-CANDES.
    RELEASE Almmmate.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Anula-Transferencia) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Anula-Transferencia Procedure 
PROCEDURE Anula-Transferencia :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR r-Rowid AS ROWID NO-UNDO.

DISPLAY 'INICIO' TODAY STRING(TIME, 'HH:MM:SS') WITH STREAM-IO NO-BOX NO-LABELS.
PAUSE 0.

/* NO alm de transf virtual */
CICLO:
FOR EACH Almacen NO-LOCK WHERE Almacen.codcia = s-codcia AND 
    LOOKUP (Almacen.codalm, '999,11T') = 0,
    EACH Almcmov WHERE Almcmov.CodCia = S-CODCIA AND 
    Almcmov.CodAlm = Almacen.codalm AND 
    Almcmov.TipMov = "S" AND 
    Almcmov.CodMov = S-CODMOV AND 
    Almcmov.FlgEst <> "A" AND 
    Almcmov.FlgSit = "T" AND
    Almcmov.FchDoc >= 01/01/2012 AND    /* Solo a partir de este año */
    Almcmov.FchDoc < ( TODAY - 1 )      /* Maximo 1 dia */
    TRANSACTION ON ERROR UNDO, NEXT CICLO ON STOP UNDO, NEXT CICLO:
    /* RHC 07/05/2013 CHEQUEAMOS SI HA SIDO RECEPCIONADA */
    FIND LAST Almrcdoc WHERE Almrcdoc.codcia = s-codcia
        AND Almrcdoc.codalm <> Almcmov.codalm
        AND Almrcdoc.coddoc = "G/R"
        AND Almrcdoc.nrodoc = STRING(Almcmov.nroser, '999') + STRING(Almcmov.nrodoc, '999999')
        NO-LOCK NO-ERROR.
    IF AVAILABLE Almrcdoc THEN NEXT.
    /* ************************************************** */
    /* RHC 17/07/2013 QUE NO ESTE EN UNA HOJA DE RUTA EN TRAMITE */
    FOR EACH di-rutac NO-LOCK WHERE di-rutac.codcia = s-codcia
        AND di-rutac.coddoc = "H/R"
        AND di-rutac.flgest <> "A"
        AND di-rutac.fchdoc >= almcmov.fchdoc:
        FOR EACH di-rutag OF di-rutac NO-LOCK WHERE di-rutag.codalm = almcmov.codalm
            AND di-rutag.tipmov = almcmov.tipmov
            AND di-rutag.codmov = almcmov.codmov
            AND INTEGER(di-rutag.serref) = almcmov.nroser
            AND INTEGER(di-rutag.nroref) = almcmov.nrodoc:
            NEXT CICLO.
        END.
    END.
    /* ********************************************************* */
    RUN Restaura-Pedido.
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, NEXT CICLO.
    /*Valida almacenes*/
    IF almcmov.almdes <> '11T' THEN DO:
        RUN ing-trf-vir-del (ROWID(Almcmov), '999').
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, NEXT CICLO.
    END.
    /* Eliminamos el detalle para el almacen de Origen */
    FOR EACH Almdmov OF Almcmov:
        ASSIGN R-ROWID = ROWID(Almdmov).
        RUN almacstk.
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, NEXT CICLO.
        /* RHC 30.03.04 REACTIVAMOS RUTINA */
        RUN almacpr1 (R-ROWID, "D").
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, NEXT CICLO.
        DELETE Almdmov.
    END.
    ASSIGN 
        Almcmov.FlgEst = 'A'
        Almcmov.Observ = "      A   N   U   L   A   D   O       "
        Almcmov.usuario = S-USER-ID
        Almcmov.FchAnu = TODAY.
END.
DISPLAY 'FIN' TODAY STRING(TIME, 'HH:MM:SS') WITH STREAM-IO NO-BOX NO-LABELS.
PAUSE 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Genera-Detalle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Genera-Detalle Procedure 
PROCEDURE Genera-Detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR R-ROWID AS ROWID NO-UNDO.

DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN "ADM-ERROR":
    FOR EACH B-DMOV OF B-CMOV NO-LOCK:
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
            Almdmov.codmat = B-DMOV.codmat 
            Almdmov.CanDes = B-DMOV.CanDes 
            Almdmov.CodUnd = B-DMOV.CodUnd 
            Almdmov.Factor = B-DMOV.Factor 
            Almdmov.ImpCto = B-DMOV.ImpCto 
            Almdmov.PreUni = B-DMOV.PreUni 
            Almdmov.AlmOri = Almcmov.AlmDes 
            Almdmov.CodAjt = '' 
            Almdmov.HraDoc = Almcmov.HorRcp
            R-ROWID = ROWID(Almdmov).

        RUN ALMACSTK (R-ROWID).
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.

        RUN almacpr1 (R-ROWID, 'U').
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
    END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ing-trf-vir-del) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ing-trf-vir-del Procedure 
PROCEDURE ing-trf-vir-del :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER pRowid AS ROWID.
DEF INPUT PARAMETER pCodAlm AS CHAR.

/* RHC 23/10/2012 RUTINA ELIMINADA */
RETURN "OK".
/* ******************************* */

DEF BUFFER CMOV FOR Almcmov.
DEF BUFFER DMOV FOR Almdmov.

DEF VAR R-ROWID AS ROWID NO-UNDO.

DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    /* Cabecera */
    r-Rowid = ?.
    FOR EACH CMOV USE-INDEX Almc04 WHERE CMOV.codcia = Almcmov.codcia
        AND CMOV.codalm = pCodAlm
        AND CMOV.tipmov = "I"
        AND CMOV.codmov = Almcmov.codmov
        AND CMOV.nroser = 000
        AND CMOV.almdes = Almcmov.codalm
        BY CMOV.FchDoc DESC:
        IF INTEGER(SUBSTRING(CMOV.NroRf1,1,3)) = Almcmov.NroSer
            AND INTEGER(SUBSTRING(CMOV.NroRf1,4)) = Almcmov.NroDoc 
            THEN DO:
            r-Rowid = ROWID(CMOV).
            LEAVE.
        END.
    END.
    IF r-Rowid = ? THEN RETURN 'ADM-ERROR'.
    FIND CMOV WHERE ROWID(CMOV) = r-Rowid EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF ERROR-STATUS:ERROR THEN RETURN 'ADM-ERROR'.                                                                     
    ASSIGN 
        CMOV.FlgEst = 'A'
        CMOV.Observ = "      A   N   U   L   A   D   O       "
        CMOV.Usuario = S-USER-ID.
    FOR EACH DMOV OF CMOV:
        ASSIGN R-ROWID = ROWID(DMOV).
        RUN ALMDCSTK (R-ROWID).   /* Descarga del Almacen POR INGRESOS */
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
        /* RHC 03.04.04 REACTIVAMOS KARDEX POR ALMACEN */
        RUN almacpr1 (R-ROWID, 'D').
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
        DELETE DMOV.
    END.
    RELEASE CMOV.
    RELEASE DMOV.
    IF AVAILABLE(Almmmate) THEN RELEASE Almmmate.
END.
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Recepciona-Transferencia) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Recepciona-Transferencia Procedure 
PROCEDURE Recepciona-Transferencia :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO TRANSACTION ON ERROR UNDO, RETURN ON STOP UNDO, RETURN:
    FOR EACH Almacen NO-LOCK WHERE Almacen.codcia = s-codcia AND Almacen.codalm <> '999':        /* NO alm de transf virtual */
        FOR EACH B-CMOV WHERE {&condicion}:
            IF ( TODAY - B-CMOV.FchDoc ) <= 7 THEN NEXT.   /* Maximo 7 dias */
            FIND Almtdocm WHERE Almtdocm.CodCia = S-CODCIA 
                AND Almtdocm.CodAlm = B-CMOV.AlmDes
                AND Almtdocm.TipMov = "I" 
                AND Almtdocm.CodMov = S-CODMOV
                EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            IF NOT AVAILABLE Almtdocm THEN NEXT.
            CREATE Almcmov.
            ASSIGN 
                Almcmov.usuario = "AUTOMATICO"
                Almcmov.CodCia  = B-CMOV.CodCia 
                Almcmov.CodAlm  = B-CMOV.AlmDes
                Almcmov.AlmDes  = B-CMOV.CodAlm
                Almcmov.TipMov  = "I"
                Almcmov.CodMov  = s-CodMov 
                Almcmov.NroSer  = 000
                Almcmov.FlgSit  = ""
                Almcmov.FchDoc  = TODAY
                Almcmov.NroRf1 = STRING(B-CMOV.NroSer,"999") + STRING(B-CMOV.NroDoc,"999999")
                Almcmov.NroRf3 = B-CMOV.NroRf3
                Almcmov.HorRcp  = STRING(TIME,"HH:MM:SS")
                Almcmov.NomRef = Almacen.Descripcion.
            ASSIGN
                Almcmov.NroDoc  = Almtdocm.NroDoc
                Almtdocm.NroDoc = Almtdocm.NroDoc + 1.

            RUN Genera-Detalle.
            IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.

            /* RHC 24.03.2011 Generamos un movimiento de ingreso en el almacen 10 */
            RUN sal-trf-vir (ROWID(Almcmov), '999').
            IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
            /* ******************************************************************* */

            ASSIGN 
                B-CMOV.FlgSit  = "R" 
                B-CMOV.HorRcp  = STRING(TIME,"HH:MM:SS")
                B-CMOV.NroRf2  = STRING(Almcmov.NroDoc, "999999").
            DISPLAY 'OK:' B-CMOV.codalm b-cmov.nroser b-cmov.nrodoc WITH STREAM-IO NO-BOX WIDTH 200.
            PAUSE 0.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Restaura-Pedido) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Restaura-Pedido Procedure 
PROCEDURE Restaura-Pedido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN "ADM-ERROR":
    CASE Almcmov.codref:
        WHEN 'R/A' THEN DO:     /* Reposicion Automatica */
            FIND Almcrepo WHERE Almcrepo.codcia = s-codcia
                AND Almcrepo.nroser = INTEGER(SUBSTRING(Almcmov.nrorf1,1,3))
                AND Almcrepo.nrodoc = INTEGER(SUBSTRING(Almcmov.nrorf1,4))
                EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF NOT AVAILABLE Almcrepo THEN RETURN "ADM-ERROR".
            FOR EACH almdmov OF almcmov NO-LOCK:
                FIND Almdrepo OF Almcrepo WHERE Almdrepo.codmat = Almdmov.codmat
                    EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
                IF NOT AVAILABLE Almdrepo THEN UNDO, RETURN "ADM-ERROR".
                Almdrepo.CanAten = Almdrepo.CanAten - Almdmov.candes.
                RELEASE Almdrepo.
            END.
            ASSIGN
                almcrepo.FlgEst = 'P'
                almcrepo.HorAct = STRING(TIME, 'HH:MM')
                almcrepo.FecAct = TODAY
                almcrepo.UsrAct = s-user-id.
            RELEASE Almcrepo.
        END.
        WHEN 'PED' THEN DO:
            FIND Almcrequ WHERE Almcrequ.CodCia = Almcmov.codcia 
                AND Almcrequ.CodAlm = Almcmov.AlmDes 
                AND Almcrequ.NroSer = INTEGER(SUBSTRING(Almcmov.NroRf1,1,3)) 
                AND Almcrequ.NroDoc = INTEGER(SUBSTRING(Almcmov.NroRf1,4,6)) 
                EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF NOT AVAILABLE Almcrequ THEN RETURN "ADM-ERROR".
            ASSIGN
                Almcrequ.FlgEst = "P".
            FOR EACH Almdmov OF Almcmov NO-LOCK:
                FIND Almdrequ WHERE Almdrequ.CodCia = Almdmov.CodCia 
                    AND Almdrequ.CodAlm = Almcmov.Almdes 
                    AND Almdrequ.NroSer = INTEGER(SUBSTRING(Almcmov.NroRf1,1,3))
                    AND Almdrequ.NroDoc = INTEGER(SUBSTRING(Almcmov.NroRf1,4,6))
                    AND Almdrequ.CodMat = almdmov.codmat 
                    EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
                IF NOT AVAILABLE Almdrequ THEN UNDO, RETURN "ADM-ERROR".
                Almdrequ.CanDes = Almdrequ.CanDes - Almdmov.candes.
                RELEASE Almdrequ.
            END.
            RELEASE Almcrequ.
        END.
        WHEN 'O/D' THEN DO:
            /* Actualiza Detalle de la Orden de Despacho */
            FOR EACH Almdmov OF Almcmov NO-LOCK:
                FIND Facdpedi WHERE Facdpedi.codcia = s-codcia
                    AND Facdpedi.coddoc = Almcmov.codref
                    AND Facdpedi.nroped = Almcmov.nrorf3
                    AND Facdpedi.codmat = Almdmov.codmat
                    EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                IF NOT AVAILABLE Facdpedi THEN UNDO, RETURN "ADM-ERROR".
                ASSIGN 
                    FacDPedi.CanAte = FacDPedi.CanAte - Almdmov.CanDes.
                IF (FacDPedi.CanPed - FacDPedi.CanAte) <= 0 THEN FacDPedi.FlgEst = "C".
                ELSE FacDPedi.FlgEst = "P".
            END.
            /* Abre la O/D */
            FIND Faccpedi WHERE Faccpedi.codcia = s-codcia
                AND Faccpedi.coddoc = Almcmov.codref
                AND Faccpedi.nroped = Almcmov.nrorf3
                EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF NOT AVAILABLE Faccpedi THEN UNDO, RETURN "ADM-ERROR".
            FIND FIRST Facdpedi OF Faccpedi WHERE (Facdpedi.CanPed > Facdpedi.CanAte) NO-LOCK NO-ERROR.
            IF AVAILABLE Facdpedi THEN Faccpedi.FlgEst = "P".
            ELSE Faccpedi.FlgEst = "C".
        END.
    END CASE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sal-trf-vir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sal-trf-vir Procedure 
PROCEDURE sal-trf-vir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pRowid AS ROWID.
DEF INPUT PARAMETER pCodAlm AS CHAR.

/* RHC 23/10/2012 RUTINA ELIMINADA */
RETURN "OK".
/* ******************************* */

DEF BUFFER CMOV FOR Almcmov.
DEF BUFFER ITEM FOR Almdmov.

DEF VAR R-ROWID AS ROWID NO-UNDO.
DEF VAR x-Item AS INT NO-UNDO.

DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    FIND CMOV WHERE ROWID(CMOV) = pRowid NO-LOCK NO-ERROR.
    IF NOT AVAILABLE CMOV THEN RETURN 'ADM-ERROR'.

    /* Buscamos el correlativo de almacenes */
    FIND Almacen WHERE Almacen.CodCia = CMOV.codcia
        AND Almacen.CodAlm = pCodAlm 
        EXCLUSIVE-LOCK NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, RETURN 'ADM-ERROR'.
    CREATE Almcmov.
    ASSIGN 
        Almcmov.CodCia = CMOV.CodCia 
        Almcmov.CodAlm = pCodAlm 
        Almcmov.AlmDes = CMOV.CodAlm
        Almcmov.TipMov = "S"
        Almcmov.CodMov = CMOV.CodMov
        Almcmov.FlgSit = "R"      /* Recepcionado */
        Almcmov.FchDoc = TODAY
        Almcmov.HorSal = STRING(TIME,"HH:MM")
        Almcmov.HraDoc = STRING(TIME,"HH:MM")
        Almcmov.NroSer = 000
        Almcmov.NroDoc = Almacen.CorrSal
        Almcmov.NroRf1 = STRING(CMOV.NroSer,"999") + STRING(CMOV.NroDoc,"999999")
        Almacen.CorrSal = Almacen.CorrSal + 1
        Almcmov.usuario = 'AUTOMATICO'.
    FOR EACH ITEM OF CMOV NO-LOCK BY ITEM.NroItm:
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
               Almdmov.HraDoc = Almcmov.HorSal
               Almdmov.NroItm = x-Item
               R-ROWID = ROWID(Almdmov).
        x-Item = x-Item + 1.
        RUN almdcstk (R-ROWID).
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
        RUN almacpr1 (R-ROWID, "U").
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
    END.
END.

RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

