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
    Notes       : ,
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Valores preestablecidos */
DEF VAR s-codcia AS INT  INIT 001   NO-UNDO.
DEF VAR s-codalm AS CHAR            NO-UNDO.
DEF VAR cl-codcia AS INT INIT 000   NO-UNDO.
DEF VAR pv-codcia AS INT INIT 000   NO-UNDO.
DEF VAR s-user-id AS CHAR INIT 'ADMIN' NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF almcmov.
DISABLE TRIGGERS FOR LOAD OF almdmov.
DISABLE TRIGGERS FOR LOAD OF almacen.
DISABLE TRIGGERS FOR LOAD OF gn-prov.
DISABLE TRIGGERS FOR LOAD OF almmmate.

DEF VAR x-codpro AS CHAR NO-UNDO.

DEF VAR s-NroSer AS INT INIT 000 NO-UNDO.

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
/* Movimientos de Ingresos al almacén */
DEF TEMP-TABLE W-MoviAlmacen NO-UNDO 
    LIKE OOMoviAlmacen 
    FIELD w-Rowid AS ROWID.

DEF TEMP-TABLE T-MoviAlmacen NO-UNDO 
    LIKE OOMoviAlmacen 
    FIELD t-Rowid AS ROWID.
DEF TEMP-TABLE T-Lg-Cocmp NO-UNDO 
    LIKE Lg-Cocmp. 
DEF TEMP-TABLE T-Lg-Docmp NO-UNDO 
    LIKE Lg-Docmp. 

DEF BUFFER B-MoviAlmacen FOR OOMoviAlmacen.

PUT UNFORMATTED 'Inicio: ' DATETIME(TODAY, MTIME) SKIP.
CICLO:
DO TRANSACTION ON ERROR UNDO, LEAVE ON STOP UNDO, LEAVE:
    /* Primero Cargamos la Información a usar  */
    FOR EACH OOMoviAlmacen WHERE OOMoviAlmacen.codcia = s-codcia
        AND OOMoviAlmacen.FlagMigracion = "N":
        CREATE W-MoviAlmacen.
        BUFFER-COPY OOMoviAlmacen TO W-MoviAlmacen ASSIGN W-MoviAlmacen.w-Rowid = ROWID(OOMoviAlmacen).
    END.
    /* Ingresos-Salidas por Compras */
    FOR EACH W-MoviAlmacen BREAK BY W-MoviAlmacen.CodAlm 
        BY W-MoviAlmacen.TipMov
        BY W-MoviAlmacen.CodMov
        BY W-MoviAlmacen.NroSer 
        BY W-MoviAlmacen.NroDoc:
        IF FIRST-OF(W-MoviAlmacen.CodAlm) 
            OR FIRST-OF(W-MoviAlmacen.TipMov) 
            OR FIRST-OF(W-MoviAlmacen.CodMov)
            OR FIRST-OF(W-MoviAlmacen.NroSer)
            OR FIRST-OF(W-MoviAlmacen.NroDoc)
            THEN DO:
            EMPTY TEMP-TABLE T-MoviAlmacen.
        END.
        CREATE T-MoviAlmacen.
        BUFFER-COPY W-MoviAlmacen TO T-MoviAlmacen
            ASSIGN T-MoviAlmacen.T-Rowid = W-MoviAlmacen.w-Rowid.

        IF LAST-OF(W-MoviAlmacen.CodAlm) 
            OR LAST-OF(W-MoviAlmacen.TipMov) 
            OR LAST-OF(W-MoviAlmacen.CodMov)
            OR LAST-OF(W-MoviAlmacen.NroSer)
            OR LAST-OF(W-MoviAlmacen.NroDoc)
            THEN DO:
            /* MoviAlmacen */
            CASE W-MoviAlmacen.TipMov:
                WHEN "I" THEN RUN Crea-Cabecera-Detalle-Ingresos NO-ERROR.
                WHEN "S" THEN RUN Crea-Cabecera-Detalle-Salidas NO-ERROR.
            END CASE.
            /* Proveedor */
            RUN Crea-Proveedor.
        END.
    END.
    /* Salida por Transferencia */
    EMPTY TEMP-TABLE T-MoviAlmacen.
    FOR EACH W-MoviAlmacen WHERE W-MoviAlmacen.TipMov = "I" 
        AND W-MoviAlmacen.CodAlm <> W-MoviAlmacen.AlmDes
        BREAK BY W-MoviAlmacen.CodAlm 
        BY W-MoviAlmacen.AlmDes
        BY W-MoviAlmacen.TipMov
        BY W-MoviAlmacen.CodMov
        BY W-MoviAlmacen.NroSer 
        BY W-MoviAlmacen.NroDoc:
        IF FIRST-OF(W-MoviAlmacen.CodAlm) 
            OR FIRST-OF(W-MoviAlmacen.AlmDes) 
            OR FIRST-OF(W-MoviAlmacen.TipMov) 
            OR FIRST-OF(W-MoviAlmacen.CodMov)
            OR FIRST-OF(W-MoviAlmacen.NroSer)
            OR FIRST-OF(W-MoviAlmacen.NroDoc)
            THEN DO:
            EMPTY TEMP-TABLE T-MoviAlmacen.
        END.
        CREATE T-MoviAlmacen.
        BUFFER-COPY W-MoviAlmacen TO T-MoviAlmacen
            ASSIGN T-MoviAlmacen.T-Rowid = W-MoviAlmacen.w-Rowid.

        IF LAST-OF(W-MoviAlmacen.CodAlm) 
            OR LAST-OF(W-MoviAlmacen.AlmDes)
            OR LAST-OF(W-MoviAlmacen.TipMov) 
            OR LAST-OF(W-MoviAlmacen.CodMov)
            OR LAST-OF(W-MoviAlmacen.NroSer)
            OR LAST-OF(W-MoviAlmacen.NroDoc)
            THEN DO:
            /* MoviAlmacen */
            RUN Salida-por-Transferencia.
        END.
    END.

    /* Cierre final */
    FOR EACH W-MoviAlmacen:
        FIND B-MoviAlmacen WHERE ROWID(B-MoviAlmacen ) = W-MoviAlmacen.w-rowid
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF ERROR-STATUS:ERROR THEN DO:
            DISPLAY "ERROR:Actualizando Flag (almacen" OOMoviAlmacen.CodAlm "mov" 
                OOMoviAlmacen.TipMov OOMoviAlmacen.CodMov "numero" OOMoviAlmacen.NroSer
                OOMoviAlmacen.NroDoc FORMAT '99999999' "producto" W-MoviAlmacen.CodMat ")"
                WITH STREAM-IO NO-BOX NO-LABELS.
            PAUSE 0.
            UNDO CICLO, LEAVE.
        END.
        PUT UNFORMATTED 'Transferido ' B-MoviAlmacen.NroSer B-MoviAlmacen.NroDoc SKIP.
        ASSIGN
            B-MoviAlmacen.FlagMigracion = "S".
    END.
END.
PUT UNFORMATTED 'Fin: ' DATETIME(TODAY, MTIME) SKIP.
/*QUIT.*/

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
    IF ERROR-STATUS:ERROR AND LOCKED(Almmmate) THEN UNDO, RETURN 'ADM-ERROR'.
    IF NOT AVAILABLE Almmmate THEN DO:
        CREATE Almmmate.
        ASSIGN
            Almmmate.CodCia = S-CODCIA
            Almmmate.CodAlm = C-CODALM
            Almmmate.CodMat = I-CODMAT.
    END.
/*     IF ERROR-STATUS:ERROR THEN UNDO, RETURN 'ADM-ERROR'. */
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
    FIND FIRST Almmmate WHERE Almmmate.CodCia = S-CODCIA AND
          Almmmate.CodAlm = C-CODALM AND 
          Almmmate.CodMat = I-CODMAT EXCLUSIVE-LOCK NO-ERROR.
    IF ERROR-STATUS:ERROR AND LOCKED(Almmmate) THEN UNDO, RETURN 'ADM-ERROR'.
    IF NOT AVAILABLE Almmmate THEN DO:
        CREATE Almmmate.
        ASSIGN
            Almmmate.CodCia = S-CODCIA
            Almmmate.CodAlm = C-CODALM
            Almmmate.CodMat = I-CODMAT.
    END.
/*     IF NOT AVAILABLE Almmmate THEN UNDO, RETURN 'ADM-ERROR'. */
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

&IF DEFINED(EXCLUDE-Crea-Cabecera-Detalle-Ingresos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Crea-Cabecera-Detalle-Ingresos Procedure 
PROCEDURE Crea-Cabecera-Detalle-Ingresos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

CICLO:
DO TRANSACTION ON ERROR UNDO, RETURN ON STOP UNDO, RETURN:
    FIND FIRST Almcmov  WHERE Almcmov.codcia = s-codcia
        AND Almcmov.codalm =  W-MoviAlmacen.CodAlm
        AND Almcmov.tipmov =  W-MoviAlmacen.TipMov
        AND Almcmov.codmov =  W-MoviAlmacen.CodMov
        AND Almcmov.nroser =  W-MoviAlmacen.NroSer
        AND Almcmov.nrodoc =  W-MoviAlmacen.NroDoc
        NO-LOCK NO-ERROR.
    IF AVAILABLE Almcmov THEN DO:
        DISPLAY "ERROR:Cabecera ya registrada (almacen" W-MoviAlmacen.CodAlm "mov" 
            W-MoviAlmacen.TipMov W-MoviAlmacen.CodMov "numero" W-MoviAlmacen.NroSer
            W-MoviAlmacen.NroDoc FORMAT '9999999999' ")"
            WITH STREAM-IO NO-BOX NO-LABELS.
        PAUSE 0.
        /*RETURN.*/
    END.
    ELSE DO:
        CREATE Almcmov.
        BUFFER-COPY W-MoviAlmacen 
            TO Almcmov
            ASSIGN Almcmov.FlgEst = "C".    /* OJO */
    END.
    DETALLE:
    FOR EACH T-MoviAlmacen:
        FIND Almdmov OF Almcmov WHERE Almdmov.CodMat = T-MoviAlmacen.CodMat NO-LOCK NO-ERROR.
        IF AVAILABLE Almdmov THEN DO:
            DISPLAY "ERROR:Detalle ya registrado (almacen" W-MoviAlmacen.CodAlm "mov" 
                W-MoviAlmacen.TipMov W-MoviAlmacen.CodMov "numero" W-MoviAlmacen.NroSer
                W-MoviAlmacen.NroDoc FORMAT '99999999' "producto" T-MoviAlmacen.CodMat ")"
                WITH STREAM-IO NO-BOX NO-LABELS.
            PAUSE 0.
            DELETE T-MoviAlmacen.
            NEXT DETALLE.
        END.
        CREATE Almdmov.
        BUFFER-COPY Almcmov TO Almdmov
            ASSIGN
            Almdmov.CodMat = T-MoviAlmacen.CodMat 
            Almdmov.CanDes = T-MoviAlmacen.CanDes 
            Almdmov.CodUnd = T-MoviAlmacen.CodUnd
            Almdmov.Factor = T-MoviAlmacen.Factor
            Almdmov.PreUni = T-MoviAlmacen.LinPreUni
            Almdmov.HraDoc     = Almcmov.HorRcp
            NO-ERROR.
        IF ERROR-STATUS:ERROR = YES THEN DO:
            DISPLAY "ERROR:Detalle ya registrado (almacen" W-MoviAlmacen.CodAlm "mov" 
                W-MoviAlmacen.TipMov W-MoviAlmacen.CodMov "numero" W-MoviAlmacen.NroSer
                W-MoviAlmacen.NroDoc FORMAT '99999999' "producto" T-MoviAlmacen.CodMat ")"
                WITH STREAM-IO NO-BOX NO-LABELS.
            PAUSE 0.
            UNDO DETALLE, NEXT DETALLE.
        END.
        RUN ALMACSTK (ROWID(Almdmov)).
        IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
            DISPLAY "ERROR:Stock (almacen" W-MoviAlmacen.CodAlm "mov" 
                W-MoviAlmacen.TipMov W-MoviAlmacen.CodMov "numero" W-MoviAlmacen.NroSer
                W-MoviAlmacen.NroDoc FORMAT '99999999' "producto" T-MoviAlmacen.CodMat ")"
                WITH STREAM-IO NO-BOX NO-LABELS.
            PAUSE 0.
            UNDO CICLO, RETURN ERROR.
        END.
        /* RHC 31.03.04 REACTIVAMOS KARDEX POR ALMACEN */
        RUN almacpr1 (ROWID(Almdmov), 'U').
        IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
            DISPLAY "ERROR:Kardex (almacen" W-MoviAlmacen.CodAlm "mov" 
                W-MoviAlmacen.TipMov W-MoviAlmacen.CodMov "numero" W-MoviAlmacen.NroSer
                W-MoviAlmacen.NroDoc FORMAT '99999999' "producto" T-MoviAlmacen.CodMat ")"
                WITH STREAM-IO NO-BOX NO-LABELS.
            PAUSE 0.
            UNDO CICLO, RETURN ERROR.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Crea-Cabecera-Detalle-Salidas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Crea-Cabecera-Detalle-Salidas Procedure 
PROCEDURE Crea-Cabecera-Detalle-Salidas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

CICLO:
DO TRANSACTION ON ERROR UNDO, RETURN ON STOP UNDO, RETURN:
    FIND FIRST Almcmov  WHERE Almcmov.codcia = s-codcia
        AND Almcmov.codalm =  W-MoviAlmacen.CodAlm
        AND Almcmov.tipmov =  W-MoviAlmacen.TipMov
        AND Almcmov.codmov =  W-MoviAlmacen.CodMov
        AND Almcmov.nroser =  W-MoviAlmacen.NroSer
        AND Almcmov.nrodoc =  W-MoviAlmacen.NroDoc
        NO-LOCK NO-ERROR.
    IF AVAILABLE Almcmov THEN DO:
        DISPLAY "ERROR: Cancera ya registrada (almacen" W-MoviAlmacen.CodAlm "mov" 
            W-MoviAlmacen.TipMov W-MoviAlmacen.CodMov "numero" W-MoviAlmacen.NroSer
            W-MoviAlmacen.NroDoc FORMAT '9999999999' ")"
            WITH STREAM-IO NO-BOX NO-LABELS.
        PAUSE 0.
        /*RETURN.*/
    END.
    ELSE DO:
        CREATE Almcmov.
        BUFFER-COPY W-MoviAlmacen TO Almcmov.
    END.
    DETALLE:
    FOR EACH T-MoviAlmacen:
        FIND Almdmov OF Almcmov WHERE Almdmov.CodMat = T-MoviAlmacen.CodMat NO-LOCK NO-ERROR.
        IF AVAILABLE Almdmov THEN DO:
            DISPLAY "ERROR:Detalle registrado (almacen" W-MoviAlmacen.CodAlm "mov" 
                W-MoviAlmacen.TipMov W-MoviAlmacen.CodMov "numero" W-MoviAlmacen.NroSer
                W-MoviAlmacen.NroDoc FORMAT '99999999' "producto" T-MoviAlmacen.CodMat ")"
                WITH STREAM-IO NO-BOX NO-LABELS.
            PAUSE 0.
            DELETE T-MoviAlmacen.
            NEXT DETALLE.
        END.
        CREATE Almdmov.
        BUFFER-COPY Almcmov TO Almdmov
            ASSIGN
            Almdmov.CodMat = T-MoviAlmacen.CodMat 
            Almdmov.CanDes = T-MoviAlmacen.CanDes 
            Almdmov.CodUnd = T-MoviAlmacen.CodUnd
            Almdmov.Factor = T-MoviAlmacen.Factor
            Almdmov.PreUni = T-MoviAlmacen.LinPreUni
            Almdmov.HraDoc = Almcmov.HorRcp
            NO-ERROR.
        RUN almdcstk (ROWID(Almdmov)).
        IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
            DISPLAY "ERROR:Stock (almacen" W-MoviAlmacen.CodAlm "mov" 
                W-MoviAlmacen.TipMov W-MoviAlmacen.CodMov "numero" W-MoviAlmacen.NroSer
                W-MoviAlmacen.NroDoc FORMAT '99999999' "producto" T-MoviAlmacen.CodMat ")"
                WITH STREAM-IO NO-BOX NO-LABELS.
            PAUSE 0.
            UNDO CICLO, RETURN ERROR.
        END.

        RUN ALMACPR1 (ROWID(Almdmov),"U").
        IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
            DISPLAY "ERROR:Stock (almacen" W-MoviAlmacen.CodAlm "mov" 
                W-MoviAlmacen.TipMov W-MoviAlmacen.CodMov "numero" W-MoviAlmacen.NroSer
                W-MoviAlmacen.NroDoc FORMAT '99999999' "producto" T-MoviAlmacen.CodMat ")"
                WITH STREAM-IO NO-BOX NO-LABELS.
            PAUSE 0.
            UNDO CICLO, RETURN ERROR.
        END.
     END.
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Crea-Proveedor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Crea-Proveedor Procedure 
PROCEDURE Crea-Proveedor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN
    x-CodPro = SUBSTRING(OOMoviAlmacen.CodPro,3,8).
IF x-CodPro = '' THEN RETURN.
FIND FIRST gn-prov WHERE gn-prov.codcia = pv-codcia
    AND gn-prov.ruc = OOMoviAlmacen.CodPro
    NO-LOCK NO-ERROR.
IF AVAILABLE gn-prov THEN RETURN.
FIND FIRST gn-prov WHERE gn-prov.codcia = pv-codcia
    AND gn-prov.codpro = x-codpro
    NO-LOCK NO-ERROR.
IF AVAILABLE gn-prov THEN RETURN.
CREATE gn-prov.
ASSIGN
    gn-prov.CodCia = PV-CODCIA
    gn-prov.codpro = x-codpro
    gn-prov.usuario = S-USER-ID
    gn-prov.Libre_c01 = 'No'
    gn-prov.Libre_c02 = 'No'
    gn-prov.Libre_c03 = 'No'
    gn-prov.ruc       = OOMoviAlmacen.CodPro
    gn-prov.nompro    = OOMoviAlmacen.NomRef
    gn-prov.flgsit    = 'A'
    gn-prov.fching    = TODAY.

RELEASE gn-prov.

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

&IF DEFINED(EXCLUDE-Salida-Por-Transferencia) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Salida-Por-Transferencia Procedure 
PROCEDURE Salida-Por-Transferencia :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR s-coddiv AS CHAR NO-UNDO.
DEF VAR x-NroDoc AS INT NO-UNDO.
DEF BUFFER B-Almacen FOR Almacen.
DEF BUFFER CMOV FOR Almcmov.

ASSIGN
    s-NroSer = 000.
FIND Almacen WHERE Almacen.codcia = s-codcia
    AND Almacen.codalm = W-MoviAlmacen.CodAlm
    NO-LOCK.
s-coddiv = Almacen.coddiv.
FIND B-Almacen WHERE B-Almacen.codcia = s-codcia
    AND B-Almacen.codalm = W-MoviAlmacen.AlmDes
    NO-LOCK.
IF Almacen.CodDiv <> B-Almacen.CodDiv THEN DO:
    /* Diferentes locales */
    FIND FIRST FacCorre WHERE FacCorre.CodCia = S-CODCIA 
        AND FacCorre.CodDoc = "G/R"    
        AND FacCorre.CodDiv = S-CODDIV 
        AND FacCorre.CodAlm = W-MoviAlmacen.CodAlm
        AND FacCorre.TipMov = "S"
        AND FacCorre.CodMov = 03
        AND FacCorre.NroSer <> 000
        NO-LOCK NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        DISPLAY "ERROR: G/R x Transferencia Correlativo no Registrado alm:" W-MoviAlmacen.CodAlm 
            WITH STREAM-IO NO-BOX NO-LABELS.
        PAUSE 0.
        RETURN ERROR.
    END.
    s-NroSer = FacCorre.NroSer.
END.
FIND Almtdocm WHERE Almtdocm.CodCia = s-codcia
    AND Almtdocm.CodAlm = W-MoviAlmacen.CodAlm
    AND Almtdocm.TipMov = "S"
    AND Almtdocm.CodMov = 03
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almtdocm THEN DO:
    DISPLAY "ERROR: Salida x Transferencia Movimiento no Registrado alm:" W-MoviAlmacen.CodAlm 
        WITH STREAM-IO NO-BOX NO-LABELS.
    PAUSE 0.
    RETURN ERROR.
END.

CICLO:
DO TRANSACTION ON ERROR UNDO, RETURN ERROR ON STOP UNDO, RETURN ERROR:
    /* Buscamos el correlativo de Guias de Remision */
    FIND FIRST FacCorre WHERE FacCorre.CodCia = S-CODCIA 
        AND FacCorre.CodDoc = "G/R" 
        AND FacCorre.CodDiv = S-CODDIV 
        AND FacCorre.NroSer = S-NROSER EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE Faccorre AND s-NroSer <> 000 THEN DO:
        DISPLAY "ERROR: Salida x Transferencia Correlativo bloqueado alm:" W-MoviAlmacen.CodAlm 
            WITH STREAM-IO NO-BOX NO-LABELS.
        PAUSE 0.
        UNDO, RETURN ERROR.
    END.
    IF AVAILABLE Faccorre AND s-NroSer <> 000 THEN DO:
        ASSIGN
            x-NroDoc = Faccorre.Correlativo.
        REPEAT:
            IF NOT CAN-FIND(FIRST Almcmov WHERE Almcmov.codcia = Almtdocm.CodCia
                          AND Almcmov.CodAlm = Almtdocm.CodAlm 
                          AND Almcmov.TipMov = Almtdocm.TipMov
                          AND Almcmov.CodMov = Almtdocm.CodMov
                          AND Almcmov.NroSer = s-nroser
                          AND Almcmov.NroDoc = x-NroDoc
                          NO-LOCK)
                THEN LEAVE.
            ASSIGN x-NroDoc = x-NroDoc + 1.
        END.
        ASSIGN
            Faccorre.correlativo = x-NroDoc + 1.
    END.
    ELSE DO:
        /* Buscamos el correlativo de almacenes */
        FIND Almacen WHERE Almacen.CodCia = S-CODCIA 
            AND Almacen.CodAlm = Almtdocm.CodAlm 
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE Almacen THEN DO: 
            DISPLAY "ERROR: Salida x Transferencia Correlativo bloqueado alm:" W-MoviAlmacen.CodAlm 
                WITH STREAM-IO NO-BOX NO-LABELS.
            PAUSE 0.
            UNDO, RETURN ERROR.
        END.
        ASSIGN 
            x-Nrodoc  = Almacen.CorrSal.
        REPEAT:
            IF NOT CAN-FIND(FIRST Almcmov WHERE Almcmov.codcia = Almtdocm.CodCia
                          AND Almcmov.CodAlm = Almtdocm.CodAlm 
                          AND Almcmov.TipMov = Almtdocm.TipMov
                          AND Almcmov.CodMov = Almtdocm.CodMov
                          AND Almcmov.NroSer = s-nroser
                          AND Almcmov.NroDoc = x-NroDoc
                          NO-LOCK)
                THEN LEAVE.
            ASSIGN x-NroDoc = x-NroDoc + 1.
        END.
        ASSIGN
            Almacen.CorrSal = x-NroDoc + 1.
    END.
    /* ******************************************** */
    CREATE Almcmov.
    ASSIGN 
      Almcmov.CodCia = Almtdocm.CodCia 
      Almcmov.CodAlm = Almtdocm.CodAlm 
      Almcmov.AlmDes = W-MoviAlmacen.AlmDes
      Almcmov.TipMov = Almtdocm.TipMov
      Almcmov.CodMov = Almtdocm.CodMov
      Almcmov.NroSer = s-nroser
      Almcmov.NroDoc = x-NroDoc
      Almcmov.FlgSit = "T"      /* Transferido pero no Recepcionado */
      Almcmov.FchDoc = TODAY
      Almcmov.HorSal = STRING(TIME,"HH:MM")
      Almcmov.HraDoc = STRING(TIME,"HH:MM")
      /*Almcmov.CodRef = s-CodRef*/
      /*Almcmov.NomRef = F-nomdes*/
      Almcmov.NroRf1 = W-MoviAlmacen.NroRf1 
/*       Almcmov.NroRf2 = Almcmov.NroRf2:SCREEN-VALUE IN FRAME {&FRAME-NAME} */
      Almcmov.NroRf3 = W-MoviAlmacen.NroRf3
      Almcmov.usuario = S-USER-ID.

    RUN Detalle-Salida-por-Transferencia NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO CICLO, RETURN ERROR.

  /* DAMOS MAS VUELTAS EN CASO QUEDEN ITEMS POR GENERAR */
  FIND FIRST T-MoviAlmacen NO-ERROR.
  REPEAT WHILE AVAILABLE T-MoviAlmacen:
      IF AVAILABLE Faccorre AND s-NroSer <> 000 THEN DO:
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
      IF NOT AVAILABLE Almcmov THEN UNDO CICLO, RETURN ERROR.

      RUN Detalle-Salida-por-Transferencia NO-ERROR.
      IF ERROR-STATUS:ERROR THEN UNDO CICLO, RETURN ERROR.

      FIND FIRST T-MoviAlmacen NO-ERROR.
  END.
END.

END PROCEDURE.

PROCEDURE Detalle-Salida-por-Transferencia:

    DEF VAR x-Item AS INT INIT 0 NO-UNDO.
    DEF VAR r-Rowid AS ROWID NO-UNDO.

    FOR EACH T-MoviAlmacen:
        FIND FacCfgGn WHERE Faccfggn.codcia = s-codcia NO-LOCK.
        /* RHC 24/02/2016 Transf. interna sale en un solo documento */
        IF s-nroser <> 000 AND x-Item >= FacCfgGn.Items_Guias THEN LEAVE.
        /* ******************************************************** */
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
               Almdmov.codmat = T-MoviAlmacen.codmat
               Almdmov.CanDes = T-MoviAlmacen.CanDes
               Almdmov.CodUnd = T-MoviAlmacen.CodUnd
               Almdmov.Factor = T-MoviAlmacen.Factor
               Almdmov.ImpCto = T-MoviAlmacen.LinImpLin
               Almdmov.PreUni = T-MoviAlmacen.LinPreUni
               Almdmov.AlmOri = Almcmov.AlmDes 
               Almdmov.CodAjt = ''
               Almdmov.HraDoc = HorSal
               Almdmov.NroItm = x-Item
               R-ROWID = ROWID(Almdmov).
        x-Item = x-Item + 1.
        RUN almdcstk (R-ROWID).
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN ERROR.
        RUN almacpr1 (R-ROWID, "U").
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN ERROR.

        /* Se anulan los items que se pueden descargar */
        DELETE T-MoviAlmacen.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

