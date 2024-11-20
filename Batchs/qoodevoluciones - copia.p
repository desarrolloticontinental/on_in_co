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
DISABLE TRIGGERS FOR LOAD OF AlmCrossDocking.
DISABLE TRIGGERS FOR LOAD OF almmmatg.

DEF VAR x-codpro AS CHAR NO-UNDO.

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
DEF TEMP-TABLE T-MoviAlmacen NO-UNDO 
    LIKE OOMoviAlmacen 
    FIELD t-Rowid AS ROWID.

DEF BUFFER B-MoviAlmacen FOR OOMoviAlmacen.

PUT UNFORMATTED 'Inicio: ' DATETIME(TODAY, MTIME) SKIP.
/* RHC 21/05/19 NO pasan los que tienen SLOTING */
FOR EACH OOMoviAlmacen EXCLUSIVE-LOCK WHERE OOMoviAlmacen.codcia = s-codcia
        AND OOMoviAlmacen.FlagMigracion = "N",
    FIRST Almacen NO-LOCK WHERE Almacen.codcia = OOMoviAlmacen.codcia AND
        Almacen.codalm = OOMoviAlmacen.codalm
    BREAK BY OOMoviAlmacen.CodAlm 
        BY OOMoviAlmacen.TipMov
        BY OOMoviAlmacen.CodMov
        BY OOMoviAlmacen.NroSer 
        BY OOMoviAlmacen.NroDoc:
    /* NO Sloting */
    IF ooMoviAlmacen.TipMov = "I" AND ooMoviAlmacen.CodMov = 90 THEN DO:
        FIND FIRST TabGener WHERE TabGener.CodCia = Almacen.CodCia AND 
            TabGener.Codigo = Almacen.CodDiv AND    
            TabGener.Clave = "CFGINC" AND
            TabGener.Libre_l03 = YES         /* Sloting */
            NO-LOCK NO-ERROR.
        IF AVAILABLE TabGener THEN NEXT.
    END.
    /* NO Transferencias */
    IF ooMoviAlmacen.TipMov = "I" AND ooMoviAlmacen.CodMov = 03 THEN DO:
        NEXT.
    END.

    IF FIRST-OF(OOMoviAlmacen.CodAlm) 
        OR FIRST-OF(OOMoviAlmacen.TipMov) 
        OR FIRST-OF(OOMoviAlmacen.CodMov)
        OR FIRST-OF(OOMoviAlmacen.NroSer)
        OR FIRST-OF(OOMoviAlmacen.NroDoc)
        THEN DO:
        EMPTY TEMP-TABLE T-MoviAlmacen.
    END.

    CREATE T-MoviAlmacen.
    BUFFER-COPY OOMoviAlmacen TO T-MoviAlmacen
        ASSIGN T-MoviAlmacen.T-Rowid = ROWID(OOMoviAlmacen).
    IF LAST-OF(OOMoviAlmacen.CodAlm) 
        OR LAST-OF(OOMoviAlmacen.TipMov) 
        OR LAST-OF(OOMoviAlmacen.CodMov)
        OR LAST-OF(OOMoviAlmacen.NroSer)
        OR LAST-OF(OOMoviAlmacen.NroDoc)
        THEN DO:
        /* MoviAlmacen */
        CASE OOMoviAlmacen.TipMov:
            WHEN "I" THEN RUN Crea-Cabecera-Detalle-Ingresos NO-ERROR.
            WHEN "S" THEN RUN Crea-Cabecera-Detalle-Salidas NO-ERROR.
        END CASE.
        /* Proveedor */
        RUN Crea-Proveedor.
    END.
END.
PUT UNFORMATTED 'Fin: ' DATETIME(TODAY, MTIME) SKIP.
QUIT.

/*
FOR EACH OOMoviAlmacen WHERE OOMoviAlmacen.FlagMigracion = "N"
    AND OOMoviAlmacen.TipMov = "I" AND OOMoviAlmacen.CodMov = 09
    BREAK BY OOMoviAlmacen.CodAlm BY OOMoviAlmacen.NroSer BY OOMoviAlmacen.NroDoc:
    IF FIRST-OF(OOMoviAlmacen.CodAlm) OR FIRST-OF(OOMoviAlmacen.NroSer)
        OR FIRST-OF(OOMoviAlmacen.NroDoc)
        THEN EMPTY TEMP-TABLE T-MoviAlmacen.
    CREATE T-MoviAlmacen.
    BUFFER-COPY OOMoviAlmacen TO T-MoviAlmacen
        ASSIGN T-MoviAlmacen.T-Rowid = ROWID(OOMoviAlmacen).

    IF LAST-OF(OOMoviAlmacen.CodAlm) OR LAST-OF(OOMoviAlmacen.NroSer)
        OR LAST-OF(OOMoviAlmacen.NroDoc) THEN DO:
        /* MoviAlmacen */
        RUN Crea-Cabecera-Detalle NO-ERROR.
    END.
END.
*/

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
            FIND CURRENT B-DMOV EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
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

DEFINE VAR x-Contador AS INT NO-UNDO.
RLOOP:
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    /* Ubicamos el detalle a Actualizar */
    FIND AlmDMov WHERE ROWID(AlmDMov) = R-DMov NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almdmov THEN UNDO, RETURN 'ADM-ERROR'.
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
    /* ******************************************************************************** */
    RUN Asigna-Producto (Almdmov.codalm, Almdmov.codmat).
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        UNDO, RETURN 'ADM-ERROR'.
    END.
    /* ******************************************************************************** */
    /* Actualizamos a los Materiales por Almacen */
    {lib/lock-genericov3.i
        &Tabla="Almmmate"
        &Alcance="FIRST"
        &Condicion="Almmmate.CodCia = S-CODCIA AND ~
        Almmmate.CodAlm = C-CODALM AND ~
        Almmmate.CodMat = I-CODMAT"
        &Bloqueo="EXCLUSIVE-LOCK NO-ERROR NO-WAIT"
        &Accion="RETRY"
        &Mensaje="NO"
        &TipoError="UNDO, RETURN 'ADM-ERROR'"
        }
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
    RUN Asigna-Producto (Almdmov.codalm, Almdmov.codmat).
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        UNDO, RETURN 'ADM-ERROR'.
    END.
    /* Des-Actualizamos a los Materiales por Almacen */
    {lib/lock-genericov3.i
        &Tabla="Almmmate"
        &Alcance="FIRST"
        &Condicion="Almmmate.CodCia = S-CODCIA AND ~
        Almmmate.CodAlm = C-CODALM AND ~
        Almmmate.CodMat = I-CODMAT"
        &Bloqueo="EXCLUSIVE-LOCK NO-ERROR NO-WAIT"
        &Accion="RETRY"
        &Mensaje="NO"
        &TipoError="UNDO, RETURN 'ADM-ERROR'"
        }
    Almmmate.StkAct = Almmmate.StkAct - F-CANDES.
END.

RETURN 'OK'.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Asigna-Producto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asigna-Producto Procedure 
PROCEDURE Asigna-Producto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pCodAlm AS CHAR.
DEF INPUT PARAMETER pCodMat AS CHAR.

DEF BUFFER AMATE FOR Almmmate.
DEF BUFFER BMATG FOR Almmmatg.

FIND AMATE WHERE AMATE.CodCia = s-CodCia AND
    AMATE.CodAlm = pCodAlm AND
    AMATE.CodMat = pCodMat NO-LOCK NO-ERROR.
IF AVAILABLE AMATE THEN RETURN 'OK'.     /* Ya Asignado */

FIND BMATG WHERE BMATG.codcia = s-codcia
    AND BMATG.codmat = pCodMat
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE BMATG THEN RETURN 'OK'.
FIND CURRENT BMATG EXCLUSIVE-LOCK NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN 'ADM-ERROR'.

CREATE AMATE.
ASSIGN 
    AMATE.CodCia = s-CodCia
    AMATE.CodAlm = pCodAlm
    AMATE.CodMat = pCodMat
    AMATE.DesMat = BMATG.DesMat
    AMATE.UndVta = BMATG.UndStk
    AMATE.CodMar = BMATG.CodMar
    AMATE.FacEqu = BMATG.FacEqu
    NO-ERROR.
IF ERROR-STATUS:ERROR THEN UNDO, RETURN 'ADM-ERROR'.
FIND FIRST almautmv WHERE almautmv.CodCia = BMATG.codcia AND
    almautmv.CodFam = BMATG.codfam AND
    almautmv.CodMar = BMATG.codMar AND
    almautmv.Almsol = AMATE.CodAlm NO-LOCK NO-ERROR.
IF AVAILABLE almautmv THEN 
    ASSIGN 
        AMATE.AlmDes = almautmv.Almdes
        AMATE.CodUbi = almautmv.CodUbi.
/* Actualizamos la lista de Almacenes */ 
IF TRUE <> (BMATG.almacenes > "") THEN BMATG.Almacenes = TRIM(AMATE.CodAlm).
IF LOOKUP(TRIM(AMATE.CodAlm),BMATG.almacenes) = 0 THEN
    ASSIGN BMATG.almacenes = TRIM(BMATG.almacenes) + "," + TRIM(AMATE.CodAlm).
RELEASE AMATE.
RELEASE BMATG.
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
        AND Almcmov.codalm =  OOMoviAlmacen.CodAlm
        AND Almcmov.tipmov =  OOMoviAlmacen.TipMov
        AND Almcmov.codmov =  OOMoviAlmacen.CodMov
        AND Almcmov.nroser =  OOMoviAlmacen.NroSer
        AND Almcmov.nrodoc =  OOMoviAlmacen.NroDoc
        NO-LOCK NO-ERROR.
    IF AVAILABLE Almcmov THEN DO:
        DISPLAY "ERROR:Cabecera ya registrada (almacen" OOMoviAlmacen.CodAlm "mov" 
            OOMoviAlmacen.TipMov OOMoviAlmacen.CodMov "numero" OOMoviAlmacen.NroSer
            OOMoviAlmacen.NroDoc FORMAT '999999999' ")"
            WITH STREAM-IO NO-BOX NO-LABELS WIDTH 320.
        PAUSE 0.
        RETURN.
    END.
    CREATE Almcmov.
    BUFFER-COPY OOMoviAlmacen 
        TO Almcmov
        ASSIGN Almcmov.FlgEst = "C".    /* OJO */
    /* RHC 01/12/2016 Cross Docking */
    IF Almcmov.codmov = 90 OR Almcmov.codmov = 91 THEN DO:
        FIND AlmCrossDocking WHERE AlmCrossDocking.CodCia = Almcmov.codcia
            AND AlmCrossDocking.CodAlm = Almcmov.codalm
            AND AlmCrossDocking.TipMov = Almcmov.tipmov
            AND AlmCrossDocking.CodMov = Almcmov.codmov
            AND AlmCrossDocking.NroSer = Almcmov.nroser
            AND AlmCrossDocking.NroDoc = Almcmov.nrodoc
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE AlmCrossDocking THEN DO:
            CREATE AlmCrossDocking.
            BUFFER-COPY Almcmov 
                TO AlmCrossDocking
                ASSIGN AlmCrossDocking.FlgEst = "P"     /* OJO */
                NO-ERROR.
        END.
    END.
    DETALLE:
    FOR EACH T-MoviAlmacen:
        FIND Almdmov OF Almcmov WHERE Almdmov.CodMat = T-MoviAlmacen.CodMat NO-LOCK NO-ERROR.
        IF AVAILABLE Almdmov THEN DO:
            DISPLAY "ERROR:Detalle ya registrado (almacen" OOMoviAlmacen.CodAlm "mov" 
                OOMoviAlmacen.TipMov OOMoviAlmacen.CodMov "numero" OOMoviAlmacen.NroSer
                OOMoviAlmacen.NroDoc FORMAT '999999999' "producto" T-MoviAlmacen.CodMat ")"
                WITH STREAM-IO NO-BOX NO-LABELS.
            PAUSE 0.
            UNDO CICLO, RETURN.
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
            DISPLAY "ERROR:Detalle ya registrado (almacen" OOMoviAlmacen.CodAlm "mov" 
                OOMoviAlmacen.TipMov OOMoviAlmacen.CodMov "numero" OOMoviAlmacen.NroSer
                OOMoviAlmacen.NroDoc FORMAT '999999999' "producto" T-MoviAlmacen.CodMat ")"
                WITH STREAM-IO NO-BOX NO-LABELS.
            PAUSE 0.
            UNDO CICLO, RETURN.
        END.
        RUN ALMACSTK (ROWID(Almdmov)).
        IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
            DISPLAY "ERROR:Stock (almacen" OOMoviAlmacen.CodAlm "mov" 
                OOMoviAlmacen.TipMov OOMoviAlmacen.CodMov "numero" OOMoviAlmacen.NroSer
                OOMoviAlmacen.NroDoc FORMAT '99999999' "producto" T-MoviAlmacen.CodMat ")"
                WITH STREAM-IO NO-BOX NO-LABELS.
            PAUSE 0.
            UNDO CICLO, RETURN.
        END.
        /* RHC 31.03.04 REACTIVAMOS KARDEX POR ALMACEN */
        RUN almacpr1 (ROWID(Almdmov), 'U').
        IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
            DISPLAY "ERROR:Kardex (almacen" OOMoviAlmacen.CodAlm "mov" 
                OOMoviAlmacen.TipMov OOMoviAlmacen.CodMov "numero" OOMoviAlmacen.NroSer
                OOMoviAlmacen.NroDoc FORMAT '999999999' "producto" T-MoviAlmacen.CodMat ")"
                WITH STREAM-IO NO-BOX NO-LABELS.
            PAUSE 0.
            UNDO CICLO, RETURN.
        END.
    END.
    /* Transferido */
    FOR EACH T-MoviAlmacen:
        FIND B-MoviAlmacen WHERE ROWID(B-MoviAlmacen ) = T-MoviAlmacen.t-rowid
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF ERROR-STATUS:ERROR THEN DO:
            DISPLAY "ERROR:Actualizando Flag (almacen" OOMoviAlmacen.CodAlm "mov" 
                OOMoviAlmacen.TipMov OOMoviAlmacen.CodMov "numero" OOMoviAlmacen.NroSer
                OOMoviAlmacen.NroDoc FORMAT '999999999' "producto" T-MoviAlmacen.CodMat ")"
                WITH STREAM-IO NO-BOX NO-LABELS.
            PAUSE 0.
            UNDO CICLO, RETURN.
        END.
        PUT UNFORMATTED 'Transferido ' B-MoviAlmacen.NroSer B-MoviAlmacen.NroDoc SKIP.
        ASSIGN
            B-MoviAlmacen.FlagFecha = TODAY
            B-MoviAlmacen.FlagHora = STRING(TIME,'HH:MM:SS')
            B-MoviAlmacen.FlagMigracion = "S".
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
        AND Almcmov.codalm =  OOMoviAlmacen.CodAlm
        AND Almcmov.tipmov =  OOMoviAlmacen.TipMov
        AND Almcmov.codmov =  OOMoviAlmacen.CodMov
        AND Almcmov.nroser =  OOMoviAlmacen.NroSer
        AND Almcmov.nrodoc =  OOMoviAlmacen.NroDoc
        NO-LOCK NO-ERROR.
    IF AVAILABLE Almcmov THEN DO:
        DISPLAY "ERROR: Cancera ya registrada (almacen" OOMoviAlmacen.CodAlm "mov" 
            OOMoviAlmacen.TipMov OOMoviAlmacen.CodMov "numero" OOMoviAlmacen.NroSer
            OOMoviAlmacen.NroDoc FORMAT '999999999' ")"
            WITH STREAM-IO NO-BOX NO-LABELS.
        PAUSE 0.
        RETURN.
    END.
    CREATE Almcmov.
    BUFFER-COPY OOMoviAlmacen TO Almcmov.
    DETALLE:
    FOR EACH T-MoviAlmacen:
        FIND Almdmov OF Almcmov WHERE Almdmov.CodMat = T-MoviAlmacen.CodMat NO-LOCK NO-ERROR.
        IF AVAILABLE Almdmov THEN DO:
            DISPLAY "ERROR:Detalle registrado (almacen" OOMoviAlmacen.CodAlm "mov" 
                OOMoviAlmacen.TipMov OOMoviAlmacen.CodMov "numero" OOMoviAlmacen.NroSer
                OOMoviAlmacen.NroDoc FORMAT '999999999' "producto" T-MoviAlmacen.CodMat ")"
                WITH STREAM-IO NO-BOX NO-LABELS.
            PAUSE 0.
            UNDO CICLO, RETURN.
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
            DISPLAY "ERROR:Stock (almacen" OOMoviAlmacen.CodAlm "mov" 
                OOMoviAlmacen.TipMov OOMoviAlmacen.CodMov "numero" OOMoviAlmacen.NroSer
                OOMoviAlmacen.NroDoc FORMAT '999999999' "producto" T-MoviAlmacen.CodMat ")"
                WITH STREAM-IO NO-BOX NO-LABELS.
            PAUSE 0.
            UNDO CICLO, RETURN.
        END.

        RUN ALMACPR1 (ROWID(Almdmov),"U").
        IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
            DISPLAY "ERROR:Stock (almacen" OOMoviAlmacen.CodAlm "mov" 
                OOMoviAlmacen.TipMov OOMoviAlmacen.CodMov "numero" OOMoviAlmacen.NroSer
                OOMoviAlmacen.NroDoc FORMAT '999999999' "producto" T-MoviAlmacen.CodMat ")"
                WITH STREAM-IO NO-BOX NO-LABELS.
            PAUSE 0.
            UNDO CICLO, RETURN.
        END.
     END.
     /* Transferido */
     FOR EACH T-MoviAlmacen:
         FIND B-MoviAlmacen WHERE ROWID(B-MoviAlmacen ) = T-MoviAlmacen.t-rowid
             EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
         IF ERROR-STATUS:ERROR THEN DO:
             DISPLAY "ERROR:Actualizando Flag (almacen" OOMoviAlmacen.CodAlm "mov" 
                 OOMoviAlmacen.TipMov OOMoviAlmacen.CodMov "numero" OOMoviAlmacen.NroSer
                 OOMoviAlmacen.NroDoc FORMAT '999999999' "producto" T-MoviAlmacen.CodMat ")"
                 WITH STREAM-IO NO-BOX NO-LABELS.
             PAUSE 0.
             UNDO CICLO, RETURN.
         END.
         PUT UNFORMATTED 'Transferido ' B-MoviAlmacen.NroSer B-MoviAlmacen.NroDoc SKIP.
         ASSIGN
             B-MoviAlmacen.FlagFecha = TODAY
             B-MoviAlmacen.FlagHora = STRING(TIME,'HH:MM:SS')
             B-MoviAlmacen.FlagMigracion = "S".
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
    gn-prov.fching    = TODAY
    NO-ERROR.
IF ERROR-STATUS:ERROR THEN UNDO, RETURN.

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

