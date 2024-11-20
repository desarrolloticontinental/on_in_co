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
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR x-NroCor AS INT NO-UNDO.
DEF VAR x-OrdMat AS INT NO-UNDO.
DEF VAR s-user-id AS CHAR INIT 'SISTEMAS' NO-UNDO.
DEF VAR c-Alm AS CHAR NO-UNDO.
DEF VAR lCodMat AS CHAR NO-UNDO.

DEF BUFFER MATG FOR Almmmatg.
DEFINE TEMP-TABLE t-Almmmate NO-UNDO LIKE Almmmate.

DISABLE TRIGGERS FOR LOAD OF Almmmatg.
DISABLE TRIGGERS FOR LOAD OF Almmmate.

RUN Carga-Temporal.

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

FOR EACH Almtmatg EXCLUSIVE-LOCK WHERE almtmatg.CodCia = s-codcia AND almtmatg.FlgAut = "X":
    /* Capturamos Correlativo */
    FIND LAST MATG WHERE MATG.CodCia = S-CODCIA NO-LOCK NO-ERROR.
    IF AVAILABLE MATG THEN x-NroCor = INTEGER(MATG.codmat) + 1.
    ELSE x-NroCor = 1.
    FIND LAST MATG WHERE MATG.Codcia = S-CODCIA 
        AND  MATG.CodFam = Almtmatg.Codfam
        USE-INDEX Matg08 NO-LOCK NO-ERROR.
    IF AVAILABLE MATG 
    THEN x-ordmat = MATG.Orden + 3.
    ELSE x-ordmat = 1.
    CREATE Almmmatg.
    BUFFER-COPY Almtmatg 
        TO Almmmatg
        ASSIGN
            Almmmatg.codmat = STRING(x-NroCor,"999999")
            Almmmatg.orden  = x-ordmat
            Almmmatg.ordlis = x-ordmat
            Almmmatg.tpoart = 'A'     /* Activo */
            Almmmatg.FchIng = TODAY
            Almmmatg.FchAct = TODAY
            Almmmatg.Libre_C05 = s-user-id + "|" + STRING(TODAY, '99/99/9999').

    /* Actualizamos las SUBCATEGORIAS */
    FOR EACH webscmatt NO-LOCK WHERE webscmatt.CodCia = s-codcia
        AND webscmatt.codmat = Almtmatg.codmat:
        CREATE webscmatg.
        ASSIGN
            webscmatg.CodCia = webscmatt.CodCia 
            webscmatg.codmat = Almmmatg.codmat
            webscmatg.Subcategoria = webscmatt.Subcategoria.
    END.

    /* Actualizamos la lista de Almacenes */ 
    C-ALM = ''.
    FOR EACH Almacen NO-LOCK WHERE Almacen.CodCia = Almmmatg.codcia AND Almacen.TdoArt:
        /* CONSISTENCIA POR PRODUCTO Y ALMACEN */
        IF Almmmatg.TpoMrg <> '' AND Almacen.Campo-c[2] <> '' THEN DO:
            IF Almmmatg.TpoMrg <> Almacen.Campo-c[2] THEN NEXT.
        END.
        /* *********************************** */
        IF C-ALM = "" THEN C-ALM = TRIM(Almacen.CodAlm).
        IF LOOKUP(TRIM(Almacen.CodAlm),C-ALM) = 0 THEN C-ALM = C-ALM + "," + TRIM(Almacen.CodAlm).
    END.
    ASSIGN 
        Almmmatg.almacenes = C-ALM.

    /* RHC 24-09-2013 ACTUALIZAMOS MONEDA DE VENTA Y TIPO DE CAMBIO */
    FIND Almtfami OF Almmmatg NO-LOCK NO-ERROR.
    IF AVAILABLE Almtfami THEN DO:
       ASSIGN
           Almmmatg.tpocmb = Almtfami.tpocmb.
    END.
    IF NOT (Almmmatg.MonVta = 1 OR Almmmatg.MonVta = 2) THEN Almmmatg.MonVta = 1.
    /* ************************************************************ */

    /* RHC 27.11.09 REPOSICIONES AUTOMATICAS */
    FIND Vtapmatg WHERE Vtapmatg.codcia = s-codcia
        AND Vtapmatg.codmat = 'T' + Almtmatg.codmat
        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE Vtapmatg THEN DO:
        ASSIGN
            Vtapmatg.codmat = Almmmatg.codmat
            NO-ERROR.
    END.

    RUN Trigger-Almmmatg.
    RUN Replica-Almmmatg.

    RUN ACTUALIZA-MAT-x-ALM NO-ERROR.  
    IF ERROR-STATUS:ERROR THEN UNDO, RETURN ERROR.

    /* Ic - 22May2015  -  Minimos */
    lCodMat = "aprobacion" + almmmatg.codmat.
    RUN Procesa-Handle (lCodmat).
    IF ERROR-STATUS:ERROR THEN UNDO, RETURN ERROR.

    /* ACTUALIZA CISSAC */
    RUN Actualiza-Cissac NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, RETURN ERROR.

    /* Actualizamos FLAGS */
    ASSIGN
        Almtmatg.FlgAut = 'A'
        Almtmatg.FchAut = TODAY
        Almtmatg.UsrAut = s-user-id
        Almtmatg.CodNew = Almmmatg.CodMat.
    DISPLAY NOW almtmatg.codmat almtmatg.DesMat WITH STREAM-IO NO-BOX WIDTH 320.
    PAUSE 0.
END.
QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Actualiza-Cissac) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Actualiza-Cissac Procedure 
PROCEDURE Actualiza-Cissac :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN p-catcontiacissac (ROWID(Almmmatg)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ACTUALIZA-MAT-x-ALM) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ACTUALIZA-MAT-x-ALM Procedure 
PROCEDURE ACTUALIZA-MAT-x-ALM :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FOR EACH Almacen NO-LOCK WHERE Almacen.CodCia = s-codcia AND Almacen.TdoArt
        TRANSACTION ON ERROR UNDO, RETURN ERROR ON STOP UNDO, RETURN ERROR:

        /* CONSISTENCIA POR PRODUCTO Y ALMACEN */
        IF Almmmatg.TpoMrg <> '' AND Almacen.Campo-c[2] <> '' THEN DO:
            IF Almmmatg.TpoMrg <> Almacen.Campo-c[2] THEN NEXT.
        END.
        /* *********************************** */
        FIND Almmmate WHERE Almmmate.CodCia = Almacen.codcia AND 
             Almmmate.CodAlm = Almacen.CodAlm AND 
             Almmmate.CodMat = Almmmatg.CodMat NO-ERROR.
        IF NOT AVAILABLE Almmmate THEN DO:
           CREATE Almmmate.
           ASSIGN Almmmate.CodCia = Almmmatg.codcia
                  Almmmate.CodAlm = Almacen.CodAlm
                  Almmmate.CodMat = Almmmatg.CodMat.
        END.
        ASSIGN Almmmate.DesMat = Almmmatg.DesMat
               Almmmate.FacEqu = Almmmatg.FacEqu
               Almmmate.UndVta = Almmmatg.UndStk
               Almmmate.CodMar = Almmmatg.CodMar.
        FIND FIRST almautmv WHERE 
             almautmv.CodCia = Almmmatg.codcia AND
             almautmv.CodFam = Almmmatg.codfam AND
             almautmv.CodMar = Almmmatg.codMar AND
             almautmv.Almsol = Almmmate.CodAlm NO-LOCK NO-ERROR.
        IF AVAILABLE almautmv THEN 
           ASSIGN Almmmate.AlmDes = almautmv.Almdes
                  Almmmate.CodUbi = almautmv.CodUbi.
        /* CODIGO DE UBICACION POR DEFECTO */
        IF Almmmate.codubi = '' THEN Almmmate.CodUbi = 'G-0'.

        RUN Replica-Almmmate.
        
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Carga-Temporal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Temporal Procedure 
PROCEDURE Carga-Temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

EMPTY TEMP-TABLE t-almmmate.
FOR EACH almacen WHERE almacen.codcia = s-codcia AND 
            Almacen.campo-c[9] <> 'i' AND   /* Que no este inactivos */
            Almacen.campo-c[6] = 'si' AND   /* Almacenes comerciales */
            Almacen.Campo-C[3] <> "Si" AND      /* No Remates */
            Almacen.campo-c[2] <> ''  AND    /* Mayorista/Minorista */
            Almacen.AlmCsg = NO :       /* No de Consignación */
    CREATE t-almmmate.
        ASSIGN  t-almmmate.codalm   = almacen.codalm
                t-almmmate.almdes   = almacen.descripcion
                t-almmmate.vctmn1   = 0
                t-almmmate.vctmn2   = 0.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-p-catcontiacissac) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE p-catcontiacissac Procedure 
PROCEDURE p-catcontiacissac :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pRowid AS ROWID.

/* RHC 12/06/2015 */
FIND Almmmatg WHERE ROWID(Almmmatg) = pRowid NO-LOCK.
FIND almmmatgci OF almmmatg NO-LOCK NO-ERROR.
IF NOT AVAILABLE almmmatgci THEN DO:
    CREATE Almmmatgci.
    BUFFER-COPY Almmmatg
        TO AlmmmatgCi
        ASSIGN
        AlmmmatgCi.Libre_d05 = 1.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-p-logmmatg-cissac) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE p-logmmatg-cissac Procedure 
PROCEDURE p-logmmatg-cissac :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF INPUT PARAMETER pRowid AS ROWID.
    DEF INPUT PARAMETER pEvent AS CHAR.
    DEF INPUT PARAMETER pUser AS CHAR.

    RUN p-logmmatg-cissac-a (pRowid, pEvent, pUser) /*NO-ERROR*/ .

    IF CONNECTED("cissac") THEN DISCONNECT cissac NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-p-logmmatg-cissac-a) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE p-logmmatg-cissac-a Procedure 
PROCEDURE p-logmmatg-cissac-a :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF INPUT PARAMETER pRowid AS ROWID.
    DEF INPUT PARAMETER pEvent AS CHAR.
    DEF INPUT PARAMETER pUser AS CHAR.

    /* RHC 12/06/2015 */
    DEF BUFFER B-MATG FOR Almmmatg.
    FIND B-MATG WHERE ROWID(B-MATG) = pRowid NO-LOCK NO-ERROR.
    IF NOT AVAILABLE B-MATG THEN RETURN.
    CREATE LogmmatgCi.
    BUFFER-COPY B-MATG TO LogmmatgCi
        ASSIGN
        LogmmatgCi.LogDate = TODAY
        LogmmatgCi.LogTime = STRING(TIME, 'HH:MM:SS')
        LogmmatgCi.LogUser = pUser
        LogmmatgCi.FlagFechaHora = DATETIME(TODAY, MTIME)
        LogmmatgCi.FlagUsuario = pUser
        LogmmatgCi.flagestado = pEvent.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Procesa-Handle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Procesa-Handle Procedure 
PROCEDURE Procesa-Handle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER L-Handle AS CHAR.

    DEFINE VAR lx-handle AS CHAR.
    DEFINE VAR lx-articulo AS CHAR.

    lx-handle = l-handle.
    IF SUBSTRING(l-handle,1,10) = 'aprobacion'  THEN DO:
        lx-handle = "aprobacion".
        lx-articulo = SUBSTRING(l-handle,11,6).
    END.
    CASE lx-handle:
        WHEN "aprobacion" THEN DO:
            RUN ue-aprobacion (lx-articulo).
        END.

    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Replica-Almmmate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Replica-Almmmate Procedure 
PROCEDURE Replica-Almmmate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    {rpl/reptrig.i
    &Table  = almmmate
    &Key    =  "string(almmmate.codcia,'999') + string(almmmate.codalm,'x(5)') + string(almmmate.codmat,'x(6)')"
    &Prg    = r-almmmate
    &Event  = WRITE
    &FlgDB0 = TRUE
    &FlgDB1 = "NOT LOOKUP(almacen.coddiv, '00501') > 0"  /* TIENDA CONO NORTE */
    &FlgDB2 = "NOT LOOKUP(almacen.coddiv, '00023') > 0"  /* TIENDA SURQUILLO */
    &FlgDB3 = "NOT LOOKUP(almacen.coddiv, '00027') > 0"  /* TIENDA CHORRILLOS */
    &FlgDB4 = "NOT LOOKUP(almacen.coddiv, '00502') > 0"  /* TIENDA SAN BORJA */
    &FlgDB5 = "NOT LOOKUP(almacen.coddiv, '00503') > 0"  /* TIENDA LA MOLINA */
    &FlgDB6 = "NOT LOOKUP(almacen.coddiv, '00504') > 0"  /* TIENDA BENEFICENCIA */
    &FlgDB7 = "NOT LOOKUP(almacen.coddiv, '00505') > 0"  /* TIENDA PLAZA NORTE */
    &FlgDB8 = "NOT LOOKUP(almacen.coddiv, '00507') > 0"  /* TIENDA LA RAMBLA */
    &FlgDB9 = "NOT LOOKUP(almacen.coddiv, '00508') > 0"  /* TIENDA SAN ISIDRO */
    &FlgDB10 = "NOT LOOKUP(almacen.coddiv, '00065') > 0"  /* TIENDA CHICLAYO */
    &FlgDB11 = "NOT LOOKUP(almacen.coddiv, '00510') > 0"  /* TIENDA ATOCONGO */
    &FlgDB12 = "NOT LOOKUP(almacen.coddiv, '00511') > 0"  /* TIENDA ANGAMOS  */
    &FlgDB13 = "NOT LOOKUP(almacen.coddiv, '00512') > 0"  /* TIENDA SALAVERRY  */
    &FlgDB14 = "NOT LOOKUP(almacen.coddiv, '00513') > 0"  /* TIENDA CENTRO CIVICO  */
    &FlgDB15 = "NOT LOOKUP(almacen.coddiv, '00514') > 0"  /* TIENDA PRIMAVERA  */
    &FlgDB16 = "NOT LOOKUP(almacen.coddiv, '00516') > 0"  /* TIENDA BELLAVISTA  */
    &FlgDB17 = TRUE
    &FlgDB18 = "NOT LOOKUP(almacen.coddiv,'00060,00061,00062,00063,10060') > 0"     /* AREQUIPA */
    &FlgDB19 = "NOT LOOKUP(almacen.coddiv, '00069') > 0"  /* TRUJILLO */
    &FlgDB20 = "NOT LOOKUP(almacen.coddiv, '00023,00027,00501,00502,00503,00510') > 0"    /* SERVIDOR UTILEX */
    &FlgDB30 = NO
    }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Replica-Almmmatg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Replica-Almmmatg Procedure 
PROCEDURE Replica-Almmmatg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    {rpl/reptrig.i
    &Table  = almmmatg
    &Key    = "string(almmmatg.codcia,'999') + string(almmmatg.codmat,'x(6)')"
    &Prg    = r-almmmatg
    &Event  = WRITE
    &FlgDB0 = YES          /* Replicas: Sede Remota -> Sede Principal */
    &FlgDB1 = NO    /* Plaza Lima Norte 00501 */
    &FlgDB2 = NO    /* Surquillo 00023 */
    &FlgDB3 = NO    /* Chorrillos 00027 */
    &FlgDB4 = NO    /* San Borja 00502 */
    &FlgDB5 = NO    /* La Molina 00503 */
    &FlgDB6 = NO    /* Beneficiencia 00504 */
    &FlgDB7 = NO    /* Feria Plaza Norte 00505 */
    &FlgDB8 = NO    /* La Rambla 00507 */
    &FlgDB9 = NO    /* San Isidro 00508 */
    &FlgDB10 = NO   /* Chiclayo 00065 */
    &FlgDB11 = NO   /* Atocongo 00510 */
    &FlgDB12 = NO   /* Angamos 00511 */
    &FlgDB13 = NO   /* Salaverry 00512 */
    &FlgDB14 = NO   /* Centro Civico 00513 */
    &FlgDB15 = NO   /* Primavera 00514 */
    &FlgDB16 = NO   /* Bellavista 00516 */
    &FlgDB17 = TRUE
    &FlgDB18 = NO   /* AREQUIPA */
    &FlgDB19 = NO   /* TRUJILLO */
    &FlgDB20 = NO   /* SERVIDOR CENTRAL */
    &FlgDB30 = NO   /* SERVIDOR ATE */
    }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Trigger-Almmmatg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Trigger-Almmmatg Procedure 
PROCEDURE Trigger-Almmmatg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR x-CtoTot AS DEC NO-UNDO.
DEF VAR f-Factor AS DEC NO-UNDO.
DEF VAR pRowid AS ROWID NO-UNDO.

/* PARCHE */
IF Almmmatg.Libre_c04   = '' THEN Almmmatg.Libre_c04   = Almmmatg.CodMat.
IF Almmmatg.CodigoPadre = '' THEN Almmmatg.CodigoPadre = Almmmatg.CodMat.
IF Almmmatg.FactorPadre = 0  THEN Almmmatg.FactorPadre = 1.
IF Almmmatg.MonVta = 0 THEN Almmmatg.MonVta = 1.

/* CONTROL PARA MIGRACION A SPEED */
Almmmatg.Libre_d05 = 1.
/* ****************************** */
/* RHC 30.01.2013 AGREGAMOS LOG DE CONTROL PARA OPENORANGE */
/* CREATE OpenPrecios.                                    */
/* ASSIGN                                                 */
/*     OpenPrecios.CodCia = Almmmatg.codcia               */
/*     OpenPrecios.CodMat = Almmmatg.codmat               */
/*     OpenPrecios.LogDate = TODAY                        */
/*     OpenPrecios.LogTime = STRING(TIME, 'HH:MM:SS')     */
/*     OpenPrecios.LogUser = s-user-id                    */
/*     OpenPrecios.FlagFechaHora = DATETIME(TODAY, MTIME) */
/*     OpenPrecios.FlagUsuario = s-user-id                */
/*     OpenPrecios.flagestado = "I".                      */
/* LOG de control */
CREATE Logmmatg.
BUFFER-COPY Almmmatg TO Logmmatg
    ASSIGN
        Logmmatg.LogDate = TODAY
        Logmmatg.LogTime = STRING(TIME, 'HH:MM:SS')
        Logmmatg.LogUser = s-user-id
        Logmmatg.FlagFechaHora = DATETIME(TODAY, MTIME)
        Logmmatg.FlagUsuario = s-user-id
        Logmmatg.flagestado = "I".
pRowid = ROWID(Almmmatg).
RUN p-logmmatg-cissac (pRowid, "I", s-user-id) NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ue-aprobacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ue-aprobacion Procedure 
PROCEDURE ue-aprobacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER p-Articulo AS CHAR    NO-UNDO.

    DEF VAR EsCampana AS LOG NO-UNDO.

    FIND FIRST almmmatg WHERE almmmatg.codcia = s-codcia 
        AND almmmatg.codmat = p-articulo NO-LOCK NO-ERROR.
    IF AVAILABLE almmmatg THEN DO:
        /* Buscamos si es o no campaña */
        EsCampana = NO.
        rloop:
        FOR EACH VtaTabla WHERE VtaTabla.CodCia = Almmmate.codcia
            AND VtaTabla.Tabla = "CAMPAÑAS" NO-LOCK:
            IF TODAY >= VtaTabla.Rango_fecha[1] AND TODAY <= VtaTabla.Rango_fecha[2] THEN DO:
                EsCampana = YES.
                LEAVE rloop.
            END.
        END.

        FOR EACH t-almmmate WHERE t-almmmate.vctmn1 <> 0 OR t-almmmate.vctmn2 <> 0 :
            FIND Almmmate WHERE Almmmate.CodCia = s-codcia AND 
                 Almmmate.CodAlm = t-almmmate.CodAlm AND 
                 Almmmate.CodMat = Almmmatg.CodMat NO-ERROR.
            IF NOT AVAILABLE Almmmate THEN DO:
               CREATE Almmmate.
               ASSIGN Almmmate.CodCia = Almmmatg.codcia
                      Almmmate.CodAlm = t-almmmate.CodAlm
                      Almmmate.CodMat = Almmmatg.CodMat.
            END.
            ASSIGN Almmmate.DesMat = Almmmatg.DesMat
                   Almmmate.FacEqu = Almmmatg.FacEqu
                   Almmmate.UndVta = Almmmatg.UndStk
                   Almmmate.CodMar = Almmmatg.CodMar
                   almmmate.vctmn1 = t-almmmate.vctmn1
                   almmmate.vctmn2 = t-almmmate.vctmn2 .

            IF EsCampana = NO  AND Almmmate.VCtMn2 > 0 THEN Almmmate.StkMin = Almmmate.VCtMn2.
            IF EsCampana = YES AND Almmmate.VCtMn1 > 0 THEN Almmmate.StkMin = Almmmate.VCtMn1.

            FIND FIRST almautmv WHERE 
                 almautmv.CodCia = Almmmatg.codcia AND
                 almautmv.CodFam = Almmmatg.codfam AND
                 almautmv.CodMar = Almmmatg.codMar AND
                 almautmv.Almsol = t-almmmate.CodAlm NO-LOCK NO-ERROR.
            IF AVAILABLE almautmv THEN 
               ASSIGN Almmmate.AlmDes = almautmv.Almdes
                      Almmmate.CodUbi = almautmv.CodUbi.

            /* CODIGO DE UBICACION POR DEFECTO */
            IF Almmmate.codubi = '' THEN Almmmate.CodUbi = 'G-0'.

            RUN Replica-Almmmate.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

