&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description : Actualiza los costos de reposcicion y margenes de utilidad

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
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-user-id AS CHAR INIT 'SISTEMAS' NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF Almmmatg.
DISABLE TRIGGERS FOR LOAD OF Almmmate.

/* RUTINA MODIFICADA SOLO PARA REPLICA EL CATALOGO DE MATERIALES */
PUT UNFORMATTED 'INICIO ' STRING(TIME, 'HH:MM:SS') SKIP.
FOR EACH OOAlmmmatg EXCLUSIVE-LOCK WHERE OOAlmmmatg.CodCia = s-codcia 
    AND OOAlmmmatg.FlagMigracion = 'N':
    {lib/lock-genericov21.i ~
        &Tabla="Almmmatg" ~
        &Alcance="FIRST" ~
        &Condicion="Almmmatg.codcia = OOAlmmmatg.codcia ~
        AND Almmmatg.codmat = OOAlmmmatg.codmat" ~
        &Bloqueo="EXCLUSIVE-LOCK NO-WAIT" ~
        &Accion="RETRY" ~
        &Mensaje="NO" ~
        &TipoError="DO: ~
        PUT UNFORMATTED 'Codigo: ' Almmmatg.codmat ' bloqueado por otro usuario' SKIP. ~
        LEAVE. ~
        END" ~
        }
    /* ACTUALIZAMOS OTRAS TABLAS */
    IF OOAlmmmatg.FlagEstado = "CREATE" THEN DO:
        /* RHC 24-09-2013 ACTUALIZAMOS MONEDA DE VENTA Y TIPO DE CAMBIO */
        FIND Almtfami OF Almmmatg NO-LOCK NO-ERROR.
        IF AVAILABLE Almtfami THEN ASSIGN Almmmatg.tpocmb = Almtfami.tpocmb.
        IF NOT (Almmmatg.MonVta = 1 OR Almmmatg.MonVta = 2) THEN Almmmatg.MonVta = 1.
        /* Actualizamos materiales por almacén */
        RUN ACTUALIZA-MAT-x-ALM.  
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, LEAVE.
    END.
    /* MIGRACION A LAS OTRAS TIENDAS */
    RUN Replica-Almmmatg.
    FOR EACH Almmmat1 OF Almmmatg NO-LOCK:
        RUN Replica-Almmmat1.
    END.
    ASSIGN
        OOAlmmmatg.FlagMigracion = "S"
        OOAlmmmatg.MigFecha = TODAY
        OOAlmmmatg.MigHora = STRING(TIME, 'HH:MM:SS')
        OOAlmmmatg.MigUsuario = s-user-id.
    RELEASE Almmmatg.
    RELEASE OOAlmmmatg.
END.
DISPLAY 'Final:' TODAY STRING(TIME, 'HH:MM:SS') SKIP
    WITH STREAM-IO NO-BOX NO-LABELS.
PAUSE 0.
QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ACTUALIZA-MAT-x-ALM) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ACTUALIZA-MAT-x-ALM Procedure 
PROCEDURE ACTUALIZA-MAT-x-ALM :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR LocalCounter AS INTEGER INITIAL 0 NO-UNDO.
FOR EACH Almacen NO-LOCK WHERE Almacen.CodCia = Almmmatg.codcia AND Almacen.TdoArt
    ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    /* CONSISTENCIA POR PRODUCTO Y ALMACEN */
    IF Almmmatg.TpoMrg <> '' AND Almacen.Campo-c[2] <> '' THEN DO:
        IF Almmmatg.TpoMrg <> Almacen.Campo-c[2] THEN NEXT.
    END.
    /* *********************************** */
    LocalCounter = -1.
    REPEAT ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
        LocalCounter = LocalCounter + 1.
        IF LocalCounter >= 5 THEN LEAVE.
        FIND FIRST Almmmate WHERE Almmmate.CodCia = Almmmatg.codcia AND 
             Almmmate.CodAlm = Almacen.CodAlm AND 
             Almmmate.CodMat = Almmmatg.CodMat EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF AVAILABLE Almmmate THEN LEAVE.
        IF NOT AVAILABLE Almmmate THEN DO:
            IF LOCKED(Almmmate) THEN DO:
                PAUSE 2 NO-MESSAGE.
                NEXT.
            END.
            LEAVE.
        END.
    END.
    IF LocalCounter >= 5 THEN RETURN 'ADM-ERROR'.
    IF NOT AVAILABLE Almmmate THEN DO:
       CREATE Almmmate.
       ASSIGN 
           Almmmate.CodCia = Almmmatg.codcia
           Almmmate.CodAlm = Almacen.CodAlm
           Almmmate.CodMat = Almmmatg.CodMat.
    END.
    ASSIGN 
        Almmmate.DesMat = Almmmatg.DesMat
        Almmmate.FacEqu = Almmmatg.FacEqu
        Almmmate.UndVta = Almmmatg.UndStk
        Almmmate.CodMar = Almmmatg.CodMar.
    FIND FIRST almautmv WHERE almautmv.CodCia = Almmmatg.codcia AND 
        almautmv.CodFam = Almmmatg.codfam AND
        almautmv.CodMar = Almmmatg.codMar AND
        almautmv.Almsol = Almmmate.CodAlm NO-LOCK NO-ERROR.
    IF AVAILABLE almautmv THEN 
        ASSIGN 
        Almmmate.AlmDes = almautmv.Almdes
        Almmmate.CodUbi = almautmv.CodUbi.
    RUN Replica-Almmmate.
    RELEASE Almmmate.
END.
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Replica-Almmmat1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Replica-Almmmat1 Procedure 
PROCEDURE Replica-Almmmat1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    {rpl/reptrig.i
    &Table  = almmmat1
    &Key    =  "string(almmmat1.codcia,'999') + string(almmmat1.codmat,'x(6)')"
    &Prg    = r-almmmat1
    &Event  = WRITE
    &FlgDB0 = TRUE  /* Replicar de la sede remota a la base principal */
    &FlgDB1 = NO    /* Plaza Lima Norte 00501 */
    &FlgDB2 = NO    /* Surquillo 00023 */
    &FlgDB3 = NO    /* Chorrillos 00027 */
    &FlgDB4 = NO    /* San Borja 00502 */
    &FlgDB5 = NO    /* La Molina 00503 */
    &FlgDB6 = NO    /* Beneficiencia 00504 */
    &FlgDB7 = NO    /* Plaza Norte 00505 */
    &FlgDB8 = NO    /* La Rambla 00507 */
    &FlgDB9 = NO    /* San Isidro 00508 */
    &FlgDB10 = NO   /* 00065 */
    &FlgDB11 = NO   /* Atocongo 00510 */
    &FlgDB12 = NO   /* Angamos  00511 */
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
    &FlgDB20 = "NOT LOOKUP(almacen.coddiv, '00023,00027,00501,00502,00503,00504,00510') > 0"    /* SERVIDOR UTILEX */
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

