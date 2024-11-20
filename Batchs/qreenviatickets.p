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
         HEIGHT             = 4.77
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DISABLE TRIGGERS FOR LOAD OF Vtadtickets.

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.

/* Volvemos a reenviar Tickets consumidos */
PUT UNFORMATTED 'Reenviando Tickets' SKIP.
FOR EACH Vtadtickets NO-LOCK WHERE VtaDTickets.CodCia = s-codcia
    AND DATE(VtaDTickets.Fecha) = TODAY:
    PUT UNFORMATTED
        VtaDTickets.CodDiv ' ' 
        VtaDTickets.Fecha ' '
        VtaDTickets.NroTck ' ' 
        VtaDTickets.Valor 
        SKIP.
    RUN replica-vtadtickets.
END.
DISPLAY 'Final:' TODAY STRING(TIME, 'HH:MM:SS') SKIP
    WITH STREAM-IO NO-BOX NO-LABELS.
PAUSE 0.
/* Volvemos a reenviar Tickets aprobados */
/* PUT UNFORMATTED 'Tickets Aprobados' SKIP.                                              */
/* FOR EACH Vtatabla NO-LOCK WHERE Vtatabla.codcia = 1 AND Vtatabla.tabla = 'vutilextck': */
/*     PUT UNFORMATTED                                                                    */
/*         VtaTabla.Llave_c1 ' '                                                          */
/*         VtaTabla.Llave_c2 ' '                                                          */
/*         VtaTabla.LLave_c3 ' '                                                          */
/*         SKIP.                                                                          */
/*     RUN replica-vtatabla.                                                              */
/* END.                                                                                   */

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-replica-vtadtickets) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE replica-vtadtickets Procedure 
PROCEDURE replica-vtadtickets :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{rpl/reptrig.i
    &Table  = vtadtickets
    &Key    = "string(vtadtickets.codcia,'999') + ~
        string(vtadtickets.codpro, 'x(11)') + ~
        string(vtadtickets.producto, 'x(8)') + ~
        string(vtadtickets.nrotck, 'x(12)')"
    &Prg    = r-vtadtickets
    &Event  = WRITE
    &FlgDB0 = TRUE
    &FlgDB1 = YES    /* Plaza Lima Norte 00501 */
    &FlgDB2 = YES    /* Surquillo 00023 */
    &FlgDB3 = YES    /* Chorrillos 00027 */
    &FlgDB4 = YES    /* San Borja 00502 */
    &FlgDB5 = YES    /* La Molina 00503 */
    &FlgDB6 = YES    /* Beneficiencia 00504 */
    &FlgDB7 = YES    /* Feria Plaza Norte 00505 */
    &FlgDB8 = YES    /* La Rambla 00507 */
    &FlgDB9 = YES    /* San Isidro 00508 */
    &FlgDB10 = YES   /* Chiclayo 00065 */
    &FlgDB11 = YES   /* Atocongo 00510 */
    &FlgDB12 = YES   /* Angamos 00511 */
    &FlgDB13 = TRUE
    &FlgDB14 = TRUE
    &FlgDB15 = TRUE
    &FlgDB16 = TRUE
    &FlgDB17 = TRUE
    &FlgDB18 = TRUE   /* Jesus Obrero 00005 */
    &FlgDB19 = NO   /* CONTINENTAL PERU */
    &FlgDB20 = NO   /* SERVIDOR CENTRAL UTILEX */
    &FlgDB30 = NO   /* SERVIDOR CENTRAL UTILEX */
    }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-replica-vtatabla) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE replica-vtatabla Procedure 
PROCEDURE replica-vtatabla :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    {rpl/reptrig.i
    &Table  = vtatabla
    &Key    =  "string(vtatabla.codcia,'999') + string(vtatabla.tabla,'x(20)') + 
    string(vtatabla.llave_c1,'x(20)') + string(vtatabla.llave_c2,'x(20)') + string(vtatabla.llave_c3,'x(20)')"
    &Prg    = r-vtatabla
    &Event  = WRITE
    &FlgDB0 = TRUE
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
    &FlgDB13 = TRUE
    &FlgDB14 = TRUE
    &FlgDB15 = TRUE
    &FlgDB16 = TRUE
    &FlgDB17 = TRUE
    &FlgDB18 = TRUE   /* Jesus Obrero 00005 */
    &FlgDB19 = TRUE   /* CONTINENTAL PERU */
    &FlgDB20 = NO   /* SERVIDOR CENTRAL UTILEX */
    &FlgDB30 = NO   /* SERVIDOR CENTRAL UTILEX */
    }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

