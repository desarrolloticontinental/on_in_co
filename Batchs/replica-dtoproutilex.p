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

PUT UNFORMATTED 'INICIO: ' NOW SKIP.
FOR EACH vtatabla NO-LOCK WHERE codcia = 1 
    AND tabla = 'dtoproutilex'
    /*AND (TODAY >= VtaTabla.Rango_fecha[1] AND TODAY <= VtaTabla.Rango_fecha[2])*/
    BY vtatabla.llave_c1 DESC:
    RUN replica.
END.
PUT UNFORMATTED 'FIN: ' NOW SKIP.
QUIT.

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
         HEIGHT             = 4.96
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-replica) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE replica Procedure 
PROCEDURE replica :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    {rpl/reptrig.i
    &Table  = vtatabla
    &Key    =  "string(vtatabla.codcia,'999') + string(vtatabla.tabla,'x(20)') + ~
    string(vtatabla.llave_c1,'x(30)') + string(vtatabla.llave_c2,'x(20)') + ~
        string(vtatabla.llave_c3,'x(20)') + string(vtatabla.llave_c4,'x(20)') + ~
        string(vtatabla.llave_c5,'x(20)')"
    &Prg    = r-vtatabla
    &Event  = WRITE
    &FlgDB0 = TRUE  /* Replicar de la sede remota a la base principal */
    &FlgDB1 = YES    /* Plaza Lima YESrte 00501 */
    &FlgDB2 = YES    /* Surquillo 00023 */
    &FlgDB3 = YES    /* Chorrillos 00027 */
    &FlgDB4 = YES    /* San Borja 00502 */
    &FlgDB5 = YES    /* La Molina 00503 */
    &FlgDB6 = YES    /* Beneficiencia 00504 */
    &FlgDB7 = YES    /* Plaza YESrte 00505 */
    &FlgDB8 = YES    /* La Rambla 00507 */
    &FlgDB9 = YES    /* San Isidro 00508 */
    &FlgDB10 = YES   /* 00065 */
    &FlgDB11 = YES   /* Atocongo 00510 */
    &FlgDB12 = YES   /* Angamos 00511 */
    &FlgDB13 = YES   /* Salaverry 00512 */
    &FlgDB14 = YES   /* Centro Civico 00513 */
    &FlgDB15 = YES   /* Primavera 00514 */
    &FlgDB16 = YES   /* Bellavista 00516 */
    &FlgDB17 = YES
    &FlgDB18 = YES   /* AREQUIPA*/
    &FlgDB19 = YES   /* EXPOLIBRERIA */
    &FlgDB20 = NO    /* SERVIDOR CENTRAL */
    &FlgDB30 = NO   /* SERVIDOR ATE */
    }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

