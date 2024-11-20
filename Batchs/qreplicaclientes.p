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
         HEIGHT             = 5.58
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DISABLE TRIGGERS FOR LOAD OF gn-clie.

DEF VAR cl-codcia AS INT INIT 000 NO-UNDO.
DEF VAR DesdeC AS DATE NO-UNDO.
DEF VAR HastaC AS DATE NO-UNDO.

ASSIGN
    DesdeC = TODAY  /*- 1*/
    HastaC = TODAY.

/* 1ro. cargamos productos por almacen */
DISPLAY 'INICIO:' NOW SKIP WITH STREAM-IO NO-BOX.
FOR EACH gn-clie NO-LOCK WHERE gn-clie.codcia = cl-codcia
    AND gn-clie.fching >= DesdeC
    AND gn-clie.fching <= HastaC:
    DISPLAY 'cliente:' gn-clie.CodCli gn-clie.NomCli SKIP WITH STREAM-IO NO-BOX WIDTH 320.
    RUN replica.
END.
DISPLAY '   FIN:' NOW SKIP WITH STREAM-IO NO-BOX.

QUIT.


PROCEDURE replica:
/* ************** */
    {rpl/reptrig.i
        &Table  = gn-clie
        &Key    =  "string(gn-clie.codcia,'999') + string(gn-clie.codcli, 'x(11)')"
        &Prg    = r-gn-clie
        &Event  = WRITE
        &FlgDB0 = YES          /* Replicas: Sede Remota -> Sede Principal */
        &FlgDB1 = YES    /* Plaza Lima Norte 00501 */
        &FlgDB2 = YES    /* Surquillo 00023 */
        &FlgDB3 = YES   /* Chorrillos 00027 */
        &FlgDB4 = YES    /* San Borja 00502 */
        &FlgDB5 = YES    /* La Molina 00503 */
        &FlgDB6 = YES    /* Beneficiencia 00504 */
        &FlgDB7 = YES    /* Feria Plaza Norte 00505 */
        &FlgDB8 = YES    /* La Rambla 00507 */
        &FlgDB9 = YES    /* San Isidro 00508 */
        &FlgDB10 = YES   /* Chiclayo 00065 */
        &FlgDB11 = YES   /* Atocongo 00510 */
        &FlgDB12 = YES   /* Angamos 00511 */
        &FlgDB13 = YES   /* Salaverry 00512 */
        &FlgDB14 = YES   /* Centro Civico 00513 */
        &FlgDB15 = YES   /* Primavera 00514 */
        &FlgDB16 = YES   /* Bellavista 00516 */
        &FlgDB17 = YES
        &FlgDB18 = YES   /* AREQUIPA */
        &FlgDB19 = YES   /* TRUJILLO */
        &FlgDB20 = NO   /* SERVIDOR CENTRAL */
        &FlgDB30 = NO   /* SERVIDOR ATE */
        }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


