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

DEF VAR x-CodFchF AS DATE NO-UNDO.
DEF VAR x-CodFchI AS DATE NO-UNDO.
DEF VAR s-CodCia AS INT INIT 001 NO-UNDO.

ASSIGN
    x-CodFchI = DATE(09,25,2019)
    x-CodFchF = TODAY.

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
         HEIGHT             = 4.04
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DEF BUFFER FERIAS FOR gn-divi.
DEF VAR x-Archivo AS CHAR NO-UNDO.

PUT UNFORMATTED 'INICIO ' NOW SKIP.
SESSION:DATE-FORMAT = "dmy".
x-Archivo = "/home/v/IN/txt/" + "cotizaciones" + STRING(DAY(x-CodFchF),'99') + 
    STRING(MONTH(x-CodFchF),'99') + STRING(YEAR(x-CodFchF),'9999') + ".txt".

OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
/* FOR EACH INTEGRAL.faccpedi NO-LOCK WHERE INTEGRAL.faccpedi.codcia = s-codcia AND */
/*     LOOKUP(INTEGRAL.faccpedi.coddiv, '00015,10060,10065,10067') > 0 AND          */
/*     INTEGRAL.faccpedi.coddoc = 'COT' AND                                         */
/*     INTEGRAL.faccpedi.fchped >= x-CodFchI AND                                    */
/*     INTEGRAL.faccpedi.fchped <= x-CodFchF AND                                    */
/*     LOOKUP(INTEGRAL.faccpedi.libre_c01, '20015,20060,20067,20065') > 0 AND       */
/*     INTEGRAL.faccpedi.flgest <> 'A',                                             */
/*     EACH INTEGRAL.facdpedi OF INTEGRAL.faccpedi NO-LOCK:                         */
/*     EXPORT DELIMITER "~029"                                                      */
/*         INTEGRAL.faccpedi.coddiv                                                 */
/*         INTEGRAL.faccpedi.nroped                                                 */
/*         INTEGRAL.faccpedi.fchped                                                 */
/*         INTEGRAL.faccpedi.fchent                                                 */
/*         INTEGRAL.faccpedi.codven                                                 */
/*         INTEGRAL.faccpedi.codcli                                                 */
/*         INTEGRAL.facdpedi.codmat                                                 */
/*         INTEGRAL.facdpedi.undvta                                                 */
/*         INTEGRAL.facdpedi.canped                                                 */
/*         INTEGRAL.facdpedi.implin                                                 */
/*         INTEGRAL.facdpedi.canate                                                 */
/*         INTEGRAL.faccpedi.libre_c01.                                             */
/* END.                                                                             */
FOR EACH FERIAS NO-LOCK WHERE FERIAS.codcia = s-codcia,     /* Todas las divisiones */
    EACH INTEGRAL.faccpedi NO-LOCK WHERE INTEGRAL.faccpedi.codcia = s-codcia AND
    INTEGRAL.faccpedi.coddiv = FERIAS.coddiv AND
    INTEGRAL.faccpedi.coddoc = 'COT' AND
    INTEGRAL.faccpedi.fchped >= x-CodFchI AND
    INTEGRAL.faccpedi.fchped <= x-CodFchF, /* AND
    INTEGRAL.faccpedi.flgest <> 'A',*/
    FIRST gn-divi NO-LOCK WHERE gn-divi.codcia = INTEGRAL.faccpedi.codcia
    AND gn-divi.coddiv = INTEGRAL.faccpedi.libre_c01
    AND gn-divi.canalventa = 'FER',
    EACH INTEGRAL.facdpedi OF INTEGRAL.faccpedi NO-LOCK:
    EXPORT DELIMITER "~029"
        INTEGRAL.faccpedi.coddiv
        INTEGRAL.faccpedi.nroped
        INTEGRAL.faccpedi.fchped
        INTEGRAL.faccpedi.fchent
        INTEGRAL.faccpedi.codven
        INTEGRAL.faccpedi.codcli
        INTEGRAL.facdpedi.codmat
        INTEGRAL.facdpedi.undvta
        INTEGRAL.facdpedi.canped
        INTEGRAL.facdpedi.implin
        INTEGRAL.facdpedi.canate
        INTEGRAL.faccpedi.libre_c01
        INTEGRAL.faccpedi.flgest.

END.
OUTPUT CLOSE.

DEFINE VARIABLE comm-line AS CHARACTER FORMAT "x(70)".                                        
comm-line = "/usr/bin/qoncotexport2".
OS-COMMAND VALUE(comm-line) SILENT NO-CONSOLE.
PUT UNFORMATTED 'PROCESO TERMINADO ' NOW SKIP.

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


