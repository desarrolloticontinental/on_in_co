&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : p-bgrg_r.p
    Purpose     : Avisa que una guia de remision est� para facturar

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DEF VAR s-CodCia AS INT INIT 001 NO-UNDO.
DEF VAR s-CodDiv AS CHAR INIT '00000' NO-UNDO.  /* Santa Raquel */

s-CodCia = INTEGER(ENTRY(1,SESSION:PARAMETER)).
s-CodDiv = ENTRY(2,SESSION:PARAMETER).

DEF TEMP-TABLE T-GUIA LIKE CcbCDocu.
DEF VAR s-CodDoc AS CHAR INIT 'BD' NO-UNDO.

DEF VAR s-Titulo AS CHAR INIT 'CIA. NO IDENTIFICADA' NO-UNDO FORMAT 'x(50)'.

FIND GN-CIAS WHERE GN-CIAS.CodCia = s-CodCia NO-LOCK NO-ERROR.
IF AVAILABLE GN-CIAS THEN s-Titulo = TRIM(GN-CIAS.NomCia) + ': BOLETAS DE DEPOSITO APROBADAS '.

DEF VAR x-Mensaje AS CHAR VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 5.
DEFINE FRAME f-Mensaje
    SKIP(1)
    x-Mensaje AT 5 BGCOLOR 11 FGCOLOR 0 FONT 1 SKIP(1) 
    "Presione F10 para cerrar esta ventana..." FGCOLOR 15 SKIP
    /*"Presione F12 para cerrar el programa...." SKIP*/
    WITH AT COLUMN 50 ROW 1 NO-LABELS OVERLAY
        TITLE s-Titulo
        WIDTH 60
        VIEW-AS DIALOG-BOX BGCOLOR 1.

/* loop buscando informacion nueva */
PRINCIPAL:
REPEAT:
    FOR EACH CcbCDocu WHERE codcia = s-codcia
            AND coddoc = s-coddoc
            AND coddiv = s-coddiv
            AND fchubi = TODAY
            AND flgest = "P"
            AND flgsit = "Autorizada"
            AND LOOKUP(fmapgo, '000,002,403') > 0
            NO-LOCK:
        FIND T-GUIA OF CcbCDocu NO-LOCK NO-ERROR.
        IF NOT AVAILABLE T-GUIA
        THEN DO:
            /* AVISO AL USUARIO */
            ASSIGN
                x-Mensaje = 'BOLETA DE DEPOSITO: ' + ccbcdocu.nrodoc + 
                            ' ' + ccbcdocu.nomcli +
                            ' ' + (IF ccbcdocu.codmon = 1 THEN 'S/.' ELSE 'US$') +
                            TRIM (STRING(ccbcdocu.imptot)) + 
                            ' AUTORIZADA.'.
            DISPLAY x-Mensaje WITH FRAME f-Mensaje.
            READKEY.
            REPEAT WHILE LASTKEY <> KEYCODE("F10"):
                READKEY.
                
                IF LASTKEY = KEYCODE("F12")
                THEN LEAVE PRINCIPAL.
                
            END.
            HIDE FRAME f-Mensaje.
            CREATE T-GUIA.
            RAW-TRANSFER CcbCDocu TO T-GUIA.
        END.
    END.
END.
QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


