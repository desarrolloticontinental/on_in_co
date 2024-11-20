&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r11
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*--------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
&IF "{1}" = "Definitions" &THEN
  DEFINE VARIABLE LISTA-MONEDA    AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE MONEDA          AS CHARACTER     NO-UNDO.
  LISTA-MONEDA =  "S/.,US$".
  DEFINE VARIABLE Max-Nro-Monedas AS INTEGER       NO-UNDO.
  Max-Nro-Monedas = NUM-ENTRIES( LISTA-MONEDA).
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
                                                                        */
&ANALYZE-RESUME
 



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

&IF "{1}" = "ANY-PRINTABLE" &THEN
    IF ( LASTKEY - ASC("0") ) > Max-Nro-Monedas THEN RETURN NO-APPLY.
    
    ASSIGN SELF:SCREEN-VALUE = CHR( LASTKEY )
           MONEDA:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = 
                ENTRY(INTEGER(SELF:SCREEN-VALUE), Lista-Moneda ).
    RETURN NO-APPLY.
&ENDIF

&IF "{1}" = "CURSOR-LEFT" &THEN
    IF INTEGER( SELF:SCREEN-VALUE ) <= 1
    THEN ASSIGN SELF:SCREEN-VALUE = STRING( Max-Nro-Monedas, "9").
    ELSE ASSIGN SELF:SCREEN-VALUE = STRING(INTEGER( SELF:SCREEN-VALUE ) - 1).
    
    ASSIGN MONEDA:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = 
                    ENTRY(INTEGER(SELF:SCREEN-VALUE), Lista-Moneda ).

    RETURN NO-APPLY.
&ENDIF

&IF "{1}" = "CURSOR-RIGHT" &THEN   
  IF INTEGER( SELF:SCREEN-VALUE ) >= Max-Nro-Monedas 
  THEN ASSIGN SELF:SCREEN-VALUE = "1".
  ELSE ASSIGN SELF:SCREEN-VALUE = STRING(INTEGER( SELF:SCREEN-VALUE ) + 1).

  ASSIGN MONEDA:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = 
                    ENTRY(INTEGER(SELF:SCREEN-VALUE), Lista-Moneda ).

  RETURN NO-APPLY.
&ENDIF

&IF "{1}" = "LEAVE" &THEN
  MONEDA:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = 
    ENTRY(INTEGER(SELF:SCREEN-VALUE), Lista-Moneda ).
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


