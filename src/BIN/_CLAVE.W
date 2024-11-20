&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
/* Procedure Description
"Permite cambiar la clave del usuario"
*/
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME W-INGRESO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-INGRESO 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 07/07/94 -  5:08 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE VECES AS INTEGER INITIAL 0.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME W-INGRESO

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS X-oldpass x-password X-Confirma b-aceptar ~
b-cancelar RECT-1 
&Scoped-Define DISPLAYED-OBJECTS F-NomUser X-oldpass x-password X-Confirma 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON b-aceptar AUTO-GO 
     IMAGE-UP FILE "img/b-ok":U
     LABEL "&Aceptar" 
     SIZE 10.86 BY 1.5.

DEFINE BUTTON b-cancelar AUTO-END-KEY 
     IMAGE-UP FILE "img/b-cancel":U
     LABEL "&Cancelar" 
     SIZE 10.86 BY 1.5.

DEFINE VARIABLE F-NomUser AS CHARACTER FORMAT "x(10)":UC 
     LABEL "Usuario" 
      VIEW-AS TEXT 
     SIZE 22.14 BY .58
     FONT 0 NO-UNDO.

DEFINE VARIABLE X-Confirma AS CHARACTER FORMAT "X(50)":U 
     LABEL "Confirmar Clave" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .81
     BGCOLOR 15 FGCOLOR 0 FONT 0 NO-UNDO.

DEFINE VARIABLE X-oldpass AS CHARACTER FORMAT "X(50)":U 
     LABEL "Clave Actual" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .81
     BGCOLOR 15 FGCOLOR 0 FONT 0 NO-UNDO.

DEFINE VARIABLE x-password AS CHARACTER FORMAT "X(50)":U 
     LABEL "Nueva Clave" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .81
     BGCOLOR 15 FGCOLOR 0 FONT 0 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53 BY 4.65.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME W-INGRESO
     F-NomUser AT ROW 1.58 COL 13.86 COLON-ALIGNED
     X-oldpass AT ROW 2.62 COL 13.86 COLON-ALIGNED PASSWORD-FIELD 
     x-password AT ROW 3.58 COL 13.86 COLON-ALIGNED PASSWORD-FIELD 
     X-Confirma AT ROW 4.5 COL 13.86 COLON-ALIGNED PASSWORD-FIELD 
     b-aceptar AT ROW 1.65 COL 42
     b-cancelar AT ROW 3.23 COL 42
     RECT-1 AT ROW 1 COL 1.29
     SPACE(0.70) SKIP(0.00)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 4
         TITLE "Cambiar Clave de Acceso".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX W-INGRESO
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME W-INGRESO:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN F-NomUser IN FRAME W-INGRESO
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-INGRESO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-INGRESO W-INGRESO
ON GO OF FRAME W-INGRESO /* Cambiar Clave de Acceso */
DO:
   ASSIGN x-Password x-OldPass x-Confirma.

   /* Ic - 17/07/2019, validacion y para que acepte letras y numeros */
   x-Password = TRIM(x-Password).
   x-Confirma = TRIM(x-Confirma).

   IF LENGTH(x-Password) < 5  THEN DO:
       MESSAGE "Su nueva clave debe contener cinco(5) digitos o mas!!!"
            VIEW-AS ALERT-BOX ERROR.
       APPLY "ENTRY" TO X-Password.
       RETURN NO-APPLY.      
   END.

   DEF VAR x-numeros AS CHAR INIT "1234567890" NO-UNDO.
   DEF VAR x-letras AS CHAR INIT "ABCDEFGHIJKLMNÑOPQRSTUVWXYZ" NO-UNDO.
   DEFINE VAR x-contiene-letras AS LOG INIT NO.
   DEFINE VAR x-contiene-numeros AS LOG INIT NO.
   DEFINE VAR x-caracter AS CHAR.
   DEFINE VAR x-pos AS INT.

   /* Contiene numeros */
   DO x-pos = 1 TO LENGTH(x-Password):
        x-Caracter = SUBSTRING(x-Password,x-pos,1).
        IF INDEX(x-numeros, x-Caracter ) > 0 THEN DO:
            x-contiene-numeros = YES.
            LEAVE.
        END.
   END.
   /* Contiene letras */
   DO x-pos = 1 TO LENGTH(x-Password):
        x-Caracter = SUBSTRING(x-Password,x-pos,1).
        IF INDEX(x-letras, x-Caracter ) > 0 THEN DO:
            x-contiene-letras = YES.
            LEAVE.
        END.
   END.

   IF x-contiene-letras <> x-contiene-numeros THEN DO:
       MESSAGE "Su nueva clave debe contener LETRAS y NUMEROS"
            VIEW-AS ALERT-BOX ERROR.
       APPLY "ENTRY" TO X-Password.
       RETURN NO-APPLY.      
   END.


   /* Ic - 17/07/2019 */

   IF x-Confirma <> x-Password 
   THEN DO:
        MESSAGE "Su nueva clave no coincide con la confirmación de la clave"
             VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO X-Password.
        RETURN NO-APPLY.      
   END. 
   FIND integral._USER WHERE integral._USER._USERID = USERID("integral") EXCLUSIVE-LOCK.
   IF NOT _User._Password = ENCODE(x-OldPass)
   THEN DO:
        MESSAGE "La Clave actual no es Correcta - Reingrese" VIEW-AS ALERT-BOX ERROR.
        VECES = VECES + 1.
        IF VECES >= 3 THEN RETURN.
        APPLY "ENTRY" TO X-oldpass.  
        RETURN NO-APPLY.   
    END.
    /* Ticket : 69536  */
    ASSIGN integral._User._Password = ENCODE(x-Password)
        _user._U-misc2[7] = STRING(TODAY,"99/99/9999").
        
    RELEASE _User NO-ERROR.
END.


/*
DEF VAR cValidos AS CHAR INIT "1234567890" NO-UNDO.

DEF VAR iPosicion AS INT NO-UNDO.
DEF VAR cCaracter AS CHAR NO-UNDO.

pError = "".

pNumber = TRIM(pNumber).
pLongitud = LENGTH(pNumber).

IF TRUE <> (pNumber > '') OR pLongitud = 0 THEN DO:
    pError = "Debe ingresar un número válido".
    RETURN.
END.
DO iPosicion = 1 TO pLongitud:
    cCaracter = SUBSTRING(pNumber,iPosicion,1).
    IF INDEX(cValidos, cCaracter ) = 0 THEN DO:
        pError = "Caracter NO válido: " + cCaracter + CHR(10) +
            "Debe ingresar un numérico".
        RETURN.
    END.
END.

*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-INGRESO 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  F-NomUser:SCREEN-VALUE = USERID("integral").
  X-oldpass:SCREEN-VALUE = "". 
  X-Confirma:SCREEN-VALUE = "".
  x-password:SCREEN-VALUE = "".
  /* En OpenEdge ya no es necesario ***
  run SendMessageA (x-password:HWND in frame {&FRAME-NAME}, 204, asc('*'), 0).
  run SendMessageA (x-Confirma:HWND in frame {&FRAME-NAME}, 204, asc('*'), 0).
  run SendMessageA (x-oldpass:HWND in frame {&FRAME-NAME}, 204, asc('*'), 0).
  *** */

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-INGRESO  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME W-INGRESO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DLL W-INGRESO 
PROCEDURE DLL :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

PROCEDURE SendMessageA EXTERNAL "user32.dll":
    DEFINE INPUT PARAMETER hwnd AS LONG.
    DEFINE INPUT PARAMETER umsg AS LONG.
    DEFINE INPUT PARAMETER wparam AS LONG.
    DEFINE INPUT PARAMETER lparam AS LONG.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-INGRESO  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY F-NomUser X-oldpass x-password X-Confirma 
      WITH FRAME W-INGRESO.
  ENABLE X-oldpass x-password X-Confirma b-aceptar b-cancelar RECT-1 
      WITH FRAME W-INGRESO.
  {&OPEN-BROWSERS-IN-QUERY-W-INGRESO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

