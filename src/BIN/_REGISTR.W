&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
/* Procedure Description
"Registro de licencia VALMIESA 
para el acceso a sus aplicaciones"
*/
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Frm-Instala
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Frm-Instala 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 11/09/95 - 11:45 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF "{&NEW}" = "" &THEN
    DEFINE INPUT PARAMETER S-TIPO-CONSULTA AS CHARACTER.
&ELSE
    DEFINE VARIABLE S-TIPO-CONSULTA AS CHARACTER INITIAL "NO".
&ENDIF
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE CHK-DATA AS CHARACTER NO-UNDO.
DEFINE VARIABLE ENCRIPTA AS CHARACTER NO-UNDO.
DEFINE VARIABLE TEMP     AS CHARACTER NO-UNDO.
DEFINE VARIABLE I        AS INTEGER   NO-UNDO.
DEFINE VARIABLE X-VTO    AS DATE      NO-UNDO.

IF S-TIPO-CONSULTA = "YES" THEN DO:
    FIND DICTDB.PF-G001 WHERE
        DICTDB.PF-G001.Aplic-Id = "@@" NO-LOCK NO-ERROR.
    IF AVAILABLE DICTDB.PF-G001 THEN DO:
        RUN CHK-DATA.
        CHK-DATA = SUBSTR( CHK-DATA, 1, 15).
        IF CHK-DATA = DICTDB.PF-G001.Verificacion AND NOT CHK-DATA = ? THEN DO:
            IF PF-G001.Vencimiento < TODAY THEN DO:
                BELL.
                MESSAGE "Versión con fecha de vencimiento caducada"
                    VIEW-AS ALERT-BOX ERROR.
            END.
            ELSE RETURN.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Frm-Instala

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES PF-G001

/* Definitions for DIALOG-BOX Frm-Instala                               */
&Scoped-define FIELDS-IN-QUERY-Frm-Instala PF-G001.Autorizado ~
PF-G001.Direccion PF-G001.Vencimiento PF-G001.No-Usuarios PF-G001.Vendido ~
PF-G001.No-Registro 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Frm-Instala PF-G001.Autorizado ~
PF-G001.Direccion PF-G001.Vencimiento PF-G001.No-Usuarios PF-G001.Vendido ~
PF-G001.No-Registro 
&Scoped-define ENABLED-TABLES-IN-QUERY-Frm-Instala PF-G001
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Frm-Instala PF-G001

&Scoped-define FIELD-PAIRS-IN-QUERY-Frm-Instala~
 ~{&FP1}Autorizado ~{&FP2}Autorizado ~{&FP3}~
 ~{&FP1}Direccion ~{&FP2}Direccion ~{&FP3}~
 ~{&FP1}Vencimiento ~{&FP2}Vencimiento ~{&FP3}~
 ~{&FP1}No-Usuarios ~{&FP2}No-Usuarios ~{&FP3}~
 ~{&FP1}Vendido ~{&FP2}Vendido ~{&FP3}~
 ~{&FP1}No-Registro ~{&FP2}No-Registro ~{&FP3}
&Scoped-define OPEN-QUERY-Frm-Instala OPEN QUERY Frm-Instala FOR EACH PF-G001 ~
      WHERE PF-G001.Aplic-Id = "@@" SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Frm-Instala PF-G001
&Scoped-define FIRST-TABLE-IN-QUERY-Frm-Instala PF-G001


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS PF-G001.Autorizado PF-G001.Direccion ~
PF-G001.Vencimiento PF-G001.No-Usuarios PF-G001.Vendido PF-G001.No-Registro 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}Autorizado ~{&FP2}Autorizado ~{&FP3}~
 ~{&FP1}Direccion ~{&FP2}Direccion ~{&FP3}~
 ~{&FP1}Vencimiento ~{&FP2}Vencimiento ~{&FP3}~
 ~{&FP1}No-Usuarios ~{&FP2}No-Usuarios ~{&FP3}~
 ~{&FP1}Vendido ~{&FP2}Vendido ~{&FP3}~
 ~{&FP1}No-Registro ~{&FP2}No-Registro ~{&FP3}
&Scoped-define ENABLED-TABLES PF-G001
&Scoped-define FIRST-ENABLED-TABLE PF-G001
&Scoped-Define ENABLED-OBJECTS R-instala R-instala2 x-CONTROL1 X-CONTROL2 ~
X-CONTROL3 B-aceptar B-Cancelar B-AYUDA 
&Scoped-Define DISPLAYED-FIELDS PF-G001.Autorizado PF-G001.Direccion ~
PF-G001.Vencimiento PF-G001.No-Usuarios PF-G001.Vendido PF-G001.No-Registro 
&Scoped-Define DISPLAYED-OBJECTS x-CONTROL1 X-CONTROL2 X-CONTROL3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-aceptar AUTO-GO 
     IMAGE-UP FILE "img/b-ok":U
     LABEL "&Aceptar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON B-AYUDA 
     IMAGE-UP FILE "img/b-ayuda":U
     LABEL "A&yuda" 
     SIZE 11 BY 1.62.

DEFINE BUTTON B-Cancelar AUTO-END-KEY 
     IMAGE-UP FILE "img/b-cancel":U
     LABEL "&Cancelar" 
     SIZE 11 BY 1.62.

DEFINE VARIABLE x-CONTROL1 AS CHARACTER FORMAT "X(5)":U 
     LABEL "No Control" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE X-CONTROL2 AS CHARACTER FORMAT "X(5)":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE X-CONTROL3 AS CHARACTER FORMAT "X(5)":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE R-instala
     EDGE-PIXELS 6 GRAPHIC-EDGE  NO-FILL 
     SIZE 74.14 BY 9.5.

DEFINE RECTANGLE R-instala2
     EDGE-PIXELS 6 GRAPHIC-EDGE  NO-FILL 
     SIZE 74.14 BY 2.65.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Frm-Instala FOR 
      PF-G001 SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Frm-Instala
     PF-G001.Autorizado AT ROW 1.5 COL 14.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 55 BY .81
          BGCOLOR 15 FGCOLOR 0 
     PF-G001.Direccion AT ROW 3 COL 14.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 55 BY .81
          BGCOLOR 15 FGCOLOR 0 
     PF-G001.Vencimiento AT ROW 4.5 COL 14.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.57 BY .81
          BGCOLOR 15 FGCOLOR 0 
     PF-G001.No-Usuarios AT ROW 4.5 COL 65.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.14 BY .81
          BGCOLOR 15 FGCOLOR 0 
     PF-G001.Vendido AT ROW 6 COL 14.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 55 BY .81
          BGCOLOR 15 FGCOLOR 0 
     PF-G001.No-Registro AT ROW 7.5 COL 14.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 FGCOLOR 0 
     x-CONTROL1 AT ROW 9 COL 14.72 COLON-ALIGNED
     X-CONTROL2 AT ROW 9 COL 22 COLON-ALIGNED NO-LABEL
     X-CONTROL3 AT ROW 9 COL 29.29 COLON-ALIGNED NO-LABEL
     B-aceptar AT ROW 11 COL 12
     B-Cancelar AT ROW 11 COL 33.29
     B-AYUDA AT ROW 11 COL 53.57
     R-instala AT ROW 1 COL 1
     R-instala2 AT ROW 10.5 COL 1
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 4
         TITLE "Registro de Licencia VALMIESA".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Frm-Instala
   Custom                                                               */
ASSIGN 
       FRAME Frm-Instala:SCROLLABLE       = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Frm-Instala
/* Query rebuild information for DIALOG-BOX Frm-Instala
     _TblList          = "integral.PF-G001"
     _Options          = "SHARE-LOCK"
     _Where[1]         = "integral.PF-G001.Aplic-Id = ""@@"""
     _Query            is OPENED
*/  /* DIALOG-BOX Frm-Instala */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Frm-Instala
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Frm-Instala Frm-Instala
ON GO OF FRAME Frm-Instala /* Registro de Licencia VALMIESA */
DO:
    ASSIGN {&FIELDS-IN-QUERY-{&FRAME-NAME}}
           X-CONTROL1
           X-CONTROL2
           X-CONTROL3.
    X-CONTROL1 = X-CONTROL1 + X-CONTROL2 + X-CONTROL3.
    ASSIGN DICTDB.PF-G001.Verificacion = X-CONTROL1.
    RUN CHK-DATA.
    CHK-DATA = SUBSTR( CHK-DATA, 1, 15).
    IF NOT X-CONTROL1 = CHK-DATA THEN DO:
        BELL.
        MESSAGE "Tarjeta de Registro mal Ingresada." VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Frm-Instala Frm-Instala
ON WINDOW-CLOSE OF FRAME Frm-Instala /* Registro de Licencia VALMIESA */
DO:
  IF S-TIPO-CONSULTA = "YES" THEN QUIT.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-aceptar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-aceptar Frm-Instala
ON SHIFT-ALT-F1 OF B-aceptar IN FRAME Frm-Instala /* Aceptar */
DO:
    ASSIGN
        {&FIELDS-IN-QUERY-{&FRAME-NAME}}
        X-CONTROL1
        X-CONTROL2
        X-CONTROL3.
    IF X-CONTROL1 = "80757" AND
        X-CONTROL2 = "@0000" AND
        X-CONTROL3 = "@0000" THEN DO:   
        X-CONTROL1 = X-CONTROL1 + X-CONTROL2 + X-CONTROL3.
        RUN CHK-DATA.
        ASSIGN
            X-CONTROL1 = SUBSTR( CHK-DATA, 1, 5)
            X-CONTROL2 = SUBSTR( CHK-DATA, 6, 5)
            X-CONTROL3 = SUBSTR( CHK-DATA, 11, 5).
        DISPLAY X-CONTROL1 X-CONTROL2 X-CONTROL3 WITH FRAME {&FRAME-NAME}.
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-AYUDA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-AYUDA Frm-Instala
ON CHOOSE OF B-AYUDA IN FRAME Frm-Instala /* Ayuda */
DO:
    BELL.
    MESSAGE
        " Ingrese los datos a partir de la hoja " SKIP
        " de licencia  entregada  por  VALMIESA " SKIP
        " autorizando el uso del sistema en  su " SKIP
        " empresa.                              " SKIP
        " En caso de  no  tener  esta  licencia " SKIP
        " llame a VALMIESA a los telefonos:     " SKIP
        "     (511) 433-2215                    " SKIP
        "     (511) 433-2266                    " SKIP
        "     (511) 433-2305                    " SKIP
        " y contacte con  nuestro  departamento " SKIP
        " de ventas."
        VIEW-AS ALERT-BOX INFORMATION TITLE "Ayuda" .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Cancelar Frm-Instala
ON CHOOSE OF B-Cancelar IN FRAME Frm-Instala /* Cancelar */
DO:
   IF S-TIPO-CONSULTA = "YES" THEN QUIT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME x-CONTROL1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL x-CONTROL1 Frm-Instala
ON ANY-PRINTABLE OF x-CONTROL1 IN FRAME Frm-Instala /* No Control */
DO:
    APPLY CAPS( CHR( LASTKEY) ) TO SELF.
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Frm-Instala 


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
    FIND DICTDB.PF-G001 WHERE DICTDB.PF-G001.Aplic-Id = "@@" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE DICTDB.PF-G001 THEN DO:
        CREATE DICTDB.PF-G001.  
        ASSIGN DICTDB.PF-G001.Aplic-Id = "@@".
    END.
    ASSIGN
        X-CONTROL1 = SUBSTR( DICTDB.PF-G001.Verificacion, 1, 5)
        X-CONTROL2 = SUBSTR( DICTDB.PF-G001.Verificacion, 6, 5)
        X-CONTROL3 = SUBSTR( DICTDB.PF-G001.Verificacion, 11, 5).
    RUN enable_UI.
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CHK-DATA Frm-Instala 
PROCEDURE CHK-DATA :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
    /* Verificando los datos integresados */
    IF DICTDB.PF-G001.Vencimiento = ? THEN X-VTO = DATE(01,01,1996).
    ELSE X-VTO = DICTDB.PF-G001.Vencimiento.

    CHK-DATA = ENCODE( LC(DICTDB.PF-G001.Autorizado) ) +
               ENCODE( LC(DICTDB.PF-G001.Direccion) )  +
               ENCODE( STRING( X-VTO ) )  +
               ENCODE( STRING(DICTDB.PF-G001.No-Usuarios, "9999") )  +
               ENCODE( LC(DICTDB.PF-G001.Vendido) )  +
               ENCODE( LC(DICTDB.PF-G001.Direccion) ).
    CHK-DATA = CAPS( ENCODE( CHK-DATA ) ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Frm-Instala _DEFAULT-DISABLE
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
  HIDE FRAME Frm-Instala.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Frm-Instala _DEFAULT-ENABLE
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

  {&OPEN-QUERY-Frm-Instala}
  GET FIRST Frm-Instala.
  DISPLAY x-CONTROL1 X-CONTROL2 X-CONTROL3 
      WITH FRAME Frm-Instala.
  IF AVAILABLE PF-G001 THEN 
    DISPLAY PF-G001.Autorizado PF-G001.Direccion PF-G001.Vencimiento 
          PF-G001.No-Usuarios PF-G001.Vendido PF-G001.No-Registro 
      WITH FRAME Frm-Instala.
  ENABLE R-instala R-instala2 PF-G001.Autorizado PF-G001.Direccion 
         PF-G001.Vencimiento PF-G001.No-Usuarios PF-G001.Vendido 
         PF-G001.No-Registro x-CONTROL1 X-CONTROL2 X-CONTROL3 B-aceptar 
         B-Cancelar B-AYUDA 
      WITH FRAME Frm-Instala.
  {&OPEN-BROWSERS-IN-QUERY-Frm-Instala}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


