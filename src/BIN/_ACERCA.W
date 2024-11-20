&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
/* Procedure Description
"Acerca de .."
*/
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

&IF "{&NEW}" = "" &THEN 
    DEFINE SHARED VARIABLE s-Aplic-Id AS CHARACTER .
&ELSE
    DEFINE VARIABLE s-Aplic-Id AS CHARACTER INITIAL "@@" .
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-1 RECT-1 Btn_OK PROGRAMAS 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-6 FILL-IN-7 FILL-IN-8 PROGRAMAS ~
FILL-IN-1 FILL-IN-2 FILL-IN-3 FILL-IN-4 FILL-IN-5 FILL-IN-MEMORIA ~
FILL-IN-Recurso 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-PROGRAMAS 
       MENU-ITEM m_Detalle      LABEL "&Detalle"      
              TOGGLE-BOX
       MENU-ITEM m_Resumen      LABEL "&Resumen"      
              TOGGLE-BOX.


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Aceptar" 
     SIZE 10 BY .88
     BGCOLOR 8 .

DEFINE VARIABLE PROGRAMAS AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 49 BY 4.96 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY .69 NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY .69 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY .69 NO-UNDO.

DEFINE VARIABLE FILL-IN-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY .69 NO-UNDO.

DEFINE VARIABLE FILL-IN-5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY .69 NO-UNDO.

DEFINE VARIABLE FILL-IN-6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 49 BY .69 NO-UNDO.

DEFINE VARIABLE FILL-IN-7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 49 BY .69 NO-UNDO.

DEFINE VARIABLE FILL-IN-8 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 49 BY .69 NO-UNDO.

DEFINE VARIABLE FILL-IN-MEMORIA AS INTEGER FORMAT "->,>>>,>>9 KB":U INITIAL 0 
     LABEL "Memoria física existente":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY .69 NO-UNDO.

DEFINE VARIABLE FILL-IN-Recurso AS INTEGER FORMAT ">>9 % disponible":U INITIAL 0 
     LABEL "Recursos del Sistema":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY .69 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "adeicon/blank":U
     SIZE 10.86 BY 2.12.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49 BY .12.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     FILL-IN-6 AT ROW 4.88 COL 2.72 NO-LABEL
     FILL-IN-7 AT ROW 5.5 COL 2.72 NO-LABEL
     FILL-IN-8 AT ROW 6.12 COL 2.72 NO-LABEL
     Btn_OK AT ROW 7.96 COL 5.86
     PROGRAMAS AT ROW 9.31 COL 2.72 NO-LABEL
     FILL-IN-1 AT ROW 1.31 COL 13 COLON-ALIGNED NO-LABEL
     FILL-IN-2 AT ROW 1.92 COL 13 COLON-ALIGNED NO-LABEL
     FILL-IN-3 AT ROW 2.54 COL 13 COLON-ALIGNED NO-LABEL
     FILL-IN-4 AT ROW 3.15 COL 13 COLON-ALIGNED NO-LABEL
     FILL-IN-5 AT ROW 3.81 COL 13 COLON-ALIGNED NO-LABEL
     FILL-IN-MEMORIA AT ROW 7.5 COL 35.72 COLON-ALIGNED
     FILL-IN-Recurso AT ROW 8.23 COL 35.72 COLON-ALIGNED
     IMAGE-1 AT ROW 2 COL 2.72
     RECT-1 AT ROW 7.15 COL 2.72
     SPACE(1.70) SKIP(7.10)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 4
         TITLE "Acerca de .."
         DEFAULT-BUTTON Btn_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
                                                                        */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-1 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-2 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-3 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-4 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-5 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-6 IN FRAME D-Dialog
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-7 IN FRAME D-Dialog
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-8 IN FRAME D-Dialog
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-MEMORIA IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Recurso IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       PROGRAMAS:POPUP-MENU IN FRAME D-Dialog       = MENU POPUP-MENU-PROGRAMAS:HANDLE
       PROGRAMAS:READ-ONLY IN FRAME D-Dialog        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{scr/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Acerca de .. */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Detalle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Detalle D-Dialog
ON VALUE-CHANGED OF MENU-ITEM m_Detalle /* Detalle */
DO:
  RUN Programas-Detalle.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Resumen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Resumen D-Dialog
ON VALUE-CHANGED OF MENU-ITEM m_Resumen /* Resumen */
DO:
  RUN Programas-Resumen.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{scr/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {scr/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {scr/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE data-aplicacion D-Dialog 
PROCEDURE data-aplicacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND FIRST PF-G003 WHERE PF-G003.Aplic-Id = s-Aplic-Id
        NO-LOCK NO-ERROR.
    IF AVAILABLE PF-G003
    THEN DO WITH FRAME {&FRAME-NAME}:
        ASSIGN FILL-IN-1 = integral.PF-G003.Detalle + " de Valmiesa"
               FILL-IN-2 = "Versión " + integral.PF-G003.Version.
        END.
    ELSE DO WITH FRAME {&FRAME-NAME}:
        ASSIGN FILL-IN-1 = "*** Aplicación Desconocida ***"
               FILL-IN-2 = "".
        END.
    FILL-IN-3 = "Sistema Integral Valmiesa Versión 1.1".
    FILL-IN-4 = "Copyright (c) 1996-" + string(year(today)) + " Valmiesa & Asociados.". 
    FILL-IN-5 = "LIMA - PERU".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE data-autorizado D-Dialog 
PROCEDURE data-autorizado :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND FIRST PF-G001 WHERE PF-G001.Aplic-Id = "@@"  /* s-Aplic-Id */
        NO-LOCK NO-ERROR.
    IF AVAILABLE PF-G001
    THEN DO WITH FRAME {&FRAME-NAME}:
        ASSIGN FILL-IN-6 = "Este producto se ha  concedido bajo licencia Nº "+ PF-G001.No-Registro + " a:"
               FILL-IN-7 = PF-G001.Autorizado
               FILL-IN-8 = PF-G001.Direccion.
        END.
    ELSE DO WITH FRAME {&FRAME-NAME}:
        ASSIGN FILL-IN-6 = "*** Registro Invalido ***"
               FILL-IN-7 = "Los derechos de sistema est n siendo violados"
               FILL-IN-8 = "Llamar a VALMIESA".
        END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Data-Recursos D-Dialog 
PROCEDURE Data-Recursos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN GetFreeSpace           (OUTPUT FILL-IN-MEMORIA, INPUT 0).
    RUN GetFreeSystemResources (OUTPUT FILL-IN-Recurso, INPUT 0).

    FILL-IN-MEMORIA = FILL-IN-MEMORIA / 1024.
    Display FILL-IN-MEMORIA FILL-IN-Recurso WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DLL D-Dialog 
PROCEDURE DLL :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
END PROCEDURE.

PROCEDURE GetFreeSpace EXTERNAL "KERNEL.exe":
    DEFINE RETURN PARAMETER mem     AS LONG.
    DEFINE INPUT  PARAMETER flag    AS SHORT.
END.

PROCEDURE GetFreeSystemResources EXTERNAL "USER.exe":
    /*
        flag = 0    SYSTEM RESOURCES
             = 1    USER RESOURCES
             = 2    GDI RESOURCES
    */
    DEFINE RETURN PARAMETER recurso AS SHORT.
    DEFINE INPUT  PARAMETER flag    AS SHORT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-6 FILL-IN-7 FILL-IN-8 PROGRAMAS FILL-IN-1 FILL-IN-2 FILL-IN-3 
          FILL-IN-4 FILL-IN-5 FILL-IN-MEMORIA FILL-IN-Recurso 
      WITH FRAME D-Dialog.
  ENABLE IMAGE-1 RECT-1 Btn_OK PROGRAMAS 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  
  /* Datos acerca de la aplicaci¢n */
  RUN Data-Aplicacion.
  RUN Data-Autorizado.
  RUN Data-Recursos.
  RUN Programas-Resumen.

  IF IMAGE-1:LOAD-IMAGE("img/app-" + s-aplic-id + ".bmp") IN FRAME {&FRAME-NAME}
  THEN.

  IF FRAME {&FRAME-NAME}:LOAD-MOUSE-POINTER("img/valmiesa.cur") 
  THEN.

  IF Btn_OK:LOAD-MOUSE-POINTER("GLOVE") IN FRAME {&FRAME-NAME}
  THEN.



  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Programas-Detalle D-Dialog 
PROCEDURE Programas-Detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE I AS INTEGER INITIAL 1 NO-UNDO.
    PROGRAMAS = "".
    REPEAT:
        IF PROGRAM-NAME(I) = ? THEN LEAVE.
        IF INDEX(PROGRAM-NAME(I), "BIN/_ACERCA") = 0 
        THEN PROGRAMAS = PROGRAMAS + PROGRAM-NAME(I) + CHR(10).
        I = I + 1.
    END.
    MENU-ITEM m_Detalle:CHECKED IN MENU POPUP-MENU-PROGRAMAS = TRUE.
    MENU-ITEM m_Resumen:CHECKED IN MENU POPUP-MENU-PROGRAMAS = FALSE.
    Display PROGRAMAS WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Programas-Resumen D-Dialog 
PROCEDURE Programas-Resumen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE I AS INTEGER INITIAL 1 NO-UNDO.
    PROGRAMAS = "".
    REPEAT:
        IF PROGRAM-NAME(I) = ? THEN LEAVE.
        IF NUM-ENTRIES(PROGRAM-NAME(I), " ") = 1
         AND INDEX(PROGRAM-NAME(I), "BIN/_ACERCA") = 0 
        THEN PROGRAMAS = PROGRAMAS + PROGRAM-NAME(I) + CHR(10).
        IF NUM-ENTRIES(PROGRAM-NAME(I), " ") = 2 
         AND ENTRY(1, PROGRAM-NAME(I), " ") = "EJECUTA"
         AND INDEX(PROGRAM-NAME(I), "BIN/_ACERCA") = 0 
        THEN PROGRAMAS = PROGRAMAS + SUBSTR(PROGRAM-NAME(I), 9) + CHR(10).
        I = I + 1.
    END.
    MENU-ITEM m_Detalle:CHECKED IN MENU POPUP-MENU-PROGRAMAS = FALSE.
    MENU-ITEM m_Resumen:CHECKED IN MENU POPUP-MENU-PROGRAMAS = TRUE.
    Display PROGRAMAS WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


