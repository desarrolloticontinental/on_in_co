&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File:

  Description: from VIEWER.W - Template for SmartViewer Objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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

DEFINE SHARED VAR S-CODCIA AS INTEGER.
DEFINE VAR    I AS INTEGER NO-UNDO.

DEFINE VAR F-PRECIO AS DEC NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES Almmmatg VtaListaMinGn
&Scoped-define FIRST-EXTERNAL-TABLE Almmmatg


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR Almmmatg, VtaListaMinGn.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS VtaListaMinGn.DtoVolR[1] ~
VtaListaMinGn.DtoVolD[1] VtaListaMinGn.DtoVolP[1] VtaListaMinGn.DtoVolR[2] ~
VtaListaMinGn.DtoVolD[2] VtaListaMinGn.DtoVolP[2] VtaListaMinGn.DtoVolR[3] ~
VtaListaMinGn.DtoVolD[3] VtaListaMinGn.DtoVolP[3] VtaListaMinGn.DtoVolR[4] ~
VtaListaMinGn.DtoVolD[4] VtaListaMinGn.DtoVolP[4] VtaListaMinGn.DtoVolR[5] ~
VtaListaMinGn.DtoVolD[5] VtaListaMinGn.DtoVolP[5] VtaListaMinGn.DtoVolR[6] ~
VtaListaMinGn.DtoVolD[6] VtaListaMinGn.DtoVolP[6] VtaListaMinGn.DtoVolR[7] ~
VtaListaMinGn.DtoVolD[7] VtaListaMinGn.DtoVolP[7] VtaListaMinGn.DtoVolR[8] ~
VtaListaMinGn.DtoVolD[8] VtaListaMinGn.DtoVolP[8] VtaListaMinGn.DtoVolR[9] ~
VtaListaMinGn.DtoVolD[9] VtaListaMinGn.DtoVolP[9] VtaListaMinGn.DtoVolR[10] ~
VtaListaMinGn.DtoVolD[10] VtaListaMinGn.DtoVolP[10] 
&Scoped-define ENABLED-TABLES VtaListaMinGn
&Scoped-define FIRST-ENABLED-TABLE VtaListaMinGn
&Scoped-Define ENABLED-OBJECTS RECT-10 RECT-13 
&Scoped-Define DISPLAYED-FIELDS VtaListaMinGn.codmat Almmmatg.UndBas ~
Almmmatg.DesMat VtaListaMinGn.DtoVolR[1] VtaListaMinGn.DtoVolD[1] ~
VtaListaMinGn.DtoVolP[1] VtaListaMinGn.DtoVolR[2] VtaListaMinGn.DtoVolD[2] ~
VtaListaMinGn.DtoVolP[2] VtaListaMinGn.DtoVolR[3] VtaListaMinGn.DtoVolD[3] ~
VtaListaMinGn.DtoVolP[3] VtaListaMinGn.DtoVolR[4] VtaListaMinGn.DtoVolD[4] ~
VtaListaMinGn.DtoVolP[4] VtaListaMinGn.DtoVolR[5] VtaListaMinGn.DtoVolD[5] ~
VtaListaMinGn.DtoVolP[5] VtaListaMinGn.DtoVolR[6] VtaListaMinGn.DtoVolD[6] ~
VtaListaMinGn.DtoVolP[6] VtaListaMinGn.DtoVolR[7] VtaListaMinGn.DtoVolD[7] ~
VtaListaMinGn.DtoVolP[7] VtaListaMinGn.DtoVolR[8] VtaListaMinGn.DtoVolD[8] ~
VtaListaMinGn.DtoVolP[8] VtaListaMinGn.DtoVolR[9] VtaListaMinGn.DtoVolD[9] ~
VtaListaMinGn.DtoVolP[9] VtaListaMinGn.DtoVolR[10] ~
VtaListaMinGn.DtoVolD[10] VtaListaMinGn.DtoVolP[10] 
&Scoped-define DISPLAYED-TABLES VtaListaMinGn Almmmatg
&Scoped-define FIRST-DISPLAYED-TABLE VtaListaMinGn
&Scoped-define SECOND-DISPLAYED-TABLE Almmmatg


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40.86 BY .85.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY 9.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     VtaListaMinGn.codmat AT ROW 1.46 COL 1.14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8.29 BY .69
          BGCOLOR 15 FGCOLOR 1 
     Almmmatg.UndBas AT ROW 1.46 COL 18.14 COLON-ALIGNED NO-LABEL FORMAT "X(8)"
          VIEW-AS FILL-IN 
          SIZE 7 BY .69
          BGCOLOR 15 FGCOLOR 1 
     Almmmatg.DesMat AT ROW 2.31 COL 1.14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 40.57 BY .69
          BGCOLOR 15 FGCOLOR 1 
     VtaListaMinGn.DtoVolR[1] AT ROW 4.19 COL 3.14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
     VtaListaMinGn.DtoVolD[1] AT ROW 4.19 COL 17.72 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.29 BY .81
     VtaListaMinGn.DtoVolP[1] AT ROW 4.19 COL 28 COLON-ALIGNED NO-LABEL WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
     VtaListaMinGn.DtoVolR[2] AT ROW 5.04 COL 3.14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
     VtaListaMinGn.DtoVolD[2] AT ROW 5.04 COL 17.72 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.29 BY .81
     VtaListaMinGn.DtoVolP[2] AT ROW 5.04 COL 28 COLON-ALIGNED NO-LABEL WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
     VtaListaMinGn.DtoVolR[3] AT ROW 5.85 COL 3.14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
     VtaListaMinGn.DtoVolD[3] AT ROW 5.85 COL 17.72 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.29 BY .81
     VtaListaMinGn.DtoVolP[3] AT ROW 5.85 COL 28 COLON-ALIGNED NO-LABEL WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
     VtaListaMinGn.DtoVolR[4] AT ROW 6.65 COL 3.14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
     VtaListaMinGn.DtoVolD[4] AT ROW 6.65 COL 17.72 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.29 BY .81
     VtaListaMinGn.DtoVolP[4] AT ROW 6.65 COL 28 COLON-ALIGNED NO-LABEL WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
     VtaListaMinGn.DtoVolR[5] AT ROW 7.46 COL 3.14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
     VtaListaMinGn.DtoVolD[5] AT ROW 7.46 COL 17.72 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.29 BY .81
     VtaListaMinGn.DtoVolP[5] AT ROW 7.46 COL 28 COLON-ALIGNED NO-LABEL WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
     VtaListaMinGn.DtoVolR[6] AT ROW 8.27 COL 3.14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
     VtaListaMinGn.DtoVolD[6] AT ROW 8.27 COL 17.72 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.29 BY .81
     VtaListaMinGn.DtoVolP[6] AT ROW 8.27 COL 28 COLON-ALIGNED NO-LABEL WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
     VtaListaMinGn.DtoVolR[7] AT ROW 9.08 COL 3.14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
     VtaListaMinGn.DtoVolD[7] AT ROW 9.08 COL 17.72 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.29 BY .81
     VtaListaMinGn.DtoVolP[7] AT ROW 9.08 COL 28 COLON-ALIGNED NO-LABEL WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     VtaListaMinGn.DtoVolR[8] AT ROW 9.88 COL 3.14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
     VtaListaMinGn.DtoVolD[8] AT ROW 9.88 COL 17.72 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.29 BY .81
     VtaListaMinGn.DtoVolP[8] AT ROW 9.88 COL 28 COLON-ALIGNED NO-LABEL WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
     VtaListaMinGn.DtoVolR[9] AT ROW 10.69 COL 3.14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
     VtaListaMinGn.DtoVolD[9] AT ROW 10.69 COL 17.72 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.29 BY .81
     VtaListaMinGn.DtoVolP[9] AT ROW 10.69 COL 28 COLON-ALIGNED NO-LABEL WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
     VtaListaMinGn.DtoVolR[10] AT ROW 11.58 COL 3.14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
     VtaListaMinGn.DtoVolD[10] AT ROW 11.58 COL 17.72 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.29 BY .81
     VtaListaMinGn.DtoVolP[10] AT ROW 11.58 COL 28 COLON-ALIGNED NO-LABEL WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
     "Cantidad Minima" VIEW-AS TEXT
          SIZE 11.14 BY .5 AT ROW 3.27 COL 5.14
     "Precio" VIEW-AS TEXT
          SIZE 8 BY .5 AT ROW 3.27 COL 31.29
     "Descuento %" VIEW-AS TEXT
          SIZE 10.86 BY .5 AT ROW 3.27 COL 18.57
     RECT-10 AT ROW 3.12 COL 2.72
     RECT-13 AT ROW 3.15 COL 2.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: integral.Almmmatg,INTEGRAL.VtaListaMinGn
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 12.81
         WIDTH              = 50.86.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm-vm/method/vmviewer.i}
{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit L-To-R                            */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN VtaListaMinGn.codmat IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Almmmatg.DesMat IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Almmmatg.UndBas IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME VtaListaMinGn.DtoVolD[10]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolD[10] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolD[10] IN FRAME F-Main /* DtoVolD */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(VtaListaMinGn.DtoVolR[10]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) = 0 THEN RETURN.
      IF INTEGER(VtaListaMinGn.DtoVolR[10]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) <> 0 THEN DO:
          MESSAGE "Descuento Solo se aplica Para Rangos Mayores a Cero " SKIP
              "Verifique Por Favor ....."  VIEW-AS ALERT-BOX ERROR.
          SELF:SCREEN-VALUE = '0.0000'.
          RETURN NO-APPLY.
      END.
      VtaListaMinGn.DtoVolP[10]:SCREEN-VALUE = STRING( ROUND(F-PRECIO * ( 1 - ( DECI(SELF:SCREEN-VALUE) / 100 ) ),4) ).
      IF DECIMAL(SELF:SCREEN-VALUE) = 0 THEN VtaListaMinGn.DtoVolP[10]:SCREEN-VALUE = '0.0000'.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMinGn.DtoVolD[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolD[1] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolD[1] IN FRAME F-Main /* DtoVolD */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(VtaListaMinGn.DtoVolR[1]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) = 0 THEN RETURN.
      IF INTEGER(VtaListaMinGn.DtoVolR[1]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) <> 0 THEN DO:
          MESSAGE "Descuento Solo se aplica Para Rangos Mayores a Cero " SKIP
              "Verifique Por Favor ....."  VIEW-AS ALERT-BOX ERROR.
          SELF:SCREEN-VALUE = '0.0000'.
          RETURN NO-APPLY.
      END.
      VtaListaMinGn.DtoVolP[1]:SCREEN-VALUE = STRING( ROUND(F-PRECIO * ( 1 - ( DECI(SELF:SCREEN-VALUE) / 100 ) ),4) ).
      IF DECIMAL(SELF:SCREEN-VALUE) = 0 THEN VtaListaMinGn.DtoVolP[1]:SCREEN-VALUE = '0.0000'.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMinGn.DtoVolD[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolD[2] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolD[2] IN FRAME F-Main /* DtoVolD */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(VtaListaMinGn.DtoVolR[2]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) = 0 THEN RETURN.
      IF INTEGER(VtaListaMinGn.DtoVolR[2]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) <> 0 THEN DO:
          MESSAGE "Descuento Solo se aplica Para Rangos Mayores a Cero " SKIP
              "Verifique Por Favor ....."  VIEW-AS ALERT-BOX ERROR.
          SELF:SCREEN-VALUE = '0.0000'.
          RETURN NO-APPLY.
      END.
      VtaListaMinGn.DtoVolP[2]:SCREEN-VALUE = STRING( ROUND(F-PRECIO * ( 1 - ( DECI(SELF:SCREEN-VALUE) / 100 ) ),4) ).
      IF DECIMAL(SELF:SCREEN-VALUE) = 0 THEN VtaListaMinGn.DtoVolP[2]:SCREEN-VALUE = '0.0000'.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMinGn.DtoVolD[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolD[3] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolD[3] IN FRAME F-Main /* DtoVolD */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(VtaListaMinGn.DtoVolR[3]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) = 0 THEN RETURN.
      IF INTEGER(VtaListaMinGn.DtoVolR[3]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) <> 0 THEN DO:
          MESSAGE "Descuento Solo se aplica Para Rangos Mayores a Cero " SKIP
              "Verifique Por Favor ....."  VIEW-AS ALERT-BOX ERROR.
          SELF:SCREEN-VALUE = '0.0000'.
          RETURN NO-APPLY.
      END.
      VtaListaMinGn.DtoVolP[3]:SCREEN-VALUE = STRING( ROUND(F-PRECIO * ( 1 - ( DECI(SELF:SCREEN-VALUE) / 100 ) ),4) ).
      IF DECIMAL(SELF:SCREEN-VALUE) = 0 THEN VtaListaMinGn.DtoVolP[3]:SCREEN-VALUE = '0.0000'.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMinGn.DtoVolD[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolD[4] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolD[4] IN FRAME F-Main /* DtoVolD */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(VtaListaMinGn.DtoVolR[4]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) = 0 THEN RETURN.
      IF INTEGER(VtaListaMinGn.DtoVolR[4]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) <> 0 THEN DO:
          MESSAGE "Descuento Solo se aplica Para Rangos Mayores a Cero " SKIP
              "Verifique Por Favor ....."  VIEW-AS ALERT-BOX ERROR.
          SELF:SCREEN-VALUE = '0.0000'.
          RETURN NO-APPLY.
      END.
      VtaListaMinGn.DtoVolP[4]:SCREEN-VALUE = STRING( ROUND(F-PRECIO * ( 1 - ( DECI(SELF:SCREEN-VALUE) / 100 ) ),4) ).
      IF DECIMAL(SELF:SCREEN-VALUE) = 0 THEN VtaListaMinGn.DtoVolP[4]:SCREEN-VALUE = '0.0000'.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMinGn.DtoVolD[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolD[5] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolD[5] IN FRAME F-Main /* DtoVolD */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(VtaListaMinGn.DtoVolR[5]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) = 0 THEN RETURN.
      IF INTEGER(VtaListaMinGn.DtoVolR[5]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) <> 0 THEN DO:
          MESSAGE "Descuento Solo se aplica Para Rangos Mayores a Cero " SKIP
              "Verifique Por Favor ....."  VIEW-AS ALERT-BOX ERROR.
          SELF:SCREEN-VALUE = '0.0000'.
          RETURN NO-APPLY.
      END.
      VtaListaMinGn.DtoVolP[5]:SCREEN-VALUE = STRING( ROUND(F-PRECIO * ( 1 - ( DECI(SELF:SCREEN-VALUE) / 100 ) ),4) ).
      IF DECIMAL(SELF:SCREEN-VALUE) = 0 THEN VtaListaMinGn.DtoVolP[5]:SCREEN-VALUE = '0.0000'.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMinGn.DtoVolD[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolD[6] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolD[6] IN FRAME F-Main /* DtoVolD */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(VtaListaMinGn.DtoVolR[6]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) = 0 THEN RETURN.
      IF INTEGER(VtaListaMinGn.DtoVolR[6]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) <> 0 THEN DO:
          MESSAGE "Descuento Solo se aplica Para Rangos Mayores a Cero " SKIP
              "Verifique Por Favor ....."  VIEW-AS ALERT-BOX ERROR.
          SELF:SCREEN-VALUE = '0.0000'.
          RETURN NO-APPLY.
      END.
      VtaListaMinGn.DtoVolP[6]:SCREEN-VALUE = STRING( ROUND(F-PRECIO * ( 1 - ( DECI(SELF:SCREEN-VALUE) / 100 ) ),4) ).
      IF DECIMAL(SELF:SCREEN-VALUE) = 0 THEN VtaListaMinGn.DtoVolP[6]:SCREEN-VALUE = '0.0000'.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMinGn.DtoVolD[7]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolD[7] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolD[7] IN FRAME F-Main /* DtoVolD */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(VtaListaMinGn.DtoVolR[7]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) = 0 THEN RETURN.
      IF INTEGER(VtaListaMinGn.DtoVolR[7]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) <> 0 THEN DO:
          MESSAGE "Descuento Solo se aplica Para Rangos Mayores a Cero " SKIP
              "Verifique Por Favor ....."  VIEW-AS ALERT-BOX ERROR.
          SELF:SCREEN-VALUE = '0.0000'.
          RETURN NO-APPLY.
      END.
      VtaListaMinGn.DtoVolP[7]:SCREEN-VALUE = STRING( ROUND(F-PRECIO * ( 1 - ( DECI(SELF:SCREEN-VALUE) / 100 ) ),4) ).
      IF DECIMAL(SELF:SCREEN-VALUE) = 0 THEN VtaListaMinGn.DtoVolP[7]:SCREEN-VALUE = '0.0000'.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMinGn.DtoVolD[8]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolD[8] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolD[8] IN FRAME F-Main /* DtoVolD */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(VtaListaMinGn.DtoVolR[8]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) = 0 THEN RETURN.
      IF INTEGER(VtaListaMinGn.DtoVolR[8]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) <> 0 THEN DO:
          MESSAGE "Descuento Solo se aplica Para Rangos Mayores a Cero " SKIP
              "Verifique Por Favor ....."  VIEW-AS ALERT-BOX ERROR.
          SELF:SCREEN-VALUE = '0.0000'.
          RETURN NO-APPLY.
      END.
      VtaListaMinGn.DtoVolP[8]:SCREEN-VALUE = STRING( ROUND(F-PRECIO * ( 1 - ( DECI(SELF:SCREEN-VALUE) / 100 ) ),4) ).
      IF DECIMAL(SELF:SCREEN-VALUE) = 0 THEN VtaListaMinGn.DtoVolP[8]:SCREEN-VALUE = '0.0000'.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMinGn.DtoVolD[9]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolD[9] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolD[9] IN FRAME F-Main /* DtoVolD */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(VtaListaMinGn.DtoVolR[9]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) = 0 THEN RETURN.
      IF INTEGER(VtaListaMinGn.DtoVolR[9]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) <> 0 THEN DO:
          MESSAGE "Descuento Solo se aplica Para Rangos Mayores a Cero " SKIP
              "Verifique Por Favor ....."  VIEW-AS ALERT-BOX ERROR.
          SELF:SCREEN-VALUE = '0.0000'.
          RETURN NO-APPLY.
      END.
      VtaListaMinGn.DtoVolP[9]:SCREEN-VALUE = STRING( ROUND(F-PRECIO * ( 1 - ( DECI(SELF:SCREEN-VALUE) / 100 ) ),4) ).
      IF DECIMAL(SELF:SCREEN-VALUE) = 0 THEN VtaListaMinGn.DtoVolP[9]:SCREEN-VALUE = '0.0000'.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMinGn.DtoVolP[10]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolP[10] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolP[10] IN FRAME F-Main /* DtoVolP */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(VtaListaMinGn.DtoVolR[10]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) <> 0 THEN DO:
          MESSAGE "Descuento Solo se aplica Para Rangos Mayores a Cero " SKIP
              "Verifique Por Favor ....."  VIEW-AS ALERT-BOX ERROR.
          SELF:SCREEN-VALUE = '0.0000'.
          RETURN NO-APPLY.
      END.
      VtaListaMinGn.DtoVolD[10]:SCREEN-VALUE = STRING ( ROUND ( ( 1 -  DECIMAL(SELF:SCREEN-VALUE) / F-PRECIO ) * 100, 6 ) ).
      IF DECIMAL(SELF:SCREEN-VALUE) = 0 
          THEN ASSIGN VtaListaMinGn.DtoVolR[10]:SCREEN-VALUE = '0' VtaListaMinGn.DtoVolD[10]:SCREEN-VALUE = '0.000000'.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMinGn.DtoVolP[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolP[1] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolP[1] IN FRAME F-Main /* DtoVolP */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(VtaListaMinGn.DtoVolR[1]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) <> 0 THEN DO:
          MESSAGE "Descuento Solo se aplica Para Rangos Mayores a Cero " SKIP
              "Verifique Por Favor ....."  VIEW-AS ALERT-BOX ERROR.
          SELF:SCREEN-VALUE = '0.0000'.
          RETURN NO-APPLY.
      END.
      VtaListaMinGn.DtoVolD[1]:SCREEN-VALUE = STRING ( ROUND ( ( 1 -  DECIMAL(SELF:SCREEN-VALUE) / F-PRECIO ) * 100, 6 ) ).
      IF DECIMAL(SELF:SCREEN-VALUE) = 0 
          THEN ASSIGN VtaListaMinGn.DtoVolR[1]:SCREEN-VALUE = '0' VtaListaMinGn.DtoVolD[1]:SCREEN-VALUE = '0.000000'.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMinGn.DtoVolP[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolP[2] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolP[2] IN FRAME F-Main /* DtoVolP */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(VtaListaMinGn.DtoVolR[2]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) <> 0 THEN DO:
          MESSAGE "Descuento Solo se aplica Para Rangos Mayores a Cero " SKIP
              "Verifique Por Favor ....."  VIEW-AS ALERT-BOX ERROR.
          SELF:SCREEN-VALUE = '0.0000'.
          RETURN NO-APPLY.
      END.
      VtaListaMinGn.DtoVolD[2]:SCREEN-VALUE = STRING ( ROUND ( ( 1 -  DECIMAL(SELF:SCREEN-VALUE) / F-PRECIO ) * 100, 6 ) ).
      IF DECIMAL(SELF:SCREEN-VALUE) = 0 
          THEN ASSIGN VtaListaMinGn.DtoVolR[2]:SCREEN-VALUE = '0' VtaListaMinGn.DtoVolD[2]:SCREEN-VALUE = '0.000000'.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMinGn.DtoVolP[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolP[3] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolP[3] IN FRAME F-Main /* DtoVolP */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(VtaListaMinGn.DtoVolR[3]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) <> 0 THEN DO:
          MESSAGE "Descuento Solo se aplica Para Rangos Mayores a Cero " SKIP
              "Verifique Por Favor ....."  VIEW-AS ALERT-BOX ERROR.
          SELF:SCREEN-VALUE = '0.0000'.
          RETURN NO-APPLY.
      END.
      VtaListaMinGn.DtoVolD[3]:SCREEN-VALUE = STRING ( ROUND ( ( 1 -  DECIMAL(SELF:SCREEN-VALUE) / F-PRECIO ) * 100, 6 ) ).
      IF DECIMAL(SELF:SCREEN-VALUE) = 0 
          THEN ASSIGN VtaListaMinGn.DtoVolR[3]:SCREEN-VALUE = '0' VtaListaMinGn.DtoVolD[3]:SCREEN-VALUE = '0.000000'.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMinGn.DtoVolP[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolP[4] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolP[4] IN FRAME F-Main /* DtoVolP */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(VtaListaMinGn.DtoVolR[4]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) <> 0 THEN DO:
          MESSAGE "Descuento Solo se aplica Para Rangos Mayores a Cero " SKIP
              "Verifique Por Favor ....."  VIEW-AS ALERT-BOX ERROR.
          SELF:SCREEN-VALUE = '0.0000'.
          RETURN NO-APPLY.
      END.
      VtaListaMinGn.DtoVolD[4]:SCREEN-VALUE = STRING ( ROUND ( ( 1 -  DECIMAL(SELF:SCREEN-VALUE) / F-PRECIO ) * 100, 6 ) ).
      IF DECIMAL(SELF:SCREEN-VALUE) = 0 
          THEN ASSIGN VtaListaMinGn.DtoVolR[4]:SCREEN-VALUE = '0' VtaListaMinGn.DtoVolD[4]:SCREEN-VALUE = '0.000000'.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMinGn.DtoVolP[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolP[5] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolP[5] IN FRAME F-Main /* DtoVolP */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(VtaListaMinGn.DtoVolR[5]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) <> 0 THEN DO:
          MESSAGE "Descuento Solo se aplica Para Rangos Mayores a Cero " SKIP
              "Verifique Por Favor ....."  VIEW-AS ALERT-BOX ERROR.
          SELF:SCREEN-VALUE = '0.0000'.
          RETURN NO-APPLY.
      END.
      VtaListaMinGn.DtoVolD[5]:SCREEN-VALUE = STRING ( ROUND ( ( 1 -  DECIMAL(SELF:SCREEN-VALUE) / F-PRECIO ) * 100, 6 ) ).
      IF DECIMAL(SELF:SCREEN-VALUE) = 0 
          THEN ASSIGN VtaListaMinGn.DtoVolR[5]:SCREEN-VALUE = '0' VtaListaMinGn.DtoVolD[5]:SCREEN-VALUE = '0.000000'.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMinGn.DtoVolP[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolP[6] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolP[6] IN FRAME F-Main /* DtoVolP */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(VtaListaMinGn.DtoVolR[6]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) <> 0 THEN DO:
          MESSAGE "Descuento Solo se aplica Para Rangos Mayores a Cero " SKIP
              "Verifique Por Favor ....."  VIEW-AS ALERT-BOX ERROR.
          SELF:SCREEN-VALUE = '0.0000'.
          RETURN NO-APPLY.
      END.
      VtaListaMinGn.DtoVolD[6]:SCREEN-VALUE = STRING ( ROUND ( ( 1 -  DECIMAL(SELF:SCREEN-VALUE) / F-PRECIO ) * 100, 6 ) ).
      IF DECIMAL(SELF:SCREEN-VALUE) = 0 
          THEN ASSIGN VtaListaMinGn.DtoVolR[6]:SCREEN-VALUE = '0' VtaListaMinGn.DtoVolD[6]:SCREEN-VALUE = '0.000000'.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMinGn.DtoVolP[7]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolP[7] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolP[7] IN FRAME F-Main /* DtoVolP */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(VtaListaMinGn.DtoVolR[7]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) <> 0 THEN DO:
          MESSAGE "Descuento Solo se aplica Para Rangos Mayores a Cero " SKIP
              "Verifique Por Favor ....."  VIEW-AS ALERT-BOX ERROR.
          SELF:SCREEN-VALUE = '0.0000'.
          RETURN NO-APPLY.
      END.
      VtaListaMinGn.DtoVolD[7]:SCREEN-VALUE = STRING ( ROUND ( ( 1 -  DECIMAL(SELF:SCREEN-VALUE) / F-PRECIO ) * 100, 6 ) ).
      IF DECIMAL(SELF:SCREEN-VALUE) = 0 
          THEN ASSIGN VtaListaMinGn.DtoVolR[7]:SCREEN-VALUE = '0' VtaListaMinGn.DtoVolD[7]:SCREEN-VALUE = '0.000000'.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMinGn.DtoVolP[8]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolP[8] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolP[8] IN FRAME F-Main /* DtoVolP */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(VtaListaMinGn.DtoVolR[8]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) <> 0 THEN DO:
          MESSAGE "Descuento Solo se aplica Para Rangos Mayores a Cero " SKIP
              "Verifique Por Favor ....."  VIEW-AS ALERT-BOX ERROR.
          SELF:SCREEN-VALUE = '0.0000'.
          RETURN NO-APPLY.
      END.
      VtaListaMinGn.DtoVolD[8]:SCREEN-VALUE = STRING ( ROUND ( ( 1 -  DECIMAL(SELF:SCREEN-VALUE) / F-PRECIO ) * 100, 6 ) ).
      IF DECIMAL(SELF:SCREEN-VALUE) = 0 
          THEN ASSIGN VtaListaMinGn.DtoVolR[8]:SCREEN-VALUE = '0' VtaListaMinGn.DtoVolD[8]:SCREEN-VALUE = '0.000000'.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMinGn.DtoVolP[9]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolP[9] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolP[9] IN FRAME F-Main /* DtoVolP */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(VtaListaMinGn.DtoVolR[9]:SCREEN-VALUE) = 0 AND DECI(SELF:SCREEN-VALUE) <> 0 THEN DO:
          MESSAGE "Descuento Solo se aplica Para Rangos Mayores a Cero " SKIP
              "Verifique Por Favor ....."  VIEW-AS ALERT-BOX ERROR.
          SELF:SCREEN-VALUE = '0.0000'.
          RETURN NO-APPLY.
      END.
      VtaListaMinGn.DtoVolD[9]:SCREEN-VALUE = STRING ( ROUND ( ( 1 -  DECIMAL(SELF:SCREEN-VALUE) / F-PRECIO ) * 100, 6 ) ).
      IF DECIMAL(SELF:SCREEN-VALUE) = 0 
          THEN ASSIGN VtaListaMinGn.DtoVolR[9]:SCREEN-VALUE = '0' VtaListaMinGn.DtoVolD[9]:SCREEN-VALUE = '0.000000'.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMinGn.DtoVolR[10]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolR[10] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolR[10] IN FRAME F-Main /* DtoVolR */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(SELF:SCREEN-VALUE) = 0  THEN DO:
          DISPLAY 0 @ VtaListaMinGn.DtoVolP[10] 0 @ VtaListaMinGn.DtoVolD[10].
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMinGn.DtoVolR[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolR[1] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolR[1] IN FRAME F-Main /* DtoVolR */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(SELF:SCREEN-VALUE) = 0  THEN DO:
          DISPLAY 0 @ VtaListaMinGn.DtoVolP[1] 0 @ VtaListaMinGn.DtoVolD[1].
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMinGn.DtoVolR[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolR[2] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolR[2] IN FRAME F-Main /* DtoVolR */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(SELF:SCREEN-VALUE) = 0  THEN DO:
          DISPLAY 0 @ VtaListaMinGn.DtoVolP[2] 0 @ VtaListaMinGn.DtoVolD[2].
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMinGn.DtoVolR[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolR[3] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolR[3] IN FRAME F-Main /* DtoVolR */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(SELF:SCREEN-VALUE) = 0  THEN DO:
          DISPLAY 0 @ VtaListaMinGn.DtoVolP[3] 0 @ VtaListaMinGn.DtoVolD[3].
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMinGn.DtoVolR[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolR[4] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolR[4] IN FRAME F-Main /* DtoVolR */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(SELF:SCREEN-VALUE) = 0  THEN DO:
          DISPLAY 0 @ VtaListaMinGn.DtoVolP[4] 0 @ VtaListaMinGn.DtoVolD[4].
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMinGn.DtoVolR[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolR[5] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolR[5] IN FRAME F-Main /* DtoVolR */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(SELF:SCREEN-VALUE) = 0  THEN DO:
          DISPLAY 0 @ VtaListaMinGn.DtoVolP[5] 0 @ VtaListaMinGn.DtoVolD[5].
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMinGn.DtoVolR[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolR[6] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolR[6] IN FRAME F-Main /* DtoVolR */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(SELF:SCREEN-VALUE) = 0  THEN DO:
          DISPLAY 0 @ VtaListaMinGn.DtoVolP[6] 0 @ VtaListaMinGn.DtoVolD[6].
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMinGn.DtoVolR[7]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolR[7] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolR[7] IN FRAME F-Main /* DtoVolR */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(SELF:SCREEN-VALUE) = 0  THEN DO:
          DISPLAY 0 @ VtaListaMinGn.DtoVolP[7] 0 @ VtaListaMinGn.DtoVolD[7].
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMinGn.DtoVolR[8]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolR[8] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolR[8] IN FRAME F-Main /* DtoVolR */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(SELF:SCREEN-VALUE) = 0  THEN DO:
          DISPLAY 0 @ VtaListaMinGn.DtoVolP[8] 0 @ VtaListaMinGn.DtoVolD[8].
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMinGn.DtoVolR[9]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMinGn.DtoVolR[9] V-table-Win
ON LEAVE OF VtaListaMinGn.DtoVolR[9] IN FRAME F-Main /* DtoVolR */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      IF INTEGER(SELF:SCREEN-VALUE) = 0  THEN DO:
          DISPLAY 0 @ VtaListaMinGn.DtoVolP[9] 0 @ VtaListaMinGn.DtoVolD[9].
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "Almmmatg"}
  {src/adm/template/row-list.i "VtaListaMinGn"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "Almmmatg"}
  {src/adm/template/row-find.i "VtaListaMinGn"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement V-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  DEF VAR x-Orden AS INT NO-UNDO.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  VtaListaMinGn.fchact = TODAY.
  DO x-Orden = 1 TO 10:
      IF VtaListaMinGn.DtoVolR[x-Orden] = 0 OR VtaListaMinGn.DtoVolD[x-Orden] = 0
      THEN ASSIGN
                VtaListaMinGn.DtoVolR[x-Orden] = 0
                VtaListaMinGn.DtoVolD[x-Orden] = 0
                VtaListaMinGn.DtoVolP[x-Orden] = 0.
  END.
  RUN dispatch IN THIS-PROCEDURE ('display-fields':U).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  
  IF AVAILABLE Almmmatg AND AVAILABLE VtaListaMinGn THEN DO WITH FRAME {&FRAME-NAME}:
      F-PRECIO = VtaListaMinGn.PreOfi.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valida.
  IF RETURN-VALUE = "ADM-ERROR" THEN RETURN "ADM-ERROR".
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE procesa-parametros V-table-Win 
PROCEDURE procesa-parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /*
    Variables a usar:
    output-var-1 como ROWID
    output-var-2 como CHARACTER
    output-var-3 como CHARACTER.
    */

    CASE HANDLE-CAMPO:name:
        WHEN "" THEN .
    END CASE.
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recoge-parametros V-table-Win 
PROCEDURE recoge-parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /*
    Variables a usar:
    input-var-1 como CHARACTER
    input-var-2 como CHARACTER
    input-var-3 como CHARACTER.
    */

    CASE HANDLE-CAMPO:name:
        WHEN "" THEN .
        /*
            ASSIGN
                input-para-1 = ""
                input-para-2 = ""
                input-para-3 = "".
         */      
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "Almmmatg"}
  {src/adm/template/snd-list.i "VtaListaMinGn"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  IF p-state = 'update-begin':U THEN DO:
     RUN valida-update.
     IF RETURN-VALUE = "ADM-ERROR" THEN RETURN.
  END.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida V-table-Win 
PROCEDURE valida :
/*------------------------------------------------------------------------------
  Purpose:     Validacion de datos
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR x-ValorAnterior AS DEC NO-UNDO.
DEF VAR x-Orden AS INT NO-UNDO.
DEF VAR x-PreUni AS DEC NO-UNDO.
DEF VAR x-Precio AS DEC EXTENT 10 NO-UNDO.


DEFINE VAR hProc AS HANDLE NO-UNDO.
RUN pri/pri-librerias PERSISTENT SET hProc.

DO WITH FRAME {&FRAME-NAME} :
    ASSIGN
        x-Precio[01] = DECIMAL(VtaListaMinGn.DtoVolP[1]:SCREEN-VALUE)
        x-Precio[02] = DECIMAL(VtaListaMinGn.DtoVolP[2]:SCREEN-VALUE)
        x-Precio[03] = DECIMAL(VtaListaMinGn.DtoVolP[3]:SCREEN-VALUE)
        x-Precio[04] = DECIMAL(VtaListaMinGn.DtoVolP[4]:SCREEN-VALUE)
        x-Precio[05] = DECIMAL(VtaListaMinGn.DtoVolP[5]:SCREEN-VALUE)
        x-Precio[06] = DECIMAL(VtaListaMinGn.DtoVolP[6]:SCREEN-VALUE)
        x-Precio[07] = DECIMAL(VtaListaMinGn.DtoVolP[7]:SCREEN-VALUE)
        x-Precio[08] = DECIMAL(VtaListaMinGn.DtoVolP[8]:SCREEN-VALUE)
        x-Precio[09] = DECIMAL(VtaListaMinGn.DtoVolP[9]:SCREEN-VALUE)
        x-Precio[10] = DECIMAL(VtaListaMinGn.DtoVolP[10]:SCREEN-VALUE).
    DO x-Orden = 1 TO 10:
        IF INPUT VtaListaMinGn.DtoVolR[x-Orden] <= 0 THEN NEXT.
        IF x-ValorAnterior = 0 THEN x-ValorAnterior = INPUT VtaListaMinGn.DtoVolR[x-Orden].
        IF INPUT VtaListaMinGn.DtoVolR[x-Orden] < x-ValorAnterior THEN DO:
            MESSAGE 'Cantidad m�nima mal registrada' VIEW-AS ALERT-BOX ERROR.
            RETURN 'ADM-ERROR'.
        END.
        x-ValorAnterior = INPUT VtaListaMinGn.DtoVolR[x-Orden].
        /* ****************************************************************************************************** */
        /* Control Margen de Utilidad */
        /* ****************************************************************************************************** */
        DEFINE VAR x-Margen AS DECI NO-UNDO.
        DEFINE VAR x-Limite AS DECI NO-UNDO.
        DEFINE VAR pError AS CHAR NO-UNDO.
        /* 1ro. Calculamos el margen de utilidad */
        x-PreUni = x-Precio[x-Orden].
        RUN PRI_Margen-Utilidad IN hProc (INPUT "",
                                          INPUT VtaListaMinGn.CodMat,
                                          INPUT Almmmatg.UndBas,
                                          INPUT x-PreUni,
                                          INPUT Almmmatg.MonVta,        /*INPUT 1,*/
                                          OUTPUT x-Margen,
                                          OUTPUT x-Limite,
                                          OUTPUT pError).
        IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
            /* Error cr�tico */
            MESSAGE pError SKIP 'No admitido' VIEW-AS ALERT-BOX ERROR TITLE 'CONTROL DE MARGEN'.
            RETURN 'ADM-ERROR'.
        END.
        /* Controlamos si el margen de utilidad est� bajo a trav�s de la variable pError */
        IF pError > '' THEN DO:
            /* Error por margen de utilidad */
            /* 2do. Verificamos si solo es una ALERTA, definido por GG */
            DEF VAR pAlerta AS LOG NO-UNDO.
            RUN PRI_Alerta-de-Margen IN hProc (INPUT Almmmatg.CodMat,
                                               OUTPUT pAlerta).
            IF pAlerta = YES THEN MESSAGE pError VIEW-AS ALERT-BOX WARNING TITLE 'CONTROL DE MARGEN'.
            ELSE DO:
                MESSAGE pError SKIP 'No admitido' VIEW-AS ALERT-BOX ERROR TITLE 'CONTROL DE MARGEN'.
                RETURN 'ADM-ERROR'.
            END.
        END.
    END.
END.
DELETE PROCEDURE hProc.
RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida-update V-table-Win 
PROCEDURE valida-update :
/*------------------------------------------------------------------------------
  Purpose:     Rutina de validacion en caso de modificacion
  Parameters:  Regresar "ADM-ERROR" si no se quiere modificar
  Notes:       
------------------------------------------------------------------------------*/

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
