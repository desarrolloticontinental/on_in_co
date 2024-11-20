&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
/* Procedure Description
"autorización de ingreso al usuario"
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
DEF OUTPUT PARAMETER pUser AS CHAR.
DEF OUTPUT PARAMETER pPassword AS CHAR.

/* &IF "{&NEW}" = "" &THEN                                */
/*     DEFINE INPUT PARAMETER cLogico AS LOGICAL NO-UNDO. */
/* &ELSE                                                  */
/*     DEFINE VARIABLE        cLogico AS LOGICAL NO-UNDO. */
/* &ENDIF                                                 */
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE veces       AS INTEGER NO-UNDO.
DEFINE VARIABLE Ciclo       AS INTEGER NO-UNDO INITIAL 0.
DEFINE VARIABLE x-user-name AS CHARACTER FORMAT "X(16)" NO-UNDO.
DEFINE VARIABLE pto AS LOGICAL.
pto = SESSION:SET-WAIT-STATE("").

SESSION:DATA-ENTRY-RETURN = YES.

/*
IF LDBNAME("DICTDB") <> "INTEGRAL"
THEN DO ON ERROR UNDO, LEAVE:
    IF NOT SETUSERID(USERID("INTEGRAL"), "", "DICTDB") 
    THEN RUN CREATE_USER.
    RETURN.
END.
*/
DEFINE VAR x-usuario-vacio AS INT.

/* DEFINE BUFFER x-vtatabla FOR vtatabla. */
/* DEFINE BUFFER b-_user FOR _user.       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME W-INGRESO

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS b-acepta IMAGE-4 S-User-Id x-password ~
b-cancelar 
&Scoped-Define DISPLAYED-OBJECTS S-User-Id x-password 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON b-acepta AUTO-GO 
     IMAGE-UP FILE "img\b-ok":U NO-FOCUS
     LABEL "&Aceptar" 
     SIZE 12 BY 1.54.

DEFINE BUTTON b-cancelar AUTO-END-KEY 
     IMAGE-UP FILE "img/b-cancel":U NO-FOCUS
     LABEL "&Cancelar" 
     SIZE 12 BY 1.54.

DEFINE VARIABLE S-User-Id AS CHARACTER FORMAT "X(16)":U 
     LABEL "Usuario" 
     VIEW-AS FILL-IN 
     SIZE 18.57 BY .81
     BGCOLOR 15 FGCOLOR 0 FONT 0.

DEFINE VARIABLE x-password AS CHARACTER FORMAT "X(16)":U 
     LABEL "Clave" 
     VIEW-AS FILL-IN 
     SIZE 18.57 BY .81
     BGCOLOR 15 FGCOLOR 0 FONT 0 NO-UNDO.

DEFINE IMAGE IMAGE-4
     FILENAME "img/continental.jpg":U
     SIZE 48 BY 3.46.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME W-INGRESO
     b-acepta AT ROW 7.54 COL 8
     S-User-Id AT ROW 5.04 COL 19 COLON-ALIGNED
     x-password AT ROW 6.15 COL 19 COLON-ALIGNED PASSWORD-FIELD 
     b-cancelar AT ROW 7.54 COL 39
     IMAGE-4 AT ROW 1.19 COL 6
     SPACE(4.28) SKIP(4.92)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 FGCOLOR 0 FONT 4
         TITLE "Ingreso al Sistema".


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
   FRAME-NAME                                                           */
ASSIGN 
       FRAME W-INGRESO:SCROLLABLE       = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-INGRESO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-INGRESO W-INGRESO
ON GO OF FRAME W-INGRESO /* Ingreso al Sistema */
DO:
    ASSIGN x-password 
           s-user-id     = CAPS(TRIM((INPUT s-user-id ))).
    
    IF s-user-id = "" THEN DO:
        x-usuario-vacio = x-usuario-vacio + 1.
        IF x-usuario-vacio >= 5 THEN DO:
            QUIT.
        END.
        ELSE DO:
            APPLY "ENTRY" TO s-user-id.
            RETURN NO-APPLY.
        END.
    END.
    pUser = s-user-id.
    pPassword = x-Password.
    
/*     IF NOT SETUSERID(s-user-id, x-password, "INTEGRAL") THEN DO:                                                   */
/*         MESSAGE "Credenciales estan incorrectas..." VIEW-AS ALERT-BOX ERROR.                                       */
/*         IF veces > 1 THEN QUIT.                                                                                    */
/*         veces = veces + 1.                                                                                         */
/*         APPLY "ENTRY" TO s-user-id.                                                                                */
/*         RETURN NO-APPLY.                                                                                           */
/*     END.                                                                                                           */
/*     ELSE DO:                                                                                                       */
/*         FIND FIRST dictdb._user WHERE _user._userid = s-user-id NO-LOCK NO-ERROR.                                  */
/*         IF AVAILABLE dictdb._user THEN DO:                                                                         */
/*                                                                                                                    */
/*             IF _user._disabled THEN DO:                                                                            */
/*                 MESSAGE "Usuario esta DESACTIVADO..." VIEW-AS ALERT-BOX ERROR.                                     */
/*                 QUIT.                                                                                              */
/*             END.                                                                                                   */
/*             ELSE DO:                                                                                               */
/*                 /* 02jun2017, Martin Salcedo, poner limite para usuarios sin claves */                             */
/*                 IF _user._password = 'pjqtudckibycRKbj' THEN DO:                                                   */
/*                     veces = 5 - (IF(_user._logins = ?) THEN 1 ELSE _user._logins).                                 */
/*                     /* Password esta en Blanco */                                                                  */
/*                     IF veces > 0 THEN DO:                                                                          */
/*                     /*IF TRUE  THEN DO:*/                                                                          */
/*                         MESSAGE "Su usuario no tiene contraseña" SKIP                                              */
/*                                 "Especifique una contraseña." SKIP                                                 */
/*                                 "Ingrese a : " SKIP                                                                */
/*                                 "*  SEGURIDAD" SKIP                                                                */
/*                                 "*  CLAVE DE INGRESO"                                                              */
/*                             VIEW-AS ALERT-BOX ERROR TITLE "ATENCION!!!".                                           */
/*                          MESSAGE "Le quedan " + STRING(veces - 1,">>9") + " intentos para ingresar sin contraseña" */
/*                                  VIEW-AS ALERT-BOX WARNING TITLE "ATENCION!!!".                                    */
/*                          /* Grabo los intentos */                                                                  */
/*                          DEFINE BUFFER bb_user FOR dictdb._user.                                                   */
/*                          FIND FIRST bb_user WHERE bb_user._userid = s-user-id NO-ERROR.                            */
/*                          IF AVAILABLE bb_user THEN DO:                                                             */
/*                              ASSIGN bb_user._logins = (IF(_user._logins = ?) THEN 0 ELSE _user._logins) + 1.       */
/*                          END.                                                                                      */
/*                          RELEASE bb_user.                                                                          */
/*                     END.                                                                                           */
/*                     ELSE DO:                                                                                       */
/*                         MESSAGE "No puede ingresar al Sistema sin contraseña." SKIP                                */
/*                                 "Por favor contacte con el dpto de sistemas"                                       */
/*                             VIEW-AS ALERT-BOX WARNING TITLE "ATENCION!!!".                                         */
/*                             QUIT.                                                                                  */
/*                     END.                                                                                           */
/*                 END.                                                                                               */
/*                                                                                                                    */
/*                 /*IF USERID("DICTDB") <> "ADMIN" AND USERID("DICTDB") <> "MASTER" THEN DO:*/                       */
/*                 IF USERID("DICTDB") <> "MASTER" THEN DO:                                                           */
/*                     /* Valida PICO y SESSION */                                                                    */
/*                     DEFINE VAR x-hProc AS HANDLE NO-UNDO.           /* Handle Libreria */                          */
/*                     DEFINE VAR x-retval AS CHAR.                                                                   */
/*                                                                                                                    */
/*                     RUN adm\pico-session.p PERSISTENT SET x-hProc.                                                 */
/*                                                                                                                    */
/*                     /* Procedimientos */                                                                           */
/*                     RUN pico-session IN x-hProc (INPUT "APLICACION", INPUT "PROGRAMA",                             */
/*                                                INPUT s-user-id, OUTPUT x-retval).                                  */
/*                                                                                                                    */
/*                     DELETE PROCEDURE x-hProc.                       /* Release Libreria */                         */
/*                                                                                                                    */
/*                     IF x-retval <> "OK" THEN DO:                                                                   */
/*                         MESSAGE "PICO y SESSION" SKIP                                                              */
/*                                 x-retval                                                                           */
/*                             VIEW-AS ALERT-BOX INFORMATION.                                                         */
/*                         QUIT.                                                                                      */
/*                     END.                                                                                           */
/*                                                                                                                    */
/*                     /* Si el usuario tiene su pass actualizado */                                                  */
/*                     IF NOT (_user._password = 'pjqtudckibycRKbj') THEN DO:                                         */
/*                                                                                                                    */
/*                         RUN verificar-pass-actualizado.                                                            */
/*                                                                                                                    */
/*                         IF RETURN-VALUE = "ADM-ERROR" THEN DO:                                                     */
/*                             QUIT.                                                                                  */
/*                         END.                                                                                       */
/*                     END.                                                                                           */
/*                 END.                                                                                               */
/*             END.                                                                                                   */
/*         END.                                                                                                       */
/*     END.                                                                                                           */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-INGRESO W-INGRESO
ON WINDOW-CLOSE OF FRAME W-INGRESO /* Ingreso al Sistema */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-cancelar W-INGRESO
ON CHOOSE OF b-cancelar IN FRAME W-INGRESO /* Cancelar */
DO:
    QUIT.
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME x-password
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL x-password W-INGRESO
ON RETURN OF x-password IN FRAME W-INGRESO /* Clave */
DO:
    APPLY "CHOOSE" TO b-acepta.
    RETURN NO-APPLY.    
END.

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

s-user-id = "".
x-password = "".

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN enable_UI.
    s-user-id:SCREEN-VALUE = "".
    x-password:SCREEN-VALUE = "".
    /*En OpenEdge ya no es necesario*
    run SendMessageA (x-password:HWND in frame {&FRAME-NAME}, 204, asc('*'), 0).
    */
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
    RUN disable_UI.
    RETURN.
END.
RUN disable_UI.
RETURN ERROR.

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
  DISPLAY S-User-Id x-password 
      WITH FRAME W-INGRESO.
  ENABLE b-acepta IMAGE-4 S-User-Id x-password b-cancelar 
      WITH FRAME W-INGRESO.
  {&OPEN-BROWSERS-IN-QUERY-W-INGRESO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE verificar-pass-actualizado W-INGRESO 
PROCEDURE verificar-pass-actualizado :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* IF NOT AVAILABLE _user THEN RETURN "ADM-ERROR".                                                         */
/*                                                                                                         */
/* DEFINE VAR x-dias AS INT INIT 90.                                                                       */
/* DEFINE VAR x-dias-advertencia AS INT INIT 3.                                                            */
/* DEFINE VAR x-data AS CHAR.                                                                              */
/* DEFINE VAR x-inicio AS DATE.                                                                            */
/* DEFINE VAR x-antiguedad AS INT.                                                                         */
/*                                                                                                         */
/* FIND FIRST x-vtatabla WHERE x-vtatabla.codcia = 1 AND                                                   */
/*                             x-vtatabla.tabla = "CONFIG-SESSION" AND                                     */
/*                             x-vtatabla.llave_c1 = "SESSION" AND                                         */
/*                             x-vtatabla.llave_c2 = "VIGENCIAS-PASS-DIAS" NO-LOCK NO-ERROR.               */
/*                                                                                                         */
/* IF AVAILABLE x-vtatabla THEN DO:                                                                        */
/*     x-dias = x-vtatabla.valor[1].                                                                       */
/*     x-dias-advertencia = x-vtatabla.valor[2].                                                           */
/*                                                                                                         */
/*     IF x-dias-advertencia < 0 THEN x-dias-advertencia = 3.                                              */
/* END.                                                                                                    */
/*                                                                                                         */
/* IF TRUE <> (_user._U-misc2[7] > "") THEN DO:                                                            */
/*     FIND FIRST b-_user WHERE ROWID(b-_user) = ROWID(_user) EXCLUSIVE-LOCK NO-ERROR.                     */
/*     IF AVAILABLE b-_user THEN DO:                                                                       */
/*         ASSIGN b-_user._U-misc2[7] = STRING(TODAY,"99/99/9999").                                        */
/*     END.                                                                                                */
/*     RELEASE b-_user.                                                                                    */
/*    /*                                                                                                   */
/*     MESSAGE "Estimado usuario, en cumplimiento de las normas y politicas" SKIP                          */
/*             "de seguridad impartidas por la gerencia de la empresa" SKIP                                */
/*             "Apartir de hoy se inicia el control, en donde usted debe" SKIP                             */
/*             "cambiar su clave de acceso al progress, por lo menos cada " + STRING(x-dias)  + " dia(s) " */
/*         VIEW-AS ALERT-BOX INFORMATION.                                                                  */
/*     */                                                                                                  */
/*     MESSAGE "Estimado usuario, según la gestion de TI y la politica de" SKIP                            */
/*             "seguridad de acceso impartidas por la gerencia de la empresa" SKIP                         */
/*             "Apartir de hoy se inicia el control, en donde usted debe" SKIP                             */
/*             "cambiar su clave de acceso al progress, por lo menos cada " + STRING(x-dias)  + " dia(s) " */
/*         VIEW-AS ALERT-BOX INFORMATION.                                                                  */
/*                                                                                                         */
/* END.                                                                                                    */
/* ELSE DO:                                                                                                */
/*     x-data = TRIM(_user._U-misc2[7]).                                                                   */
/*     x-inicio = DATE(x-data).                                                                            */
/*     x-antiguedad = TODAY - x-inicio.                                                                    */
/*                                                                                                         */
/*     IF x-antiguedad < 0 THEN DO:                                                                        */
/*         MESSAGE "Estimado Usuario, las normas y politica de seguridad" SKIP                             */
/*                 "indica que usted debio actualizar su clave de acceso" SKIP                             */
/*                 "Imposible iniciar session en progress" SKIP                                            */
/*                 "Coordine con su Keyuser o inmediato superior"                                          */
/*             VIEW-AS ALERT-BOX INFORMATION.                                                              */
/*         RETURN "ADM-ERROR".                                                                             */
/*     END.                                                                                                */
/*                                                                                                         */
/*     IF x-antiguedad >= 0 AND x-antiguedad > (x-dias - x-dias-advertencia) THEN DO:                      */
/*         /*                                                                                              */
/*         MESSAGE "Estimado Usuario, las normas y politica de seguridad" SKIP                             */
/*                 "indica que usted debe actualizar su clave de acceso" SKIP                              */
/*                 "al progress, de no actualizarlo oportunamente" SKIP                                    */
/*                 "el progress no le va permitir iniciar session."                                        */
/*             VIEW-AS ALERT-BOX INFORMATION.                                                              */
/*         */                                                                                              */
/*         MESSAGE "Estimado Usuario, las normas y politica de seguridad" SKIP                             */
/*                 "indica que usted debe actualizar su clave de acceso" SKIP                              */
/*                 "al progress, MIENTRAS NO CAMBIE SU CLAVE oportunamente" SKIP                           */
/*                 "el progress NO le va permitir abrir session."                                          */
/*             VIEW-AS ALERT-BOX INFORMATION.                                                              */
/*                                                                                                         */
/*     END.                                                                                                */
/* END.                                                                                                    */

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

