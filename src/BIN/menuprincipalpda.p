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

DEFINE SHARED VARIABLE s-user-id AS CHARACTER FORMAT "x(16)".
DEFINE SHARED VARIABLE s-codcia  AS INTEGER   FORMAT "999".
DEFINE SHARED VARIABLE s-ruccia  AS CHAR.
DEFINE SHARED VARIABLE cb-codcia AS INTEGER   FORMAT "999".
DEFINE SHARED VARIABLE cl-codcia AS INTEGER   FORMAT "999".
DEFINE SHARED VARIABLE pv-codcia AS INTEGER   FORMAT "999".
DEFINE SHARED VARIABLE s-nomcia  AS CHARACTER FORMAT "X(50)".
DEFINE SHARED VARIABLE s-nomcia1 AS CHARACTER FORMAT "X(60)".
DEFINE SHARED VARIABLE s-dircia  AS CHARACTER FORMAT "X(50)".
/* DEFINE NEW SHARED VARIABLE s-user-id AS CHARACTER FORMAT "x(16)". */
/* DEFINE NEW SHARED VARIABLE s-codcia  AS INTEGER   FORMAT "999".   */
/* DEFINE NEW SHARED VARIABLE s-ruccia  AS CHAR.                     */
/* DEFINE NEW SHARED VARIABLE cb-codcia AS INTEGER   FORMAT "999".   */
/* DEFINE NEW SHARED VARIABLE cl-codcia AS INTEGER   FORMAT "999".   */
/* DEFINE NEW SHARED VARIABLE pv-codcia AS INTEGER   FORMAT "999".   */
/* DEFINE NEW SHARED VARIABLE s-nomcia  AS CHARACTER FORMAT "X(50)". */
/* DEFINE NEW SHARED VARIABLE s-nomcia1 AS CHARACTER FORMAT "X(60)". */
/* DEFINE NEW SHARED VARIABLE s-dircia  AS CHARACTER FORMAT "X(50)". */

DEFINE VAR OK AS LOG NO-UNDO.

/* CONFIGURACIONES DE ENTORNO POR DEFECTO */
SESSION:DATA-ENTRY-RETURN = NO.    /* << OJO actua como si fuera TAB */
SESSION:APPL-ALERT-BOXES  = NO.     /* << OJO si es TRUE los mensajes son alert-box */
SESSION:SYSTEM-ALERT-BOXES  = YES.     /* << OJO si es TRUE los mensajes son alert-box */
SESSION:SUPPRESS-WARNINGS  = YES.     /* << OJO si es TRUE los mensajes son alert-box */
SESSION:TIME-SOURCE = "integral".   /* Toma la fecha y hora del servidor de 'integral' */

FIND FIRST gn-cias WHERE (s-codcia = 000 OR gn-cias.codcia = s-codcia) NO-LOCK NO-ERROR.
IF NOT AVAILABLE gn-cias THEN DO:
    MESSAGE 'Compañias aún no han registradas' VIEW-AS ALERT-BOX ERROR.
    QUIT.
END.
ASSIGN
    s-codcia = GN-Cias.CodCia
    s-nomcia = GN-Cias.NomCia
    s-nomcia1= GN-Cias.Libre-C[1]
    s-dircia = GN-Cias.DirCia
    s-ruccia = STRING(GN-Cias.RucCia).

ASSIGN
    s-user-id = USERID("integral").
RUN bin/_registr.w("YES").
IF ( s-user-id <> "MASTER" AND s-user-id <> "ADMIN" ) AND
    NOT CAN-FIND(FIRST integral.PF-G004 WHERE integral.PF-G004.User-Id = s-user-id) THEN DO:
    MESSAGE "El usuario no tiene aplicaciones inscritas"
        VIEW-AS ALERT-BOX ERROR.
    QUIT.
END.

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
         HEIGHT             = 6.19
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DEFINE NEW SHARED VARIABLE s-admin      AS LOGICAL   FORMAT "Si/No".
DEFINE NEW SHARED VARIABLE s-aplic-id   AS CHARACTER FORMAT "X(3)".
DEFINE NEW SHARED VARIABLE s-prog-name  AS CHARACTER FORMAT "X(20)".
DEFINE NEW SHARED VARIABLE s-seguridad  AS CHARACTER FORMAT "X(20)".
DEFINE NEW SHARED VARIABLE s-local      AS CHARACTER FORMAT "X(5)".
DEFINE NEW SHARED VARIABLE s-OpSys      AS CHAR.
GET-KEY-VALUE SECTION 'Startup' KEY 'OpSysVersion' VALUE s-OpSys.

/* ****************************************************************************** */
/* ********************************* USUARIO Y SEGURIDAD ************************ */
ASSIGN
    s-seguridad = ""
    s-admin     = TRUE.
IF s-user-id <> "MASTER" AND s-user-id <> "ADMIN"  THEN DO:
    FIND FIRST PF-G004 WHERE
        PF-G004.User-Id  = s-user-id AND
        /*PF-G004.Aplic-Id = s-aplic-id AND*/
        PF-G004.CodCia   = s-codcia NO-LOCK NO-ERROR.
    IF NOT AVAILABLE PF-G004 THEN DO:
        FIND FIRST PF-G004 WHERE
            PF-G004.User-Id  = s-user-id AND
            /*PF-G004.Aplic-Id = s-aplic-id AND*/
            PF-G004.CodCia   = 0 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE PF-G004 THEN DO:
            MESSAGE
                "Usuario no autorizado para ingreso" SKIP
                "a la compañía seleccionada." VIEW-AS ALERT-BOX ERROR.
                RETURN ERROR.
        END.
    END.
    /* RHC 06.05.2011 Control de usuarios */
    FIND FIRST gn-users WHERE gn-users.codcia = s-codcia 
        AND gn-users.User-Id = PF-G004.User-Id
        AND gn-users.DISABLED = YES
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-users THEN DO:
        MESSAGE 'Usuario INHABILITADO' VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.
    /* *********************** */
    ASSIGN
        s-seguridad = PF-G004.Seguridad
        s-admin     = PF-G004.admin.
END.
/* ****************************************************************************** */
/* ********************* Datos Generales por Empresa ***************************** */
/* ****************************************************************************** */
FIND FIRST Empresas WHERE Empresas.codcia = s-codcia NO-LOCK NO-ERROR.
IF AVAILABLE Empresas THEN DO:
    IF NOT Empresas.Campo-CodCbd THEN cb-codcia = s-codcia.
    IF NOT Empresas.Campo-CodCli THEN cl-codcia = s-codcia.
    IF NOT Empresas.Campo-CodPro THEN pv-codcia = s-codcia.
END.
/* ****************************************************************************** */
/* ********************** Datos Generales para Ventas *************************** */
/* ****************************************************************************** */
DEFINE NEW SHARED VARIABLE S-CODDIV AS CHAR INIT "".
DEFINE NEW SHARED VARIABLE S-CodVen AS CHAR.
DEFINE NEW SHARED VARIABLE S-LisNiv AS CHAR.

FIND FIRST FacUsers WHERE FacUsers.CodCia = S-CODCIA 
    AND  FacUsers.Usuario = S-USER-ID 
    NO-LOCK NO-ERROR.
IF AVAILABLE FacUsers THEN
    ASSIGN 
       s-CodDiv = FacUsers.CodDiv
       S-CodVen = FacUsers.CodVen
       S-LisNiv = FacUsers.Niveles.
/* ****************************************************************************** */
/* *********************** Datos Generales por Almacén ************************** */
/* ****************************************************************************** */
DEFINE NEW SHARED VARIABLE S-CODALM AS CHAR INIT "".
DEFINE NEW SHARED VARIABLE S-DESALM AS CHAR INIT "".
DEFINE NEW SHARED VARIABLE S-MOVUSR AS CHAR INIT "".
DEFINE NEW SHARED VARIABLE S-STATUS-ALMACEN AS LOG INIT YES.

FIND FIRST Almacen WHERE Almacen.CodCia = s-CodCia
    AND CAN-FIND(FIRST AlmUsers WHERE
            AlmUsers.CodCia = Almacen.CodCia AND
            AlmUsers.CodAlm = Almacen.CodAlm AND
            AlmUsers.User-Id = s-user-id NO-LOCK)
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almacen THEN
    FIND FIRST Almacen WHERE Almacen.CodCia = s-CodCia NO-LOCK NO-ERROR.
IF AVAILABLE Almacen THEN
    ASSIGN
        s-codalm = Almacen.CodAlm
        s-desalm = Almacen.Descripcion
        s-coddiv = Almacen.CodDiv
        s-status-almacen = (IF Almacen.Campo-c[9] = "I" THEN NO ELSE YES).
FIND gn-divi WHERE gn-divi.codcia = s-codcia AND gn-divi.coddiv = s-coddiv
    NO-LOCK NO-ERROR.
/* VERIFICAMOS MOVIMIENTOS VALIDOS POR USUARIO */
/* Ej I,03|I,13|S,03 */
FOR EACH AlmUsrMov NO-LOCK WHERE AlmUsrMov.CodCia = s-codcia
  AND AlmUsrMov.User-Id = s-user-id:
  IF s-MovUsr = ''
      THEN s-MovUsr = TRIM(AlmUsrMov.TipMov) + ',' + TRIM(STRING(AlmUsrMov.CodMov)).
  ELSE s-MovUsr = s-MovUsr + '|' + TRIM(AlmUsrMov.TipMov) + ',' + TRIM(STRING(AlmUsrMov.CodMov)).
END.
/* ****************************************************************************** */
/* ******************** Datos Generales para Caja Cobranzas ********************* */
/* ****************************************************************************** */
DEF NEW SHARED VAR s-codter LIKE ccbcterm.codter.

DEF VAR pOk AS LOG INIT NO NO-UNDO.

FIND FIRST integral.PF-G003a WHERE integral.PF-G003a.Aplic-Id = "CJC"
    AND CAN-FIND(FIRST integral.PF-G004 OF integral.PF-G003a WHERE integral.PF-G004.User-Id = s-User-id
                 NO-LOCK)
    NO-LOCK NO-ERROR.
IF s-user-id = 'ADMIN' OR AVAILABLE integral.PF-G003a THEN RUN ccb/d-caja01.r (OUTPUT pOk).
FIND Ccbcterm WHERE  CcbCTerm.CodCia = s-codcia 
    AND CcbCTerm.CodDiv = s-coddiv
    AND CcbCTerm.CodTer = s-codter
    NO-LOCK NO-ERROR.

/* Definimos variables para manejar el menú */
DEFINE VARIABLE hMenu     AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE hSubMenu  AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE hSubSubMenu  AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE hMenuItem AS WIDGET-HANDLE NO-UNDO.
 
/*
The MENU widget is the top level, the next level is the SUB-MENU widget, 
and the bottom level is the MENU-ITEM widget. 
The MENU widget must be related to the window widget using the property MENUBAR:
ASSIGN CURRENT-WINDOW:MENUBAR = hMenu:HANDLE.
*/

/* Definimos variables del menú */
/*DEFINE VARIABLE cSubMenues AS CHARACTER INITIAL "Sistema,Módulos,Administración,Seguridad" NO-UNDO.*/
DEFINE VARIABLE cSubMenues AS CHARACTER INITIAL "Módulos" NO-UNDO.
DEFINE VARIABLE cMenuItem1 AS CHARACTER INITIAL "Cambiar Contraseña de Usuario,Cambiar de Almacén,Salir" NO-UNDO.
DEFINE VARIABLE cMenuItem2 AS CHARACTER INITIAL "Clave" NO-UNDO.
DEFINE VARIABLE cMenuItem3 AS CHARACTER NO-UNDO.
 
DEFINE VARIABLE iAuxlr1 AS INTEGER NO-UNDO.
DEFINE VARIABLE iAuxlr2 AS INTEGER NO-UNDO.
DEFINE VARIABLE iItems  AS INTEGER NO-UNDO.
DEFINE VARIABLE cItems  AS CHARACTER NO-UNDO.


/* Armamos el Menú */
CREATE MENU hMenu.
DO iAuxlr1 = 1 TO NUM-ENTRIES(cSubMenues):
    /* Creamos cada opción del menu horizontal */
    CREATE SUB-MENU hSubMenu 
        ASSIGN 
        PARENT = hMenu 
        NAME   = ENTRY(iAuxlr1, cSubMenues)
        LABEL  = ENTRY(iAuxlr1, cSubMenues)
        FONT   = 8.

    /* Creamos los módulos en forma vertical */
    CASE hSubMenu:NAME: 
        WHEN "Sistema" THEN DO:
            ASSIGN  iItems = NUM-ENTRIES(cMenuItem1) 
                    cItems = cMenuItem1.
            DO iAuxlr2 = 1 TO iItems: 
                CREATE MENU-ITEM hMenuItem 
                    ASSIGN PARENT = hSubMenu
                    NAME = hSubMenu:NAME + ENTRY(iAuxlr2, cItems)
                    LABEL = ENTRY(iAuxlr2, cItems) 
                    FONT = 8
                    TRIGGERS:
                        ON CHOOSE PERSISTENT RUN MenuItemChoose IN THIS-PROCEDURE (INPUT hMenuItem:NAME).
                    END TRIGGERS. 
                    /*Example of how to disable a Menu-Item*/ 
                    IF hMenuItem:NAME = "EditUndo" THEN ASSIGN hMenuItem:SENSITIVE = FALSE. 
            END. /*DO iAuxlr2 = 1 TO iItems:*/
        END.
        WHEN "Seguridad" THEN 
            ASSIGN  iItems = NUM-ENTRIES(cMenuItem2)
                    cItems = cMenuItem2.
        WHEN "Módulos" THEN RUN Crea-Modulos.
        WHEN "Administración" THEN RUN Crea-Modulos-Administracion.
   END. /*CASE hSubMenu:NAME:*/ 
END. /*DO iAuxlr1 = 1 TO NUM-ENTRIES(cSubMenues):*/
 
ASSIGN CURRENT-WINDOW:MENUBAR = hMenu:HANDLE.
 
/* Definimos Pantalla */
DEF VAR FILL-IN-Empresa     AS CHAR     LABEL 'Empresa'     FORMAT 'x(40)'  NO-UNDO.
DEF VAR FILL-IN-Division    AS CHAR     LABEL 'División'    FORMAT 'x(40)'  NO-UNDO.
DEF VAR FILL-IN-Almacen     AS CHAR     LABEL 'Almacén'     FORMAT 'x(40)'  NO-UNDO.
DEF VAR FILL-IN-Terminal    AS CHAR     LABEL 'Terminal'    FORMAT 'x(40)' NO-UNDO.
ASSIGN
    FILL-IN-Empresa  = CAPS(s-nomcia)
    FILL-IN-Division = (IF AVAILABLE gn-divi THEN gn-divi.coddiv + ' - ' + GN-DIVI.DesDiv ELSE 'NO DEFINIDO')
    fill-IN-Almacen  = (IF AVAILABLE almacen THEN almacen.codalm + ' - ' + s-desalm ELSE 'NO DEFINIDO')
    FILL-IN-Terminal = (IF AVAILABLE CcbCTerm THEN CcbCTerm.CodTer ELSE 'NO DEFINIDO').

/* DEFINE IMAGE Logotipo     */
/*     FILE "img/utiles.bmp" */
/*     TRANSPARENT.          */

/* Debe ser lo mas pequeño posible */
/* Definimos el tamaño de la ventana en carateres */
ASSIGN 
    CURRENT-WINDOW:HEIGHT-CHARS = 10
    CURRENT-WINDOW:WIDTH-CHARS  = 50
    CURRENT-WINDOW:FONT = 8
    /*CURRENT-WINDOW:WINDOW-STATE = 1*/.
DEFINE FRAME F1 
    /*Logotipo */
    FILL-IN-Empresa  AT COLUMN 1 ROW 2 BGCOLOR 15 FONT 4
    FILL-IN-Division AT COLUMN 1 ROW 3 BGCOLOR 15 FONT 4
    FILL-IN-Almacen  AT COLUMN 1 ROW 4 BGCOLOR 15 FONT 4
    FILL-IN-Terminal AT COLUMN 1 ROW 5 BGCOLOR 15 FONT 4
    WITH /*SIZE 40 BY 60*/ THREE-D SIDE-LABELS /*NO-LABELS*/.
DISPLAY FILL-IN-Empresa FILL-IN-Division FILL-IN-Almacen FILL-IN-Terminal
    WITH FRAME F1.
ENABLE ALL 
    EXCEPT FILL-IN-Empresa FILL-IN-Division FILL-IN-Almacen FILL-IN-Terminal
    WITH FRAME F1.
WAIT-FOR CLOSE OF CURRENT-WINDOW.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Crea-Modulos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Crea-Modulos Procedure 
PROCEDURE Crea-Modulos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* Los módulos van a depender del usuario */
   DEF VAR hSubSubMenu AS HANDLE EXTENT 10 NO-UNDO.
   DEF VAR hSubMenuItem   AS HANDLE  NO-UNDO.
   DEF VAR hParent     AS HANDLE NO-UNDO.
   DEF VAR iNivel AS INT NO-UNDO.
   DEF VAR cProgram AS CHAR NO-UNDO.
   DEF VAR cParam AS CHAR NO-UNDO.
   DEF VAR k AS INT NO-UNDO.
   FOR EACH integral.PF-G003a NO-LOCK WHERE NOT integral.PF-G003a.Aplic-Id BEGINS "@":
       s-seguridad = "".
       IF s-user-id <> "ADMIN" THEN DO:
           FIND FIRST integral.PF-G004 OF integral.PF-G003a WHERE integral.PF-G004.User-Id = s-User-id
               NO-LOCK NO-ERROR.
           IF NOT AVAILABLE integral.PF-G004 THEN NEXT.
           s-seguridad = PF-G004.Seguridad.
       END.
       /*FIRST integral.PF-G004 OF integral.PF-G003a NO-LOCK WHERE integral.PF-G004.User-Id = s-User-id:*/
       /* 1ro el Módulo, por ej. CONTABILIDAD */
       CREATE SUB-MENU hMenuItem 
           ASSIGN PARENT = hSubMenu
                    NAME = INTEGRAL.PF-G003a.Aplic-Id
                   LABEL = INTEGRAL.PF-G003a.Detalle 
                    FONT = 8.
       /* 2do sus submenus, por ej. Maestros,Movimientos, etc */
       
       /* Barremos por niveles */
       FOR EACH integral.PF-G002a NO-LOCK WHERE integral.PF-G002a.Aplic-Id = integral.PF-G003a.Aplic-Id
           BY integral.PF-G002a.CodMnu:
           /* VERIFICAMOS SU NIVEL DE SEGURIDAD */
           OK = YES.
           IF integral.PF-G002a.Seguridad-Grupos <> "" THEN
           Seguridad:
           DO:
               DO k = 1 TO NUM-ENTRIES( s-seguridad ):
                   IF LOOKUP(ENTRY( k, s-seguridad ), PF-G002a.Seguridad-Grupos ) > 0 THEN LEAVE Seguridad.
/*                    IF CAN-DO( PF-G002a.Seguridad-Grupos, ENTRY( k, s-seguridad )) */
/*                    THEN LEAVE Seguridad.                                          */
               END.
               OK = NO.
           END.
           IF S-USER-ID = "ADMIN" THEN OK = YES.
           IF NOT OK THEN NEXT.
           /* *********************************** */


           iNivel = LENGTH(integral.PF-G002a.CodMnu) / 2.
           hParent = (IF iNivel = 1 THEN hMenuItem ELSE  hSubSubMenu[iNivel - 1]).
           CASE integral.PF-G002a.Tipo:
               WHEN 'SUB-MENU' THEN DO:
                   CREATE SUB-MENU hSubSubMenu[iNivel]
                       ASSIGN PARENT = hParent 
                                NAME = integral.PF-G002a.Etiqueta
                               LABEL = integral.PF-G002a.Etiqueta
                                FONT = 8.
               END.
               WHEN 'PROCESO' THEN DO:
                   CREATE MENU-ITEM hSubMenuItem
                        ASSIGN PARENT       = hParent
                               LABEL        = integral.PF-G002a.Etiqueta
                               NAME         = integral.PF-G002a.Etiqueta
                               FONT         = 8
                               TRIGGERS:
                                   ON CHOOSE PERSISTENT RUN Ejecutar-Programa IN THIS-PROCEDURE (INTEGRAL.PF-G002a.Programa, INTEGRAL.pf-g002a.Parametros).
                               END TRIGGERS. 

               END.
           END CASE.
       END.
/*        FOR EACH integral.PF-G002a NO-LOCK WHERE integral.PF-G002a.Aplic-Id = integral.PF-G003a.Aplic-Id */
/*            AND integral.PF-G002a.Tipo = "SUB-MENU":                                                   */
/*            IF LENGTH(integral.PF-G002a.CodMnu) <> 2 THEN NEXT.                                        */
/*            CREATE SUB-MENU hSubSubMenu                                                               */
/*                ASSIGN PARENT = hMenuItem                                                             */
/*                         NAME = integral.PF-G002a.Etiqueta                                             */
/*                        LABEL = integral.PF-G002a.Etiqueta.                                            */
/*        END.                                                                                          */
   END.

END PROCEDURE.
/*
   FOR EACH integral.PF-G003a NO-LOCK WHERE NOT integral.PF-G003a.Aplic-Id BEGINS "@",
       EACH integral.PF-G004 OF integral.PF-G003a NO-LOCK WHERE integral.PF-G004.User-Id = s-User-id:
       CREATE MENU-ITEM hMenuItem 
           ASSIGN PARENT = hSubMenu
                    NAME = INTEGRAL.PF-G003a.Aplic-Id
                   LABEL = INTEGRAL.PF-G003a.Detalle 
           TRIGGERS:
              ON CHOOSE PERSISTENT RUN bin/_medio.p( 001, INTEGRAL.PF-G003a.Aplic-Id )..
           END TRIGGERS. 
   END.
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Crea-Modulos-Administracion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Crea-Modulos-Administracion Procedure 
PROCEDURE Crea-Modulos-Administracion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* Los módulos van a depender del usuario */
   DEF VAR hSubSubMenu AS HANDLE EXTENT 10 NO-UNDO.
   DEF VAR hSubMenuItem   AS HANDLE  NO-UNDO.
   DEF VAR hParent     AS HANDLE NO-UNDO.
   DEF VAR iNivel AS INT NO-UNDO.
   DEF VAR cProgram AS CHAR NO-UNDO.
   DEF VAR cParam AS CHAR NO-UNDO.
   FOR EACH integral.PF-G004 NO-LOCK WHERE integral.PF-G004.Aplic-Id = "@@"
       AND integral.PF-G004.User-Id = s-User-id,
       EACH integral.PF-G002a NO-LOCK WHERE integral.PF-G002a.Aplic-Id = integral.PF-G004.Aplic-Id
           BY integral.PF-G002a.CodMnu:
           iNivel = LENGTH(integral.PF-G002a.CodMnu) / 2.
           hParent = (IF iNivel = 1 THEN hSubMenu ELSE  hSubSubMenu[iNivel - 1]).
           CASE integral.PF-G002a.Tipo:
               WHEN 'SUB-MENU' THEN DO:
                   CREATE SUB-MENU hSubSubMenu[iNivel]
                       ASSIGN PARENT = hParent 
                                NAME = integral.PF-G002a.Etiqueta
                               LABEL = integral.PF-G002a.Etiqueta.
               END.
               WHEN 'PROCESO' THEN DO:
                   CREATE MENU-ITEM hSubMenuItem
                        ASSIGN PARENT       = hParent
                               LABEL        = integral.PF-G002a.Etiqueta
                               NAME         = integral.PF-G002a.Etiqueta
                               TRIGGERS:
                                   ON CHOOSE PERSISTENT RUN Ejecutar-Programa (INTEGRAL.PF-G002a.Programa, PF-G002a.Icon).
                               END TRIGGERS. 

               END.
           END CASE.
   END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Ejecutar-Programa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ejecutar-Programa Procedure 
PROCEDURE Ejecutar-Programa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER cProg AS CHAR.
DEF INPUT PARAMETER cPara AS CHAR.

IF cPara = '' THEN RUN VALUE(cProg).
ELSE RUN VALUE(cProg) (cPara).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MenuItemChoose) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MenuItemChoose Procedure 
PROCEDURE MenuItemChoose :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   DEFINE INPUT PARAMETER cMenuItemName AS CHARACTER NO-UNDO.
    
   DEF VAR s-Ok AS LOG.
   CASE cMenuItemName:
       WHEN "SistemaCambiar de Almacén" THEN  DO:
           RUN alm/d-ingalm (OUTPUT s-ok).
           FIND gn-divi WHERE gn-divi.codcia = s-codcia AND gn-divi.coddiv = s-coddiv NO-LOCK.
           FIND almacen WHERE almacen.codcia = s-codcia AND almacen.codalm = s-codalm NO-LOCK.
           FILL-IN-Division = (IF AVAILABLE gn-divi THEN gn-divi.coddiv + ' - ' + GN-DIVI.DesDiv ELSE 'NO DEFINIDO').
           fill-IN-Almacen = (IF AVAILABLE almacen THEN almacen.codalm + ' - ' + s-desalm ELSE 'NO DEFINIDO').
           DISPLAY FILL-IN-Division FILL-IN-Almacen WITH FRAME F1.
       END.
   END CASE.
   
   IF cMenuItemName = 'FileExit' THEN QUIT.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

