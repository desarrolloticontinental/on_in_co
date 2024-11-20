&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
/* Procedure Description
"Intermediario en la ejecución de Aplicaciones"
*/
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
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

DEFINE INPUT PARAMETER x-codcia   AS INTEGER.
DEFINE INPUT PARAMETER x-aplic-id AS CHARACTER. 
DEFINE INPUT PARAMETER x-prog-name AS CHARACTER. 

DEFINE NEW SHARED VARIABLE s-admin      AS LOGICAL   FORMAT "Si/No".
DEFINE NEW SHARED VARIABLE s-codcia     AS INTEGER   FORMAT "999".
DEFINE NEW SHARED VARIABLE s-ruccia     AS INTEGER   FORMAT "99999999".
DEFINE NEW SHARED VARIABLE cb-codcia     AS INTEGER   FORMAT "999".
DEFINE NEW SHARED VARIABLE cl-codcia     AS INTEGER   FORMAT "999".
DEFINE NEW SHARED VARIABLE pv-codcia     AS INTEGER   FORMAT "999".
DEFINE NEW SHARED VARIABLE s-OpSys      AS CHAR.
DEFINE NEW SHARED VARIABLE s-nomcia     AS CHARACTER FORMAT "X(50)".
DEFINE NEW SHARED VARIABLE s-nomcia1    AS CHARACTER FORMAT "X(60)".
DEFINE NEW SHARED VARIABLE s-dircia     AS CHARACTER FORMAT "X(50)".
DEFINE NEW SHARED VARIABLE s-aplic-id   AS CHARACTER FORMAT "X(3)".
DEFINE NEW SHARED VARIABLE s-prog-name  AS CHARACTER FORMAT "X(20)".
DEFINE NEW SHARED VARIABLE s-seguridad  AS CHARACTER FORMAT "X(20)".
DEFINE NEW SHARED VARIABLE s-local      AS CHARACTER FORMAT "X(5)".
/* DEFINE NEW SHARED VARIABLE hSocket      AS HANDLE NO-UNDO. */

DEFINE     SHARED VARIABLE s-User-id    AS CHARACTER.

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
         HEIGHT             = 5.12
         WIDTH              = 51.14.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

ASSIGN
    s-codcia    = x-codcia
    s-aplic-id  = x-aplic-id
    s-prog-name = x-prog-name.

DEFINE VAR x-retval AS CHAR INIT "OK" NO-UNDO.

DO ON ERROR UNDO, LEAVE ON STOP UNDO, LEAVE:
    RUN inscripcion.
    IF USERID("DICTDB") <> "ADMIN" AND USERID("DICTDB") <> "MASTER" THEN DO:
        /* Valida PICO y SESSION */
        DEFINE VAR x-hProc AS HANDLE NO-UNDO.           /* Handle Libreria */       

        RUN adm\pico-session.p PERSISTENT SET x-hProc.

        /* Procedimientos */
        RUN pico-session IN x-hProc (INPUT "APLICACION", INPUT "PROGRAMA", 
                                   INPUT s-user-id, OUTPUT x-retval).   
        
        DELETE PROCEDURE x-hProc.                       /* Release Libreria */
        
        IF x-retval <> "OK" THEN DO:
            MESSAGE "PICO y SESSION" SKIP
                    x-retval 
                VIEW-AS ALERT-BOX INFORMATION.
        END.        
    END.                
    IF x-retval = "OK" THEN DO:
        RUN VALUE( s-prog-name ).
    END.    
END.

RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-inscripcion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE inscripcion Procedure 
PROCEDURE inscripcion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN
    s-seguridad = ""
    s-admin     = TRUE.

IF s-user-id <> "MASTER" AND s-user-id <> "ADMIN"  THEN DO:
    FIND DICTDB.PF-G004 WHERE
        DICTDB.PF-G004.User-Id  = s-user-id AND
        DICTDB.PF-G004.Aplic-Id = s-aplic-id AND
        DICTDB.PF-G004.CodCia   = s-codcia NO-LOCK NO-ERROR.
    IF NOT AVAILABLE DICTDB.PF-G004 THEN DO:
        FIND DICTDB.PF-G004 WHERE
            DICTDB.PF-G004.User-Id  = s-user-id AND
            DICTDB.PF-G004.Aplic-Id = s-aplic-id AND
            DICTDB.PF-G004.CodCia   = 0 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE DICTDB.PF-G004 THEN DO:
            BELL.
            MESSAGE
                "Usuario no autorizado para ingreso" SKIP
                "a la compañía seleccionada." VIEW-AS ALERT-BOX ERROR.
                RETURN ERROR.
        END.
    END.
    /* RHC 06.05.2011 Control de usuarios */
    FIND FIRST gn-users WHERE gn-users.codcia = s-codcia 
        AND gn-users.User-Id = DICTDB.PF-G004.User-Id
        AND gn-users.DISABLED = YES
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-users THEN DO:
        MESSAGE 'Usuario INHABILITADO' VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.
    /* *********************** */
    ASSIGN
        s-seguridad = DICTDB.PF-G004.Seguridad
        s-admin     = DICTDB.PF-G004.admin.
END.

FIND DICTDB.GN-CIAS WHERE DICTDB.GN-CIAS.CodCia = s-codcia NO-LOCK.
ASSIGN
    s-codcia = DICTDB.GN-Cias.CodCia
    s-nomcia = DICTDB.GN-Cias.NomCia
    s-nomcia1= DICTDB.GN-Cias.Libre-C[1]
    s-dircia = DICTDB.GN-Cias.DirCia
    s-ruccia = DICTDB.GN-Cias.RucCia.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

