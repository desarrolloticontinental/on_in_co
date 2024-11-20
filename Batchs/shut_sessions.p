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

/* shut.p */
/* Daemon that will monitor the table SHUT */
/* This table must be loaded in the Database you want to control */
/* Insert the required database name after the PROSHUT command */


/*
REPEAT:
    FIND FIRST shut NO-LOCK.
    IF shut.shut_status THEN
    DO:
        OS-COMMAND SILENT VALUE("proshut <DBNAME> -C disconnect " +
        TRIM(STRING(shut.shut_id,">>9"))).
        FIND FIRST shut SHARE-LOCK.
        ASSIGN shut.shut_status = NO
        shut.shut_id = 0.
        RELEASE shut.
    END.
    PAUSE 1.
END.

*/

DEFINE VAR s-codcia AS INT INIT 1.
DEFINE BUFFER b-shut_user FOR shut_user.
DEFINE VAR x-rowid AS ROWID.

FOR EACH shut_user WHERE shut_user.codcia = s-codcia AND shut_user.shut_status = YES NO-LOCK:
    x-rowid = ROWID(shut_user).
    FIND FIRST b-shut_user WHERE ROWID(b-shut_user) = x-rowid EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF NOT LOCKED b-shut_user THEN DO:
        IF AVAILABLE b-shut_user THEN DO:
            PUT UNFORMATTED b-shut_user.shut_id b-shut_user.user_db SKIP.
            ASSIGN b-shut_user.shut_status = NO .
            OS-COMMAND SILENT VALUE("/usr/progress10/bin/proshut integral -C disconnect " + TRIM(STRING(shut_user.shut_id,">>9"))).
        END.
    END.
END.
RELEASE shut_user.
RELEASE b-shut_user.
PUT UNFORMATTED NOW SKIP.
QUIT.

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
         HEIGHT             = 4.92
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


