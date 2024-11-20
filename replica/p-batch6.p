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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
/* DIVISION: 00504 */

{replica/p-batch.i &Campo = FlgDb6 &LLave = Idx06}
  
/*
DEFINE VARIABLE L-FLG  AS LOGICAL NO-UNDO.
DEFINE VARIABLE X-PROG AS CHAR FORMAT "x(20)" NO-UNDO.
DEFINE BUFFER b-replog FOR integral.replog.

REPEAT :
    FOR EACH integral.replog NO-LOCK WHERE integral.replog.FlgDb1 = NO 
        USE-INDEX idx01:
        FIND b-replog WHERE ROWID(b-replog) = ROWID(integral.replog)
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        IF NOT AVAILABLE b-replog THEN DO:
            PAUSE 2.
            NEXT.
        END.
        DISPLAY integral.replog.LogDate integral.replog.TableName integral.replog.Event 
            TODAY STRING(TIME,'HH:MM')
            WITH STREAM-IO NO-BOX WIDTH 200.
        X-PROG = TRIM(LC(integral.replog.RunProgram)).
        RUN VALUE(X-PROG) (b-replog.TransactionID, OUTPUT L-FLG).
        ASSIGN b-replog.FlgDb1 = L-FLG.
        RELEASE b-replog.
    END. 
END.

*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


