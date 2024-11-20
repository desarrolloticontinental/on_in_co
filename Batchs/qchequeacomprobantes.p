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

/* Verificacion de los comprobantes */
DEF VAR x-FchDoc-1 AS DATE NO-UNDO.
DEF VAR x-FchDoc-2 AS DATE NO-UNDO.
DEF VAR s-CodCia AS INT INIT 001 NO-UNDO.
DEF VAR x-Lista AS CHAR INIT 'FAC,BOL,TCK,G/R,N/C,N/D' NO-UNDO.
DEF VAR k AS INT NO-UNDO.
DEF VAR x-CodDoc AS CHAR NO-UNDO.
DEF VAR x-ImpTot AS DEC NO-UNDO.
DEF VAR x-Delta AS DEC NO-UNDO.

ASSIGN
    x-FchDoc-1 = ADD-INTERVAL(TODAY, -3, 'months')
    x-FchDoc-2 = ADD-INTERVAL(TODAY, -1, 'days').
DO k = 1 TO NUM-ENTRIES(x-Lista):
    x-CodDoc = ENTRY(k, x-Lista).
    FOR EACH Gn-divi NO-LOCK WHERE Gn-divi.codcia = s-codcia,
        EACH Ccbcdocu NO-LOCK WHERE Ccbcdocu.codcia = s-codcia
        AND Ccbcdocu.coddiv = Gn-divi.coddiv
        AND Ccbcdocu.coddoc = x-CodDoc
        AND Ccbcdocu.fchdoc >= x-FchDoc-1
        AND Ccbcdocu.fchdoc <= x-FchDoc-2
        AND Ccbcdocu.flgest <> 'A':
        x-ImpTot = 0.
        FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK:
            x-ImpTot = x-ImpTot + Ccbddocu.implin.
        END.
        /*x-Delta = ABS((Ccbcdocu.imptot + CcbcDocu.ImpDto2 - ccbcdocu.AcuBon[5] - CcbCDocu.Libre_d02) - x-ImpTot).*/
        x-Delta = ABS((Ccbcdocu.imptot + CcbcDocu.ImpDto2 - ccbcdocu.AcuBon[5]) - x-ImpTot).
        IF x-Delta > 1 THEN
            DISPLAY Ccbcdocu.coddiv Ccbcdocu.coddoc Ccbcdocu.nrodoc Ccbcdocu.fchdoc
            Ccbcdocu.imptot  CcbcDocu.ImpDto2 ccbcdocu.AcuBon[5] /*CcbCDocu.Libre_d02*/ x-imptot LABEL 'Suma Detalle'
            x-Delta LABEL 'Delta' WITH STREAM-IO NO-BOX WIDTH 320.
    END.
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
         HEIGHT             = 4.54
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


