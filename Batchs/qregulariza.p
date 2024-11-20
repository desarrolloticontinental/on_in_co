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

DEF VAR x-CodFchF AS DATE NO-UNDO.
DEF VAR x-CodFchI AS DATE NO-UNDO.
DEF VAR s-CodCia AS INT INIT 001 NO-UNDO.

ASSIGN
    x-CodFchF = TODAY - 1
    x-CodFchI = DATE(01,01,2018).

DEF STREAM Reporte.
DEF VAR x-Archivo AS CHAR NO-UNDO.

PUT UNFORMATTED 'INICIO: ' NOW SKIP.

DEF TEMP-TABLE t-ccbdcaja LIKE ccbdcaja
    FIELD fchvto AS DATE FORMAT '99/99/9999'.

FOR EACH ccbccaja NO-LOCK WHERE CcbCCaja.CodCia = s-codcia AND
    (CcbCCaja.CodDoc = "I/C" OR 
    (CcbCCaja.CodDoc = "E/C" AND LOOKUP(CcbCCaja.Tipo, "ANTREC,DEVONC,DEVOBD") > 0)) AND
    CcbCCaja.FchDoc >= x-CodFchI AND
    CcbCCaja.FchDoc <= x-CodFchF AND
    CcbCCaja.FlgEst <> "A",
    EACH CcbDCaja OF CcbCCaja NO-LOCK:
    CREATE t-ccbdcaja.
    BUFFER-COPY ccbdcaja TO t-ccbdcaja.
    FIND ccbcdocu WHERE ccbcdocu.codcia = ccbdcaja.codcia AND
        ccbcdocu.coddoc = ccbdcaja.codref AND
        ccbcdocu.nrodoc = ccbdcaja.nroref NO-LOCK NO-ERROR.
    IF AVAILABLE ccbcdocu THEN t-ccbdcaja.fchvto = ccbcdocu.fchvto.
END.

FOR EACH Ccbdcaja NO-LOCK WHERE Ccbdcaja.codcia = s-codcia
    AND LOOKUP(Ccbdcaja.CodDoc, 'N/B,CLA,CJE,REF,RNV,CPG,CJC,CVU') > 0
    AND CcbDCaja.FchDoc >= x-CodFchI 
    AND CcbDCaja.FchDoc <= x-CodFchF:
    CREATE t-ccbdcaja.
    BUFFER-COPY ccbdcaja TO t-ccbdcaja.
    FIND ccbcdocu WHERE ccbcdocu.codcia = ccbdcaja.codcia AND
        ccbcdocu.coddoc = ccbdcaja.codref AND
        ccbcdocu.nrodoc = ccbdcaja.nroref NO-LOCK NO-ERROR.
    IF AVAILABLE ccbcdocu THEN t-ccbdcaja.fchvto = ccbcdocu.fchvto.
END.

x-Archivo = "/home/v/IN/dbs/" + "ccbdcaja" + STRING(DAY(x-CodFchI),'99') +
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH t-ccbdcaja NO-LOCK:
    EXPORT DELIMITER "~029" t-ccbdcaja.
END.
OUTPUT CLOSE.

DEFINE VARIABLE comm-line AS CHARACTER FORMAT "x(70)".                                        
comm-line = "/usr/bin/qonvtaexport2".
OS-COMMAND VALUE(comm-line) SILENT NO-CONSOLE.
PUT UNFORMATTED 'PROCESO TERMINADO ' NOW SKIP.

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
         HEIGHT             = 4.65
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


