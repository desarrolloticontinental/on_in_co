&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     : Marca FlgEst = "C" para todos los comprobantes CANCELADOS

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF VAR s-CodCia AS INT INIT 001 NO-UNDO.
DEF VAR fFchIni AS DATE NO-UNDO.
DEF VAR fFchFin AS DATE NO-UNDO.
DEF VAR fFchCan AS DATE NO-UNDO.
DISABLE TRIGGERS FOR LOAD OF CcbCDocu.

ASSIGN
    fFchIni = ADD-INTERVAL(TODAY, -1, 'year')
    fFchFin = ADD-INTERVAL(TODAY, -1, 'day').

/* SOLO DOCUMENTOS DE CARGOS (FAC, BOL, LET, ETC) */
FOR EACH FacDocum NO-LOCK WHERE FacDocum.CodCia = s-CodCia AND FacDocum.TpoDoc = YES:
    FOR EACH CcbCDocu EXCLUSIVE-LOCK WHERE CcbCDocu.CodCia = s-CodCia
        AND CcbCDocu.FlgEst = "P"
        AND CcbCDocu.CodDoc = FacDocum.CodDoc 
        AND CcbCDocu.SdoAct = 0
        AND CcbCDocu.FchDoc >= fFchIni
        AND CcbCDocu.FchDoc <= fFchFin:
        ASSIGN CcbCDocu.FlgEst = "C".
        fFchCan = CcbCDocu.FchDoc.
        FOR EACH CcbDCaja NO-LOCK WHERE CcbDCaja.CodCia = CcbCDocu.CodCia
            AND CcbDCaja.CodRef = CcbCDocu.CodDoc
            AND CcbDCaja.NroRef = CcbCDocu.NroDoc 
            BY Ccbdcaja.FchDoc:
            fFchCan = Ccbdcaja.FchDoc.
        END.
        ASSIGN CcbCDocu.FchCan = fFchCan.
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
         HEIGHT             = 3.85
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


