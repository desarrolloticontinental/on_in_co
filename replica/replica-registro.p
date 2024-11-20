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
         HEIGHT             = 3.31
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DEFINE INPUT PARAMETER T-ID AS INTEGER.
DEFINE OUTPUT PARAMETER L-FLG AS LOGICAL.

L-FLG = NO.
FIND FIRST replog WHERE replog.TransactionID = T-ID NO-LOCK NO-ERROR.
IF AVAILABLE replog THEN DO:
    CASE replog.TableName:
        WHEN 'ccbcdocu' THEN DO:
            DISABLE TRIGGERS FOR LOAD OF base1.ccbcdocu.
            FIND base1.ccbcdocu WHERE base1.ccbcdocu.codcia = INTEGER(SUBSTRING(replog.keyvalue,1,3)) AND
                 base1.ccbcdocu.Coddiv = SUBSTRING(replog.keyvalue,4,5) AND
                 base1.ccbcdocu.coddoc = SUBSTRING(replog.keyvalue,9,3) AND
                 base1.ccbcdocu.nrodoc = SUBSTRING(replog.keyvalue,12)
                 EXCLUSIVE-LOCK NO-ERROR.
            CASE replog.Event:
                WHEN 'DELETE' THEN DO:
                    IF AVAILABLE base1.ccbcdocu THEN DO:
                       DELETE base1.ccbcdocu.
                       L-FLG = YES.    
                    END.
                END.
                WHEN 'WRITE' THEN DO:
                    IF NOT AVAILABLE base1.ccbcdocu THEN CREATE base1.ccbcdocu.
                    RAW-TRANSFER REPLOG.DATARECORD TO base1.ccbcdocu.
                    L-FLG = YES.
                END.
            END CASE.
            RELEASE base1.ccbcdocu.
        END.
        WHEN 'ccbddocu' THEN DO:
            DISABLE TRIGGERS FOR LOAD OF base1.ccbddocu.
            FIND base1.ccbddocu WHERE base1.ccbddocu.codcia = INTEGER(SUBSTRING(replog.keyvalue,1,3)) AND
                 base1.ccbddocu.Coddiv = SUBSTRING(replog.keyvalue,4,5) AND
                 base1.ccbddocu.coddoc = SUBSTRING(replog.keyvalue,9,3) AND
                 base1.ccbddocu.nrodoc = SUBSTRING(replog.keyvalue,12,15) AND
                 base1.ccbddocu.codmat = SUBSTRING(replog.keyvalue,27,6)
                 EXCLUSIVE-LOCK NO-ERROR.
            CASE replog.Event:
                WHEN 'DELETE' THEN DO:
                    IF AVAILABLE base1.ccbddocu THEN DO:
                       DELETE base1.ccbddocu.
                       L-FLG = YES.    
                    END.
                END.
                WHEN 'WRITE' THEN DO:
                    IF NOT AVAILABLE base1.ccbddocu THEN CREATE base1.ccbddocu.
                    RAW-TRANSFER REPLOG.DATARECORD TO base1.ccbddocu.
                    L-FLG = YES.
                END.
            END CASE.
            RELEASE base1.ccbddocu.
        END.
        WHEN 'ccbccaja' THEN DO:
            DISABLE TRIGGERS FOR LOAD OF base1.ccbccaja.
            FIND base1.ccbccaja WHERE base1.ccbccaja.codcia = INTEGER(SUBSTRING(replog.keyvalue,1,3)) AND
                 base1.ccbccaja.coddoc = SUBSTRING(replog.keyvalue,4,3) AND
                 base1.ccbccaja.nrodoc = SUBSTRING(replog.keyvalue,7)
                 EXCLUSIVE-LOCK NO-ERROR.
            CASE replog.Event:
                WHEN 'DELETE' THEN DO:
                    IF AVAILABLE base1.ccbccaja THEN DO:
                       DELETE base1.ccbccaja.
                       L-FLG = YES.    
                    END.
                END.
                WHEN 'WRITE' THEN DO:
                    IF NOT AVAILABLE base1.ccbccaja THEN CREATE base1.ccbccaja.
                    RAW-TRANSFER REPLOG.DATARECORD TO base1.ccbccaja.
                    L-FLG = YES.
                END.
            END CASE.
            RELEASE base1.ccbccaja.
        END.
        WHEN 'ccbdcaja' THEN DO:
            DISABLE TRIGGERS FOR LOAD OF base1.ccbdcaja.
            FIND base1.ccbdcaja WHERE base1.ccbdcaja.codcia = INTEGER(SUBSTRING(replog.keyvalue,1,3)) AND
                 base1.ccbdcaja.coddoc = SUBSTRING(replog.keyvalue,4,3) AND
                 base1.ccbdcaja.nrodoc = SUBSTRING(replog.keyvalue,7,15) AND
                 base1.ccbdcaja.codref = SUBSTRING(replog.keyvalue,22,3) AND
                 base1.ccbdcaja.nroref = SUBSTRING(replog.keyvalue,25)
                 EXCLUSIVE-LOCK NO-ERROR.
            CASE replog.Event:
                WHEN 'DELETE' THEN DO:
                    IF AVAILABLE base1.ccbdcaja THEN DO:
                       DELETE base1.ccbdcaja.
                       L-FLG = YES.    
                    END.
                END.
                WHEN 'WRITE' THEN DO:
                    IF NOT AVAILABLE base1.ccbdcaja THEN CREATE base1.ccbdcaja.
                    RAW-TRANSFER REPLOG.DATARECORD TO base1.ccbdcaja.
                    L-FLG = YES.
                END.
            END CASE.
            RELEASE base1.ccbdcaja.
        END.
        WHEN 'ccbcmvto' THEN DO:
            DISABLE TRIGGERS FOR LOAD OF base1.ccbcmvto.
            FIND base1.ccbcmvto WHERE base1.ccbcmvto.codcia = INTEGER(SUBSTRING(replog.keyvalue,1,3)) AND
                 base1.ccbcmvto.coddoc = SUBSTRING(replog.keyvalue,4,3) AND
                 base1.ccbcmvto.nrodoc = SUBSTRING(replog.keyvalue,7)
                 EXCLUSIVE-LOCK NO-ERROR.
            CASE replog.Event:
                WHEN 'DELETE' THEN DO:
                    IF AVAILABLE base1.ccbccaja THEN DO:
                       DELETE base1.ccbcmvto.
                       L-FLG = YES.    
                    END.
                END.
                WHEN 'WRITE' THEN DO:
                    IF NOT AVAILABLE base1.ccbcmvto THEN CREATE base1.ccbcmvto.
                    RAW-TRANSFER REPLOG.DATARECORD TO base1.ccbcmvto.
                    L-FLG = YES.
                END.
            END CASE.
            RELEASE base1.ccbcmvto.
        END.
        WHEN 'ccbdmvto' THEN DO:
            DISABLE TRIGGERS FOR LOAD OF base1.ccbdmvto.
            FIND base1.ccbdmvto WHERE base1.ccbdmvto.codcia = INTEGER(SUBSTRING(replog.keyvalue,1,3)) AND
                 base1.ccbdmvto.coddoc = SUBSTRING(replog.keyvalue,4,3) AND
                 base1.ccbdmvto.nrodoc = SUBSTRING(replog.keyvalue,7,15) AND
                 base1.ccbdmvto.codref = SUBSTRING(replog.keyvalue,22,3) AND
                 base1.ccbdmvto.nroref = SUBSTRING(replog.keyvalue,25)
                 EXCLUSIVE-LOCK NO-ERROR.
            CASE replog.Event:
                WHEN 'DELETE' THEN DO:
                    IF AVAILABLE base1.ccbdmvto THEN DO:
                       DELETE base1.ccbdmvto.
                       L-FLG = YES.    
                    END.
                END.
                WHEN 'WRITE' THEN DO:
                    IF NOT AVAILABLE base1.ccbdmvto THEN CREATE base1.ccbdmvto.
                    RAW-TRANSFER REPLOG.DATARECORD TO base1.ccbdmvto.
                    L-FLG = YES.
                END.
            END CASE.
            RELEASE base1.ccbdmvto.
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


