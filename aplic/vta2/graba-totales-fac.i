&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
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



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 3.92
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

    DEFINE VARIABLE F-IGV AS DECIMAL NO-UNDO.
    DEFINE VARIABLE F-ISC AS DECIMAL NO-UNDO.

    ASSIGN
        Ccbcdocu.ImpDto = 0
        Ccbcdocu.ImpDto2 = 0
        Ccbcdocu.ImpIgv = 0
        Ccbcdocu.ImpIsc = 0
        Ccbcdocu.ImpTot = 0
        Ccbcdocu.ImpExo = 0
        F-IGV = 0
        F-ISC = 0.
    FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK:        
       F-Igv = F-Igv + Ccbddocu.ImpIgv.
       F-Isc = F-Isc + Ccbddocu.ImpIsc.
       Ccbcdocu.ImpTot = Ccbcdocu.ImpTot + Ccbddocu.ImpLin.
       IF NOT Ccbddocu.AftIgv THEN Ccbcdocu.ImpExo = Ccbcdocu.ImpExo + Ccbddocu.ImpLin.
       IF Ccbddocu.AftIgv = YES
       THEN Ccbcdocu.ImpDto = Ccbcdocu.ImpDto + ROUND(Ccbddocu.ImpDto / (1 + Ccbcdocu.PorIgv / 100), 2).
       ELSE Ccbcdocu.ImpDto = Ccbcdocu.ImpDto + Ccbddocu.ImpDto.
    END.
    ASSIGN
        Ccbcdocu.ImpIgv = ROUND(F-IGV,2)
        Ccbcdocu.ImpIsc = ROUND(F-ISC,2)
        Ccbcdocu.ImpVta = Ccbcdocu.ImpTot - Ccbcdocu.ImpExo - Ccbcdocu.ImpIgv
        Ccbcdocu.ImpBrt = Ccbcdocu.ImpVta + Ccbcdocu.ImpIsc + Ccbcdocu.ImpDto + Ccbcdocu.ImpExo
        Ccbcdocu.SdoAct  = Ccbcdocu.ImpTot.

    /* APLICAMOS EL IMPORTE POR FACTURA POR ADELANTOS */
    ASSIGN
        Ccbcdocu.SdoAct = Ccbcdocu.SdoAct - Ccbcdocu.ImpTot2.

    /*RDP 31.01.11 - Parche Tarjetas*/
    ASSIGN 
        CcbcDocu.Imptot = CcbcDocu.ImpTot - CcbcDocu.ImpDto2.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

