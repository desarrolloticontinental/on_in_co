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
         HEIGHT             = 4.62
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* Consistencia del registro de ventas */
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR x-fchdoc AS DATE NO-UNDO.
DEFINE VARIABLE F-IGV AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-ISC AS DECIMAL NO-UNDO.
DEFINE VARIABLE x-Dto2xExonerados AS DEC NO-UNDO.
DEFINE VARIABLE x-Dto2xAfectosIgv AS DEC NO-UNDO.

DEF VAR x-ImpDto AS DEC NO-UNDO.
DEF VAR x-ImpIgv AS DEC NO-UNDO.
DEF VAR x-ImpIsc AS DEC NO-UNDO.
DEF VAR x-ImpTot AS DEC NO-UNDO.
DEF VAR x-ImpExo AS DEC NO-UNDO.
DEF VAR x-ImpVta AS DEC NO-UNDO.
DEF VAR x-ImpBrt AS DEC NO-UNDO.
DEF VAR x-ImpDto2 AS DEC NO-UNDO.

x-fchdoc = TODAY - 1.   /* dia de ayer */

DEFINE VARIABLE x-Archivo AS CHARACTER   NO-UNDO.
DEFINE STREAM REPORT.

x-Archivo = 'Y:\doc-rev'+ 
    STRING(DAY(x-fchdoc),'99') + STRING(MONTH(x-fchdoc),'99') + STRING(YEAR(x-fchdoc),'9999') + '.txt'.
OUTPUT STREAM REPORT TO VALUE(x-Archivo).
PUT STREAM REPORT UNFORMATTED
    'DIVISION|CODC|NUMERO|IMP BRUTO|X-IMPBRT|IMP EXO|X-IMPEXO|IMP VTA|X-IMPVTA|IMP IGV|X-IMPIGV'
    '|POR IGV|IMP TOTAL|X-IMPTOT|%DTO|IMP DTO2|X-IMPDTO2'
    SKIP.

FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia,
    EACH ccbcdocu OF gn-divi NO-LOCK WHERE LOOKUP(ccbcdocu.coddoc, 'FAC,BOL,N/C,N/D,TCK') > 0
    AND ccbcdocu.fchdoc = x-fchdoc
    AND ccbcdocu.flgest <> 'A':
    ASSIGN
      x-ImpDto = 0
      x-ImpDto2 = 0
      x-ImpIgv = 0
      x-ImpIsc = 0
      x-ImpTot = 0
      x-ImpExo = 0
      F-IGV = 0
      F-ISC = 0
      x-Dto2xExonerados = 0
      x-Dto2xAfectosIgv = 0.
    FOR EACH CcbDDocu OF CcbCDocu NO-LOCK:
        ASSIGN
            F-Igv = F-Igv + CcbDDocu.ImpIgv
            F-Isc = F-Isc + CcbDDocu.ImpIsc
            x-ImpTot = x-ImpTot + CcbDDocu.ImpLin
            x-ImpDto2 = x-ImpDto2 + CcbDDocu.ImpDto2.
        /* Importe Inafecto o Exonerado */
        IF CcbDDocu.ImpIgv = 0 THEN ASSIGN x-ImpExo = x-ImpExo + CcbDDocu.ImpLin x-Dto2xExonerados = x-Dto2xExonerados + CcbDDocu.ImpDto2.
        ELSE ASSIGN x-Dto2xAfectosIgv = x-Dto2xAfectosIgv + CcbDDocu.ImpDto2.
    END.
    ASSIGN
        x-ImpIgv = ROUND(F-IGV,2)
        x-ImpIsc = ROUND(F-ISC,2)
        x-ImpVta = ROUND( (x-ImpTot - x-ImpExo) / (1 + CcbCDocu.PorIgv / 100), 2)
        x-ImpBrt = x-ImpVta  + x-ImpDto.
    IF Ccbcdocu.PorIgv = 0
        THEN ASSIGN
            x-ImpVta = x-ImpExo
            x-ImpBrt = x-ImpExo.
    IF CcbCDocu.ImpDto2 > 0 THEN DO:
        IF x-ImpExo = 0 THEN 
            ASSIGN
            x-Dto2xExonerados = 0
            x-Dto2xAfectosIgv = CcbCDocu.ImpDto2.
        ASSIGN
            x-ImpTot = x-ImpTot - CcbCDocu.ImpDto2
            x-ImpIgv = x-ImpIgv -  ~
            ROUND(x-Dto2xAfectosIgv / ( 1 + CcbCDocu.PorIgv / 100) * CcbCDocu.PorIgv / 100, 2)
            x-ImpExo = x-ImpExo - x-Dto2xExonerados
            x-ImpVta = x-ImpTot - x-ImpExo - x-ImpIgv
            x-ImpBrt = x-ImpVta + x-ImpDto.
    END.
    IF x-ImpExo = 0 THEN x-ImpIgv = x-ImpTot - x-ImpVta.
    ELSE x-ImpIgv = ROUND(x-ImpVta * CcbCDocu.PorIgv / 100, 2).
    ASSIGN
        x-ImpBrt = x-ImpVta + x-ImpDto + Ccbcdocu.Libre_d01.  /* Dcto Lista Express SIN IGV */

    IF CcbCDocu.PorIgv = 0.00     /* VENTA INAFECTA */
        THEN ASSIGN
            x-ImpIgv = 0
            x-ImpVta = x-ImpExo
            x-ImpBrt = x-ImpExo.
    IF ABS(Ccbcdocu.impbrt - x-impbrt) > 0.1
        OR ABS(ccbcdocu.impexo - x-impexo) > 0.1
        OR ABS(ccbcdocu.impvta - x-impvta) > 0.1
        OR ABS(ccbcdocu.impigv - x-impigv) > 0.1
        OR ABS(ccbcdocu.imptot - x-imptot) > 0.1
        OR ABS(ccbcdocu.impdto2 - x-impdto2) > 0.1
        THEN DO:
        PUT STREAM REPORT UNFORMATTED
            ccbcdocu.coddiv '|'
            ccbcdocu.coddoc '|'
            ccbcdocu.nrodoc '|'
            ccbcdocu.impbrt '|'
            x-impbrt '|'
            ccbcdocu.impexo '|'
            x-impexo '|'
            ccbcdocu.impvta '|'
            x-impvta '|'
            ccbcdocu.impigv '|'
            x-impigv '|'
            ccbcdocu.porigv '|'
            ccbcdocu.imptot '|'
            x-imptot '|'
            ccbcdocu.pordto '|'
            ccbcdocu.impdto2 '|'
            x-impdto2
            SKIP.
    END.

/*     IF ABS(Ccbcdocu.impbrt - x-impbrt) > 0.1       */
/*         OR ABS(ccbcdocu.impexo - x-impexo) > 0.1   */
/*         OR ABS(ccbcdocu.impvta - x-impvta) > 0.1   */
/*         OR ABS(ccbcdocu.impigv - x-impigv) > 0.1   */
/*         OR ABS(ccbcdocu.imptot - x-imptot) > 0.1   */
/*         OR ABS(ccbcdocu.impdto2 - x-impdto2) > 0.1 */
/*         THEN DISPLAY                               */
/*         ccbcdocu.coddiv                            */
/*         ccbcdocu.coddoc                            */
/*         ccbcdocu.nrodoc                            */
/*         ccbcdocu.pordto                            */
/*         ccbcdocu.impdto2 x-impdto2                 */
/*         ccbcdocu.impbrt x-impbrt                   */
/*         ccbcdocu.impexo x-impexo                   */
/*         ccbcdocu.impvta x-impvta                   */
/*         ccbcdocu.impigv x-impigv                   */
/*         ccbcdocu.imptot x-imptot                   */
/*         WITH 2 COL WITH STREAM-IO.                 */

END.

OUTPUT STREAM REPORT CLOSE.

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


