&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
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
/* Definimos Variables de impresoras */
DEFINE VARIABLE s-printer-list  AS CHAR NO-UNDO.
DEFINE VARIABLE s-port-list     AS CHAR NO-UNDO.
DEFINE NEW SHARED VARIABLE s-port-name     AS CHAR format "x(20)".
DEFINE VARIABLE s-printer-count AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE s-pagina-final     AS INTEGER.
DEFINE NEW SHARED VARIABLE s-pagina-inicial   AS INTEGER.
DEFINE NEW SHARED VARIABLE s-salida-impresion AS INTEGER.
DEFINE NEW SHARED VARIABLE s-printer-name     AS CHARACTER.
DEFINE NEW SHARED VARIABLE s-print-file       AS CHARACTER.
DEFINE NEW SHARED VARIABLE s-nro-copias       AS INTEGER.
DEFINE NEW SHARED VARIABLE s-orientacion      AS INTEGER.

DEFINE SHARED VARIABLE s-OpSys AS CHAR.

DEFINE STREAM REPORT.

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
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

&GLOBAL-DEFINE PRN0  CHR(27) + CHR(64)           /* INICIALIZA LA IMPRESORA */
&GLOBAL-DEFINE PRN1  CHR(18) + CHR(27) + CHR(80) /* CANCELA MODO COND. Y SELEC. 10 CPI */
&GLOBAL-DEFINE PRN2  CHR(18) + CHR(27) + CHR(77) /* CANCELA MODO COND. Y SELEC. 12 CPI */
&GLOBAL-DEFINE PRN3  CHR(27) + CHR(80) + CHR(15) /* SELECCIONA 17 CPI */
&GLOBAL-DEFINE PRN4  CHR(27) + CHR(77) + CHR(15) /* SELECCIONA 20 CPI */
&GLOBAL-DEFINE PRN5A CHR(27) + CHR(67)           /* SELECCIONA LONGITUD DE PAGINA */
&GLOBAL-DEFINE PRN5B                             /* SELECCIONA LONGITUD DE PAGINA */
&GLOBAL-DEFINE PRN6A CHR(27) + CHR(71)           /* SELECCIONA DOBLE GOLPE    */
&GLOBAL-DEFINE PRN6B CHR(27) + CHR(72)           /* CANCELA DOBLE GOLPE       */
&GLOBAL-DEFINE PRN7A CHR(14)                     /* SELECCIONA MODO EXPANDIDO */
&GLOBAL-DEFINE PRN7B CHR(20)                     /* CANCELA MODO EXPANDIDO  */
&GLOBAL-DEFINE PRN8A CHR(27) + CHR(48)           /* SELECCIONA 88 LLP (11") */
&GLOBAL-DEFINE PRN8B CHR(27) + CHR(50)           /* SELECCIONA 66 LLP (11") */
&GLOBAL-DEFINE PRN9A CHR(27) + CHR(119) + '1'    /* SELECCIONA DOBLE ALTURA */
&GLOBAL-DEFINE PRN9B CHR(27) + CHR(119) + '0'    /* CANCELA DOBLE ALTURA    */
&GLOBAL-DEFINE PRN0A CHR(27) + CHR(14)           /* SELECCIONA DOBLE ANCHO */
&GLOBAL-DEFINE PRN0B CHR(20)                     /* CANCELA DOBLE ANCHO   */
&GLOBAL-DEFINE PRNEA CHR(27) + CHR(69)           /* SELECCIONA EMPHASIZED MODE */
&GLOBAL-DEFINE PRNEB CHR(27) + CHR(70)           /* CANCELA EMPHASIZED MODE  */
&GLOBAL-DEFINE PRND  CHR(27) + CHR(120) + '0'    /* SELECCIONA DRAFT MODE   */
&GLOBAL-DEFINE PRNN  CHR(27) + CHR(120) + '1'    /* SELECCIONA NLQ MODE */
&GLOBAL-DEFINE PRNR  CHR(27) + CHR(107) + '0'    /* SELECCIONA ROMAN      */
&GLOBAL-DEFINE PRNS  CHR(27) + CHR(107) + '1'    /* SELECCIONA SANS SERIF */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


