&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
/* Procedure Description
"Entrega un numero en su forma literal.

Formato:
             RUN _numero.p (INPUT <DECIMAL1>, <INTEGER2>, <INTEGER3> OUTPUT <VARIABLE>)
 
      donde:
             DECIMAL1  : Numero a convertir
             INTEGER2  : Numero de decimales 
             INTEGER3  : Presentacion            1 Mayusculas, 2 Minusculas, 3 Propio.
            
             VARIABLE  : Variable de tipo CHARACTER donde graba el valor de retorno.        

"
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
&IF "{&NEW}" = "" &THEN
    DEFINE INPUT  PARAMETER Numero    AS DECIMAL.
    DEFINE INPUT  PARAMETER Decimales AS INTEGER.
    DEFINE OUTPUT PARAMETER X-RPTA    AS DECIMAL.

&ELSE
    DEFINE VARIABLE Numero    AS DECIMAL INITIAL 1001.
    DEFINE VARIABLE Decimales AS INTEGER INITIAL 2.
    DEFINE VARIABLE X-RPTA  AS DECIMAL.
    
&ENDIF
DEFINE VARIABLE X-DIFE  AS DECIMAL.

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
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
IF Decimales < 0 THEN Decimales = 0.
IF Decimales > 6 THEN Decimales = 6.

/* bloqueado el 13.08.10
X-DIFE = ROUND(Numero,Decimales) - Numero.
IF X-DIFE < 0 THEN X-RPTA = ROUND(Numero,Decimales) + ( 1 / EXP(10,Decimales)).
IF X-DIFE > 0 THEN X-RPTA = ROUND(Numero,Decimales) .
IF X-DIFE = 0 THEN X-RPTA = Numero .
*/

X-RPTA = ROUND(Numero,Decimales) .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


