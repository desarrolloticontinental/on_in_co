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
DEF VAR x-servidor-ip AS CHAR.
DEF VAR x-servidor-Puerto AS CHAR.

DEFINE VAR x-articulo AS CHAR.
DEFINE VAR x-peldanio AS CHAR.
DEFINE VAR x-ctg AS CHAR.
DEFINE VAR x-cndvta AS CHAR.
DEFINE VAR x-url AS CHAR.

x-servidor-ip = "192.168.100.198:621".
x-servidor-Puerto = "621".
x-articulo = "019050".
x-peldanio = "6".
x-ctg = "C".
x-cndvta = "000".

/* Ambas funcionan */
/*
x-URL   = "http://" + x-servidor-ip + ":" + x-servidor-Puerto + "/einvoice/rest/" + x-Tipo-Doc-Emisor + "/" + 
                    x-Ruc-emisor + "/" + x-Tipo-Documento-Sunat + "/" + x-serie-numero.
*/
x-URL   = "http://" + x-servidor-ip + ":" + x-servidor-Puerto + "/api/storeprocedure/XML/" +
            x-articulo + "/" + x-peldanio + "/" + x-ctg + "/" + x-cndvta.

/* El contenido de la web */
define var v-result as char no-undo.
define var v-response as LONGCHAR no-undo.
define var v-content as LONGCHAR no-undo.
define var cTexto as LONGCHAR no-undo.

DEFINE VAR cValueTag AS CHAR.

DEFINE VAR cTagInicial AS CHAR.
DEFINE VAR cTagFinal AS CHAR.

RUN lib/http-get-contenido.r(x-Url,output v-result,output v-response,output v-content) NO-ERROR.
IF v-result = "1:Success"  THEN DO:

END.

MESSAGE "RESULTADO FINAL ==" SKIP(2)
        "URL " x-url SKIP(2)
        "v-result " v-result SKIP(2)
        "v-response " string(v-response) SKIP(2)
        "v-content " string(v-content).

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


