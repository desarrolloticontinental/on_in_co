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

DEF SHARED VAR s-codcia AS INTE.

/* Librerias

DEFINE VAR hProc AS HANDLE NO-UNDO.

RUN <libreria> PERSISTENT SET hProc.

RUN <libreria>.rutina_interna IN hProc (input  buffer tt-excel:handle,
                        /*input  session:temp-directory + "file"*/ c-xls-file,
                        output c-csv-file) .

run pi-crea-archivo-xls  IN hProc (input  buffer tt-excel:handle,
                        input  c-csv-file,
                        output c-xls-file) .

DELETE PROCEDURE hProc.

*/

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
         HEIGHT             = 5.42
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-web_api-captura-lineas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE web_api-captura-lineas Procedure 
PROCEDURE web_api-captura-lineas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* Sintaxis 
http://192.168.100.221:62/api/Sales/Record/Line/sha256$5uqVbpQ6dWv0z2Tw$8cfcfcd13571eba4873e06dd6dd25674503c498657f7da6969aae41a0249c64a?&Code=010
&Code=010 es opcional, si no se indica devuelve todas las l�neas
*/

DEF INPUT PARAMETER pCodFam AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

DEFINE VAR hDoc AS HANDLE NO-UNDO.
DEFINE VAR x-Url AS CHAR.
DEFINE VAR x-Xml AS LONGCHAR.
DEFINE VAR x-Texto AS CHAR CASE-SENSITIVE NO-UNDO.

FIND FIRST VtaTabla WHERE VtaTabla.CodCia = s-CodCia
    AND VtaTabla.Tabla = 'CONFIG-WEB-LINEAS' NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaTabla THEN DO:
    pMensaje = "NO se encontr� la configuraci�n CONFIG-WEB-LINEAS".
    RETURN "ADM-ERROR".
END.

CREATE X-DOCUMENT hDoc.

/* Barremos todas la LINEAS */
x-url = TRIM(VtaTabla.Llave_c1) + TRIM(VtaTabla.Llave_c2) + '?' + "FormatXML=1".

IF pCodFam > '' THEN x-Url = x-Url + "&Code=" + pCodFam.

hDoc:LOAD("File", x-url, FALSE) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    pMensaje = "NO se pudo cargar la informaci�n de la url:" + CHR(10) + x-url.
    RETURN "ADM-ERROR".
END.
hDoc:SAVE("LONGCHAR",x-Xml) NO-ERROR.

/* Separamos la DATA */
DEF VAR x-Inicio AS INTE NO-UNDO.
DEF VAR x-Fin AS INTE NO-UNDO.

x-Texto = STRING(x-Xml).

/* Barremos todo el texto */
DEF VAR x-Cadena AS CHAR NO-UNDO.

/* Limpiamos el texto */
DEF VAR x-Limite-Inicial AS CHAR CASE-SENSITIVE NO-UNDO.
DEF VAR x-Limite-Final AS CHAR CASE-SENSITIVE NO-UNDO.

x-Limite-Inicial = "<Data>".
x-Limite-Final   = "</Data>".
x-Inicio = INDEX(x-Texto, x-Limite-Inicial) + LENGTH(x-Limite-Inicial).
x-Fin = INDEX(x-Texto, x-Limite-Final).
x-Texto = SUBSTRING(x-Texto, x-Inicio , (x-Fin - x-Inicio)).

REPEAT:
    /* ************************************************************************* */
    /* Capturamos el primer bloque entre <Row> y </Row> */
    /* ************************************************************************* */
    x-Limite-Inicial = "<Row>".
    x-Limite-Final   = "</Row>".
    x-Inicio = INDEX(x-Texto, x-Limite-Inicial) + LENGTH(x-Limite-Inicial).
    x-Fin = INDEX(x-Texto, x-Limite-Final).
    IF x-Inicio > x-Fin THEN LEAVE.     /* Fin del proceso */
    x-Cadena = SUBSTRING(x-Texto, x-Inicio , (x-Fin - x-Inicio)).
    RUN web_api-captura-lineas-save (INPUT x-Cadena, OUTPUT pMensaje).
    IF RETURN-VALUE = 'ADM-ERROR' THEN RETURN 'ADM-ERROR'.
    /* ************************************************************************* */
    /* Recortamos la cadena nuevamente con lo que queda */
    /* ************************************************************************* */
    x-Cadena = "<Row>" + x-Cadena + "</Row>".
    x-Texto = SUBSTRING(x-Texto, INDEX(x-Texto, x-Cadena) + LENGTH(x-Cadena)).
END.

RETURN "OK".

END PROCEDURE.

/* ******************************** */
PROCEDURE web_api-captura-lineas-save:
/* ******************************** */

    DEF INPUT PARAMETER x-Cadena AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

    /* ************************************************************************* */
    /* Capturamos informacion de x-Cadena */
    /* ************************************************************************* */
    DEF VAR x-Limite-Inicial AS CHAR CASE-SENSITIVE NO-UNDO.
    DEF VAR x-Limite-Final AS CHAR CASE-SENSITIVE NO-UNDO.
    DEF VAR x-Inicio AS INTE NO-UNDO.
    DEF VAR x-Fin AS INTE NO-UNDO.
    DEF VAR x-Code AS CHAR NO-UNDO.
    DEF VAR x-Name AS CHAR NO-UNDO.
    DEF VAR x-ExchangeRate AS CHAR NO-UNDO.
    DEF VAR x-Timestamp_update AS CHAR NO-UNDO.

    DEF VAR pCuenta AS INTE NO-UNDO.

    x-Limite-Inicial = "<Code>".
    x-Limite-Final = "</Code>".
    x-Inicio = INDEX(x-Cadena, x-Limite-Inicial) + LENGTH(x-Limite-Inicial).
    x-Fin = INDEX(x-Cadena, x-Limite-Final).
    x-Code = SUBSTRING(x-cadena, x-Inicio , (x-Fin - x-Inicio)).

    x-Limite-Inicial = "<Name>".
    x-Limite-Final = "</Name>".
    x-Inicio = INDEX(x-Cadena, x-Limite-Inicial) + LENGTH(x-Limite-Inicial).
    x-Fin = INDEX(x-Cadena, x-Limite-Final).
    x-Name = SUBSTRING(x-cadena, x-Inicio , (x-Fin - x-Inicio)).

    x-Limite-Inicial = "<ExchangeRate>".
    x-Limite-Final = "</ExchangeRate>".
    x-Inicio = INDEX(x-Cadena, x-Limite-Inicial) + LENGTH(x-Limite-Inicial).
    x-Fin = INDEX(x-Cadena, x-Limite-Final).
    x-ExchangeRate = SUBSTRING(x-cadena, x-Inicio , (x-Fin - x-Inicio)).

    x-Limite-Inicial = "<Timestamp_update>".
    x-Limite-Final = "</Timestamp_update>".
    x-Inicio = INDEX(x-Cadena, x-Limite-Inicial) + LENGTH(x-Limite-Inicial).
    x-Fin = INDEX(x-Cadena, x-Limite-Final).
    x-Timestamp_update = SUBSTRING(x-cadena, x-Inicio , (x-Fin - x-Inicio)).

    /* Grabamos */
    FIND Almtfami WHERE Almtfami.CodCia = s-codcia AND
        Almtfami.codfam = x-Code
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almtfami THEN DO:
        CREATE Almtfami.
        ASSIGN
            Almtfami.CodCia = s-codcia 
            Almtfami.codfam = x-Code
            Almtfami.desfam = x-Name
            Almtfami.SwComercial = YES
            Almtfami.TpoCmb = DECIMAL(x-ExchangeRate)
            Almtfami.Libre_c05 = "NO"
            Almtfami.Libre_f02 = TODAY
            NO-ERROR.
        IF ERROR-STATUS:ERROR = YES THEN DO:
            {lib/mensaje-de-error.i &CuentaError="pCuenta" &MensajeError="pMensaje"}
            RETURN 'ADM-ERROR'.
        END.
    END.
    ELSE DO:
        FIND CURRENT Almtfami EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF ERROR-STATUS:ERROR = YES THEN DO:
            {lib/mensaje-de-error.i &CuentaError="pCuenta" &MensajeError="pMensaje"}
            RETURN 'ADM-ERROR'.
        END.
        ASSIGN
            Almtfami.TpoCmb = DECIMAL(x-ExchangeRate).
    END.
    /* Campos de control */
    ASSIGN
        Almtfami.Libre_c04 = x-Timestamp_update.
    RELEASE Almtfami.

    RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-web_api-pricing-preuni) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE web_api-pricing-preuni Procedure 
PROCEDURE web_api-pricing-preuni :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Devuelve el precio unitario ya afectado por el descuento x clf y x cond vta
------------------------------------------------------------------------------*/
/* Sintaxis
192.168.100.221:62/api/Sales/Record/PriceSalesChannelUnit/
sha256$5uqVbpQ6dWv0z2Tw$8cfcfcd13571eba4873e06dd6dd25674503c498657f7da6969aae41a0249c64a
?FormatXML=True&
FieldName=DiscountedPriceCategoryCustomerAndConditionSale,
ExchangeRate,                   /* Tipo de Cambio */
CurrencySale,                   /* Moneda de Venta */
PercentageDiscountSalesCondition,
PercentageDiscountCategoryCustomer,
CurrencyRepositionCost,         
AmountRepositionCost,
RepositionCostInCurrencySale    /* Costo de Reposici�n EN MONEDA DE VENTA */
&ArtCode=002056
&SalesChannel=1
&CategoryCustomer=A
&SalesCondition=001
&StateCategoryCustomer=1        /* Activo: obligatorio */
&StateSalesCondition=1          /* Activo: obligatorio */
*/

DEF INPUT PARAMETER pArtCode AS CHAR.
DEF INPUT PARAMETER pSalesChannel AS CHAR.
DEF INPUT PARAMETER pCategoryCustomer AS CHAR.
DEF INPUT PARAMETER pSalesCondition AS CHAR.
DEF OUTPUT PARAMETER pMonVta AS INTE NO-UNDO.
DEF OUTPUT PARAMETER pTpoCmb AS DECI NO-UNDO.
DEF OUTPUT PARAMETER pPrecioDescontado AS DECI NO-UNDO.
DEF OUTPUT PARAMETER pMessage AS CHAR NO-UNDO.

DEFINE VAR hDoc AS HANDLE NO-UNDO.
DEFINE VAR x-Url AS CHAR.
DEFINE VAR x-Xml AS LONGCHAR.
DEFINE VAR x-Texto AS CHAR.

FIND FIRST VtaTabla WHERE VtaTabla.CodCia = s-CodCia
    AND VtaTabla.Tabla = 'CONFIG-WEB-PRICING' NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaTabla THEN DO:
    pMessage = "NO se encontr� la configuraci�n CONFIG-WEB-PRICING".
    RETURN "ADM-ERROR".
END.

CREATE X-DOCUMENT hDoc.

x-url = TRIM(VtaTabla.Llave_c1) +       /* URL */
        TRIM(VtaTabla.Llave_c2) .       /* Token */

x-url = x-url + ~
"?FormatXML=True&~
FieldName=~
DiscountedPriceCategoryCustomerAndConditionSale,~
ExchangeRate,~
CurrencySale,~
PercentageDiscountSalesCondition,~
PercentageDiscountCategoryCustomer,~
CurrencyRepositionCost,~
AmountRepositionCost".

x-url = x-url + ~
'&ArtCode=' + pArtCode + ~
'&SalesChannel=' + pSalesChannel + ~
'&CategoryCustomer=' + pCategoryCustomer + ~
'&SalesCondition=' + pSalesCondition.
/*
x-url = x-url + ~
'&ArtCode=' + pArtCode + ~
'&SalesChannel=' + pSalesChannel + ~
'&CategoryCustomer=' + pCategoryCustomer + ~
'&SalesCondition=' + pSalesCondition + ~
'&StateCategoryCustomer=1' + ~
'&StateSalesCondition=1'.
*/


/* DEF VAR peditor AS CHAR VIEW-AS EDITOR SIZE 60 BY 6. */
/* peditor = x-url.                                     */
/* UPDATE peditor.                                      */


/* Reintentamos hasta 10 segundos */
DEF VAR Inicio AS INT64 NO-UNDO.
DEF VAR Fin AS INT64 NO-UNDO.

/* Inicio = ETIME(YES).        /* Valor 0 */         */
/* REPEAT:                                           */
/*     hDoc:LOAD("File", x-url, FALSE) NO-ERROR.     */
/*     IF ERROR-STATUS:ERROR = NO THEN DO:           */
/*         MESSAGE 'Conexi�n ok'.                    */
/*         LEAVE.                                    */
/*     END.                                          */
/*     Fin = ETIME.                                  */
/*     IF (Fin - Inicio) > 10000 THEN DO:            */
/*         pMessage = "Error en conexi�n de la URL". */
/*         RETURN 'ADM-ERROR'.                       */
/*     END.                                          */
/* END.                                              */

hDoc:LOAD("File", x-url, FALSE) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    pMessage = "NO se pudo cargar la informaci�n de la url" + CHR(10) + x-url.
    RETURN "ADM-ERROR".
END.
hDoc:SAVE("LONGCHAR",x-Xml) NO-ERROR.
IF ERROR-STATUS:ERROR = YES THEN MESSAGE ERROR-STATUS:GET-MESSAGE(1).

/* Buscamos la informaci�n */
/* Separamos la DATA */
DEF VAR x-Inicio AS INTE NO-UNDO.
DEF VAR x-Fin AS INTE NO-UNDO.

x-Texto = STRING(x-Xml).

MESSAGE 'URL:' x-url SKIP 'Texto:' x-Texto.

x-Inicio = INDEX(x-Texto, "<Data>").
x-Fin = INDEX(x-Texto, "</Data>").

IF x-Inicio = 0 OR x-Fin = 0 THEN DO:
    pMessage = "NO hay datos registrados".
    RETURN "ADM-ERROR".
END.

x-Texto = SUBSTRING(x-Texto, x-Inicio + 6, (x-Fin - x-Inicio - 6)).

/* **************************************************************************************** */
/* Buscamos error */
/* **************************************************************************************** */
IF INDEX(x-Texto, '<error>') > 0 THEN DO:
    pMessage = "ERROR: " + x-Texto + CHR(10) + "Comunicarse con el Jefe de L�nea".
    RETURN "ADM-ERROR".
    /* Solo regresamos sin ning�n error, no est� registrado el precio en la lista de precios */
    /*RETURN 'OK'.*/
END.

DEF VAR x-Cadena1 AS CHAR NO-UNDO.
DEF VAR x-Cadena2 AS CHAR NO-UNDO.

/* **************************************************************************************** */
/* Buscamos la moneda de venta */
/* **************************************************************************************** */
DEF VAR x-MonVta AS CHAR NO-UNDO.
DEF VAR x-Monedas AS CHAR INIT 'S/,$' NO-UNDO.

x-Cadena1 = "<CurrencySale>".
x-Cadena2 = "</CurrencySale>".

x-Inicio = INDEX(x-Texto, x-Cadena1).
x-Fin = INDEX(x-Texto, x-Cadena2).
IF x-Inicio > x-Fin THEN DO:
    pMessage = "Moneda de Venta: NO registrado en la lista de precios".
    RETURN "ADM-ERROR".
END.
x-MonVta = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

pMonVta = LOOKUP(x-MonVta, x-Monedas).
IF pMonVta = 0 THEN DO:
    pMessage = "Art�culo: " + pArtCode + CHR(10) +
                "Moneda de Venta no est� en el formato correcto" + CHR(10) + x-texto.
    RETURN "ADM-ERROR".
END.
/* **************************************************************************************** */
/* Tipo de cambio */
/* **************************************************************************************** */
DEF VAR x-TpoCmb AS CHAR NO-UNDO.

x-Cadena1 = "<ExchangeRate>".
x-Cadena2 = "</ExchangeRate>".

x-Inicio = INDEX(x-Texto, x-Cadena1).
x-Fin = INDEX(x-Texto, x-Cadena2).
IF x-Inicio > x-Fin THEN DO:
    pMessage = "Tipo de cambio: NO registrado en la lista de precios".
    RETURN "ADM-ERROR".
END.
x-TpoCmb = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

pTpoCmb = DECIMAL(x-TpoCmb) NO-ERROR.
IF ERROR-STATUS:ERROR = YES THEN DO:
    pMessage = "Art�culo: " + pArtCode + CHR(10) +
                "Tipo de Cambio no est� en el formato correcto" + CHR(10) + x-texto.
    RETURN "ADM-ERROR".
END.
/* **************************************************************************************** */
/* Buscamos el precio descontado */
/* **************************************************************************************** */
DEF VAR x-PrecioDescontado AS CHAR NO-UNDO.

x-Cadena1 = "<DiscountedPriceCategoryCustomerAndConditionSale>".
x-Cadena2 = "</DiscountedPriceCategoryCustomerAndConditionSale>".

x-Inicio = INDEX(x-Texto, x-Cadena1).
x-Fin = INDEX(x-Texto, x-Cadena2).
IF x-Inicio > x-Fin THEN DO:
    pMessage = "Precio Unitario Pelda�o: NO registrado en la lista de precios".
    RETURN "ADM-ERROR".
END.
x-PrecioDescontado = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

pPrecioDescontado = DECIMAL(x-PrecioDescontado) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    pMessage = "Precio Unitario Pelda�o no est� en formato de n�meros" + CHR(10) + x-texto.
    RETURN "ADM-ERROR".
END.

/* **************************************************************************************** */
/* Buscamos el costo de reposici�n */
/* **************************************************************************************** */
DEF VAR x-CostoReposicion AS CHAR NO-UNDO.
DEF VAR pCostoReposicion AS DECI NO-UNDO.

x-Cadena1 = "<RepositionCostInCurrencySale>".
x-Cadena2 = "</RepositionCostInCurrencySale>".

x-Inicio = INDEX(x-Texto, x-Cadena1).
x-Fin = INDEX(x-Texto, x-Cadena2).
IF x-Inicio > x-Fin THEN DO:
/*     pMessage = "Precio Unitario Pelda�o: NO registrado en la lista de precios". */
/*     RETURN "ADM-ERROR".                                                         */
END.
ELSE DO:
    x-CostoReposicion = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

    pCostoReposicion = DECIMAL(x-CostoReposicion) NO-ERROR.
    IF ERROR-STATUS:ERROR = NO THEN DO:
        IF pCostoReposicion <= pPrecioDescontado THEN DO:
            pMessage = "Precio Unitario Pelda�o es menor al Costo de Reposici�n".
            RETURN "ADM-ERROR".
        END.
    END.
END.

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

