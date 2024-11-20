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
DEF SHARED VAR s-user-id AS CHAR.
/* DEF SHARED VAR s-aplic-id AS CHAR.  */
/* DEF SHARED VAR s-prog-name AS CHAR. */

DEFINE TEMP-TABLE pTable LIKE w-report.

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
         HEIGHT             = 15.88
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

&IF DEFINED(EXCLUDE-web_api-captura-peldano-valido) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE web_api-captura-peldano-valido Procedure 
PROCEDURE web_api-captura-peldano-valido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Sintaxis 
http://192.168.100.221:64/api/Pricing/PartOfThePriceLadder/00502/xml
*/

DEF INPUT PARAMETER pCodDiv AS CHAR NO-UNDO.

DEF OUTPUT PARAMETER pEstadoValido AS LOG NO-UNDO.
DEF OUTPUT PARAMETER pSalesChannel AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER pMessage AS CHAR NO-UNDO.

pEstadoValido = YES.

DEFINE VAR hDoc AS HANDLE NO-UNDO.
DEFINE VAR x-Url AS CHAR.
DEFINE VAR x-Xml AS LONGCHAR.
DEFINE VAR x-Texto AS CHAR CASE-SENSITIVE NO-UNDO.

FIND FIRST VtaTabla WHERE VtaTabla.CodCia = s-CodCia
    AND VtaTabla.Tabla = 'CONFIG-WEB-PELDANO-VALIDO' NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaTabla THEN DO:
    pMessage = "NO se encontr� la configuraci�n CONFIG-WEB-PELDANO-VALIDO".
    RETURN "ADM-ERROR".
END.

CREATE X-DOCUMENT hDoc.

/* Barremos todas la LINEAS */
x-url = TRIM(VtaTabla.Llave_c1) + TRIM(pCodDiv) + '/xml'.

/* Capturamos el API */
DEFINE VAR x-oXmlHttp AS COM-HANDLE NO-UNDO.

CREATE "MSXML2.ServerXMLHTTP.6.0" x-oXmlHttp.

x-oXmlHttp:OPEN( "GET", x-Url, NO ). 
x-oXmlHttp:setRequestHeader( "Content-Type", "application/xml;charset=utf-8" ).
x-oXmlHttp:setOption( 2, 13056 ) .  
x-oXmlHttp:SEND() NO-ERROR.

IF ERROR-STATUS:GET-NUMBER(1) > 0 THEN DO:
    pMessage = "Se detecta demasiadas peticiones de uso del servidor de aplicaciones," + CHR(10) +
        "Vuelva a intentar y de persistir el mismo mensaje, contactar a Sistemas" + CHR(10) +
        TRIM(x-Url).
    RUN web_connection_log (x-Url, ERROR-STATUS:GET-MESSAGE(1)).
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN 'ADM-ERROR'.
END.
IF x-oXmlHttp:STATUS <> 200 THEN DO:
    pMessage = "ERROR AL ENVIAR TRAMA : " + CHR(10) + x-oXmlHttp:responseText.
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN 'ADM-ERROR'.
END.

/* Respuesta del API */
x-Xml = x-oXmlHttp:responseText.
x-Texto = STRING(x-Xml).

DEF VAR Inicio AS INT64 NO-UNDO.
DEF VAR Fin AS INT64 NO-UNDO.

/* Separamos la DATA */
DEF VAR x-Inicio AS INTE NO-UNDO.
DEF VAR x-Fin AS INTE NO-UNDO.

x-Inicio = INDEX(x-Texto, "<Data>").
x-Fin = INDEX(x-Texto, "</Data>").

IF x-Inicio = 0 OR x-Fin = 0 THEN DO:
    pMessage = "XML no retorn� ninguna informaci�n" + CHR(10) +
        "Vuelva a intentar y de persistir el mismo mensaje, contactar a Sistemas".
    RUN web_connection_log (x-Url, "XML no retorn� ninguna informaci�n").
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.

x-Texto = SUBSTRING(x-Texto, x-Inicio + 6, (x-Fin - x-Inicio - 6)).

DEF VAR x-Cadena1 AS CHAR NO-UNDO.
DEF VAR x-Cadena2 AS CHAR NO-UNDO.

/* **************************************************************************************** */
/* Buscamos error */
/* **************************************************************************************** */
DEF VAR x-Error AS CHAR NO-UNDO.
IF INDEX(x-Texto, '<error>') > 0 THEN DO:
    x-Cadena1 = "<error>".
    x-Cadena2 = "</error>".
    x-Inicio = INDEX(x-Texto, x-Cadena1).
    x-Fin = INDEX(x-Texto, x-Cadena2).
    IF x-Inicio < x-Fin THEN x-Error = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).
    pMessage = "VALIDACION: Divisi�n sin configuraci�n de Pelda�o" + CHR(10) + 
        "Divisi�n: " + pCodDiv + CHR(10) + CHR(10) +
        "Comunicarse con el Jefe de L�nea para su diagn�stico previo" + CHR(10) +
        "y de ser necesario se comunicar� con sistemas".
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.

/* **************************************************************************************** */
/* Escalera de precios */
/* **************************************************************************************** */
DEF VAR x-Estado AS CHAR NO-UNDO.
DEF VAR x-Estados AS CHAR INIT '0,1' NO-UNDO.
DEF VAR pEstado AS INTE NO-UNDO.

x-Cadena1 = "<ParteDeLaEscaleraPrecios>".
x-Cadena2 = "</ParteDeLaEscaleraPrecios>".

x-Inicio = INDEX(x-Texto, x-Cadena1).
x-Fin = INDEX(x-Texto, x-Cadena2).
IF x-Inicio > x-Fin THEN DO:
    pMessage = "No hay dato de valides del pelda�o".
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
x-Estado = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

pEstado = LOOKUP(x-Estado, x-Estados).
IF pEstado = 0 THEN DO:
    pMessage = "Divisi�n: " + pCodDiv + CHR(10) +
                "Respuesta no est� en el formato correcto" + CHR(10) + x-texto.
    RETURN "ADM-ERROR".
END.

IF x-Estado = "0" THEN pEstadoValido = NO.       /* No pertenece al pelda�o de precios */
IF x-Estado = "1" THEN pEstadoValido = YES.      /* S� pertenece al pelda�o de precios */

/* PELDA�O */
x-Cadena1 = "<PeldanoPrecios>".
x-Cadena2 = "</PeldanoPrecios>".

x-Inicio = INDEX(x-Texto, x-Cadena1).
x-Fin = INDEX(x-Texto, x-Cadena2).
IF x-Inicio > x-Fin THEN DO:
    pMessage = "No hay datos del pelda�o".
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
pSalesChannel = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

RELEASE OBJECT x-oXmlHttp NO-ERROR.

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-web_api-pricing-ctoreposicion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE web_api-pricing-ctoreposicion Procedure 
PROCEDURE web_api-pricing-ctoreposicion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Devuelve el costo de reposicion a la moneda de venta
------------------------------------------------------------------------------*/

/* Sintaxis
http://192.168.0.237:62/api/storeprocedure/XML
/002056
/x    <<< OJO <<<
/C
/000
*/
DEF INPUT PARAMETER pCodDiv  AS CHAR.
DEF INPUT PARAMETER pArtCode AS CHAR.
DEF INPUT PARAMETER pCategoryCustomer AS CHAR.
DEF INPUT PARAMETER pSalesCondition AS CHAR.
DEF OUTPUT PARAMETER pMonVta AS INTE NO-UNDO.
DEF OUTPUT PARAMETER pTpoCmb AS DECI NO-UNDO.
DEF OUTPUT PARAMETER pRepositionCostInCurrencySale AS DECI NO-UNDO.
DEF OUTPUT PARAMETER pMessage AS CHAR NO-UNDO.

DEF VAR pSalesChannel AS CHAR INIT '6' NO-UNDO.     /* TIENDAS MAYORISTAS */

IF pCodDiv  > '' THEN DO:
    FIND gn-divi WHERE gn-divi.codcia = s-codcia AND gn-divi.coddiv = pCodDiv NO-LOCK NO-ERROR.
    IF AVAILABLE gn-div AND GN-DIVI.Grupo_Divi_GG > "" 
        THEN pSalesChannel = TRIM(STRING(INTEGER(GN-DIVI.Grupo_Divi_GG))).
END.

DEFINE VAR x-Url AS LONGCHAR.
DEFINE VAR x-Xml AS LONGCHAR.
DEFINE VAR x-Texto AS CHAR.

FIND FIRST VtaTabla WHERE VtaTabla.CodCia = s-CodCia
    AND VtaTabla.Tabla = 'CONFIG-WEB-PRICING' NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaTabla THEN DO:
    pMessage = "NO se encontr� la configuraci�n CONFIG-WEB-PRICING".
    RETURN "ADM-ERROR".
END.

/* Capturamos URL y TOKEN */
x-Url = TRIM(VtaTabla.Llave_c1) +       /* URL */
        TRIM(VtaTabla.Llave_c2) .       /* Token */

/* Llave de b�squeda */
/* Llave de b�squeda */
x-Url = x-Url + ~
'/' + pArtCode + ~
'/' + pSalesChannel + ~
'/' + pCategoryCustomer + ~
'/' + pSalesCondition.

/* DEF VAR x-editor AS CHAR VIEW-AS EDITOR SIZE 60 BY 6. */
/* x-editor = string(x-url).                             */
/* UPDATE x-editor.                                      */
/* RETURN.                                               */

/* Capturamos el API */
DEFINE VAR x-oXmlHttp AS COM-HANDLE NO-UNDO.

CREATE "MSXML2.ServerXMLHTTP.6.0" x-oXmlHttp.

x-oXmlHttp:OPEN( "GET", x-Url, NO ). 
x-oXmlHttp:setRequestHeader( "Content-Type", "application/xml;charset=utf-8" ).
x-oXmlHttp:setOption( 2, 13056 ) .  
x-oXmlHttp:SEND() NO-ERROR.

IF ERROR-STATUS:GET-NUMBER(1) > 0 THEN DO:
    pMessage = "Se detecta demasiadas peticiones de uso del servidor de aplicaciones (100.198)," + CHR(10) +
        "Vuelva a intentar y de persistir el mismo mensaje, contactar a Sistemas"  + CHR(10) +
        TRIM(x-Url).
    RUN web_connection_log (x-Url, ERROR-STATUS:GET-MESSAGE(1)).
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN 'ADM-ERROR'.
END.
IF x-oXmlHttp:STATUS <> 200 THEN DO:
    pMessage = "ERROR AL ENVIAR TRAMA : " + CHR(10) + x-oXmlHttp:responseText.
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN 'ADM-ERROR'.
END.
/* Respuesta del API */
x-Xml = x-oXmlHttp:responseText.

x-Texto = STRING(x-Xml).

/* **************************************************************************************** */
/* Buscamos error */
/* **************************************************************************************** */
DEF VAR x-Cadena1 AS CHAR NO-UNDO.
DEF VAR x-Cadena2 AS CHAR NO-UNDO.
DEF VAR x-Inicio AS INTE NO-UNDO.
DEF VAR x-Fin AS INTE NO-UNDO.

IF INDEX(x-Texto, '<error>') > 0 THEN DO:
    x-Cadena1 = "<error>".
    x-Cadena2 = "</error>".
    x-Inicio = INDEX(x-Texto, x-Cadena1).
    x-Fin = INDEX(x-Texto, x-Cadena2).
    IF x-Inicio > x-Fin THEN DO:
        pMessage = "VALIDACION: Precio Pelda�o Bruto NO disponible para la venta" + CHR(10) + 
            "Porque NO est� configurado el precio pelda�o bruto o est� configurado con margen negativo" + CHR(10) + 
            "NO disponible para la venta" + CHR(10) + 
            "Art�culo: " + pArtCode + CHR(10) +
            "Comunicarse con el Jefe de L�nea".
    END.
    ELSE DO:
        pMessage = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).
        pMessage = pMessage + CHR(10) + 
            "Porque NO est� configurado el precio pelda�o bruto o est� configurado con margen negativo" + CHR(10) + 
            "NO disponible para la venta" + CHR(10) + 
            "Art�culo: " + pArtCode + CHR(10) +
            "Comunicarse con el Jefe de L�nea".
    END.
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
/* **************************************************************************************** */

DEF VAR Inicio AS INT64 NO-UNDO.
DEF VAR Fin AS INT64 NO-UNDO.

/* Separamos la DATA */
x-Inicio = INDEX(x-Texto, "<Data>").
x-Fin = INDEX(x-Texto, "</Data>").

IF x-Inicio = 0 OR x-Fin = 0 THEN DO:
    pMessage = "XML no retorn� ninguna informaci�n" + CHR(10) +
        "Vuelva a intentar y de persistir el mismo mensaje, contactar a Sistemas".
    RUN web_connection_log (x-Url, "XML no retorn� ninguna informaci�n").
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.

x-Texto = SUBSTRING(x-Texto, x-Inicio + 6, (x-Fin - x-Inicio - 6)).

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
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
x-MonVta = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

pMonVta = LOOKUP(x-MonVta, x-Monedas).
IF pMonVta = 0 THEN DO:
    pMessage = "Art�culo: " + pArtCode + CHR(10) +
                "Moneda de Venta no est� en el formato correcto" + CHR(10) + x-texto.
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
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
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
x-TpoCmb = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

pTpoCmb = DECIMAL(x-TpoCmb) NO-ERROR.
IF ERROR-STATUS:ERROR = YES THEN DO:
    pMessage = "Art�culo: " + pArtCode + CHR(10) +
                "Tipo de Cambio no est� en el formato correcto" + CHR(10) + x-texto.
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
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
    pMessage = "Costo de Reposici�n: NO registrado en la lista de precios".
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
x-CostoReposicion = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

pRepositionCostInCurrencySale = DECIMAL(x-CostoReposicion) NO-ERROR.
IF ERROR-STATUS:ERROR = YES THEN DO:
    pMessage = "Art�culo: " + pArtCode + CHR(10) +
                "Costo de Reposici�n no est� en el formato correcto" + CHR(10) + x-texto.
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.

RELEASE OBJECT x-oXmlHttp NO-ERROR.

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-web_api-pricing-ctoreposicion-Old) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE web_api-pricing-ctoreposicion-Old Procedure 
PROCEDURE web_api-pricing-ctoreposicion-Old :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Devuelve el costo de reposicion a la moneda de venta
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
DEF INPUT PARAMETER pCategoryCustomer AS CHAR.
DEF INPUT PARAMETER pSalesCondition AS CHAR.
DEF OUTPUT PARAMETER pMonVta AS INTE NO-UNDO.
DEF OUTPUT PARAMETER pTpoCmb AS DECI NO-UNDO.
DEF OUTPUT PARAMETER pRepositionCostInCurrencySale AS DECI NO-UNDO.
DEF OUTPUT PARAMETER pMessage AS CHAR NO-UNDO.

/*MESSAGE partcode psaleschannel pCategoryCustomer pSalesCondition.*/

DEFINE VAR x-Url AS LONGCHAR.
DEFINE VAR x-Xml AS LONGCHAR.
DEFINE VAR x-Texto AS CHAR.

FIND FIRST VtaTabla WHERE VtaTabla.CodCia = s-CodCia
    AND VtaTabla.Tabla = 'CONFIG-WEB-PRICING' NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaTabla THEN DO:
    pMessage = "NO se encontr� la configuraci�n CONFIG-WEB-PRICING".
    RETURN "ADM-ERROR".
END.

/* Capturamos URL y TOKEN */
x-Url = TRIM(VtaTabla.Llave_c1) +       /* URL */
        TRIM(VtaTabla.Llave_c2) .       /* Token */

/* Campos a devolver */
x-Url = x-Url + ~
"?FormatXML=True&~
FieldName=ExchangeRate,~
CurrencySale,~
RepositionCostInCurrencySale".

/* Llave de b�squeda */
x-Url = x-Url + ~
'&ArtCode=' + pArtCode + ~
'&CategoryCustomer=' + pCategoryCustomer + ~
'&SalesCondition=' + pSalesCondition.

/* DEF VAR x-editor AS CHAR VIEW-AS EDITOR SIZE 60 BY 6. */
/* x-editor = string(x-url).                             */
/* UPDATE x-editor.                                      */
/* RETURN.                                               */

/* Capturamos el API */
DEFINE VAR x-oXmlHttp AS COM-HANDLE NO-UNDO.

CREATE "MSXML2.ServerXMLHTTP.6.0" x-oXmlHttp.

x-oXmlHttp:OPEN( "GET", x-Url, NO ). 
x-oXmlHttp:setRequestHeader( "Content-Type", "application/xml;charset=utf-8" ).
x-oXmlHttp:setOption( 2, 13056 ) .  
x-oXmlHttp:SEND() NO-ERROR.

IF ERROR-STATUS:GET-NUMBER(1) > 0 THEN DO:
    pMessage = "API NO responde" + CHR(10) + CHR(10) + "Comunicarse con el �rea de Sistemas".
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN 'ADM-ERROR'.
END.
IF x-oXmlHttp:STATUS <> 200 THEN DO:
    pMessage = "ERROR AL ENVIAR TRAMA : " + CHR(10) + x-oXmlHttp:responseText.
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN 'ADM-ERROR'.
END.
/* Respuesta del API */
x-Xml = x-oXmlHttp:responseText.

x-Texto = STRING(x-Xml).

/* **************************************************************************************** */
/* Buscamos error */
/* **************************************************************************************** */
DEF VAR x-Cadena1 AS CHAR NO-UNDO.
DEF VAR x-Cadena2 AS CHAR NO-UNDO.
DEF VAR x-Inicio AS INTE NO-UNDO.
DEF VAR x-Fin AS INTE NO-UNDO.

IF INDEX(x-Texto, '<error>') > 0 THEN DO:
    x-Cadena1 = "<error>".
    x-Cadena2 = "</error>".
    x-Inicio = INDEX(x-Texto, x-Cadena1).
    x-Fin = INDEX(x-Texto, x-Cadena2).
    IF x-Inicio > x-Fin THEN DO:
        pMessage = "VALIDACION: Precio Pelda�o Bruto NO disponible para la venta" + CHR(10) + 
            "Porque NO est� configurado el precio pelda�o bruto o est� configurado con margen negativo" + CHR(10) + 
            "NO disponible para la venta" + CHR(10) + 
            "Art�culo: " + pArtCode + CHR(10) +
            "Comunicarse con el Jefe de L�nea".
    END.
    ELSE DO:
        pMessage = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).
        pMessage = pMessage + CHR(10) + 
            "Porque NO est� configurado el precio pelda�o bruto o est� configurado con margen negativo" + CHR(10) + 
            "NO disponible para la venta" + CHR(10) + 
            "Art�culo: " + pArtCode + CHR(10) +
            "Comunicarse con el Jefe de L�nea".
    END.
    RETURN "ADM-ERROR".
END.
/* **************************************************************************************** */

DEF VAR Inicio AS INT64 NO-UNDO.
DEF VAR Fin AS INT64 NO-UNDO.

/* Separamos la DATA */
x-Inicio = INDEX(x-Texto, "<Data>").
x-Fin = INDEX(x-Texto, "</Data>").

IF x-Inicio = 0 OR x-Fin = 0 THEN DO:
    pMessage = "API NO responde".
    RETURN "ADM-ERROR".
END.

x-Texto = SUBSTRING(x-Texto, x-Inicio + 6, (x-Fin - x-Inicio - 6)).

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
/* Buscamos el costo de reposici�n */
/* **************************************************************************************** */
DEF VAR x-CostoReposicion AS CHAR NO-UNDO.
DEF VAR pCostoReposicion AS DECI NO-UNDO.

x-Cadena1 = "<RepositionCostInCurrencySale>".
x-Cadena2 = "</RepositionCostInCurrencySale>".

x-Inicio = INDEX(x-Texto, x-Cadena1).
x-Fin = INDEX(x-Texto, x-Cadena2).
IF x-Inicio > x-Fin THEN DO:
    pMessage = "Costo de Reposici�n: NO registrado en la lista de precios".
    RETURN "ADM-ERROR".
END.
x-CostoReposicion = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

pRepositionCostInCurrencySale = DECIMAL(x-CostoReposicion) NO-ERROR.
IF ERROR-STATUS:ERROR = YES THEN DO:
    pMessage = "Art�culo: " + pArtCode + CHR(10) +
                "Costo de Reposici�n no est� en el formato correcto" + CHR(10) + x-texto.
    RETURN "ADM-ERROR".
END.

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-web_api-pricing-margen) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE web_api-pricing-margen Procedure 
PROCEDURE web_api-pricing-margen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pCodDiv                         AS CHAR.
DEF INPUT PARAMETER pArtCode                        AS CHAR.
DEF INPUT PARAMETER pSalesChannel AS CHAR.
DEF INPUT PARAMETER pCategoryCustomer               AS CHAR.
DEF INPUT PARAMETER pSalesCondition                 AS CHAR.
DEF OUTPUT PARAMETER pMonVta                        AS INTE NO-UNDO.
DEF OUTPUT PARAMETER pTpoCmb                        AS DECI NO-UNDO.
DEF OUTPUT PARAMETER pRepositionCostInCurrencySale  AS DECI NO-UNDO.
DEF OUTPUT PARAMETER pPrecioDescontado              AS DECI NO-UNDO.
DEF OUTPUT PARAMETER pMessage                       AS CHAR NO-UNDO.

DEFINE VAR x-Url AS LONGCHAR.
DEFINE VAR x-Xml AS LONGCHAR.
DEFINE VAR x-Texto AS CHAR.

FIND FIRST VtaTabla WHERE VtaTabla.CodCia = s-CodCia
    AND VtaTabla.Tabla = 'CONFIG-WEB-PRICING' NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaTabla THEN DO:
    pMessage = "NO se encontr� la configuraci�n CONFIG-WEB-PRICING".
    RETURN "ADM-ERROR".
END.

/* Capturamos URL y TOKEN */
x-Url = TRIM(VtaTabla.Llave_c1) +       /* URL */
        TRIM(VtaTabla.Llave_c2) .       /* Token */

/* Llave de b�squeda */
/* Llave de b�squeda */
x-Url = x-Url + ~
'/' + pArtCode + ~
'/' + pSalesChannel + ~
'/' + pCategoryCustomer + ~
'/' + pSalesCondition.

/* Capturamos el API */
DEFINE VAR x-oXmlHttp AS COM-HANDLE NO-UNDO.

CREATE "MSXML2.ServerXMLHTTP.6.0" x-oXmlHttp.

x-oXmlHttp:OPEN( "GET", x-Url, NO ). 
x-oXmlHttp:setRequestHeader( "Content-Type", "application/xml;charset=utf-8" ).
x-oXmlHttp:setOption( 2, 13056 ) .  
x-oXmlHttp:SEND() NO-ERROR.

IF ERROR-STATUS:GET-NUMBER(1) > 0 THEN DO:
    pMessage = "Se detecta demasiadas peticiones de uso del servidor de aplicaciones (100.198)," + CHR(10) +
        "Vuelva a intentar y de persistir el mismo mensaje, contactar a Sistemas"  + CHR(10) +
        TRIM(x-Url).
    RUN web_connection_log (x-Url, ERROR-STATUS:GET-MESSAGE(1)).
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN 'ADM-ERROR'.
END.
IF x-oXmlHttp:STATUS <> 200 THEN DO:
    pMessage = "ERROR AL ENVIAR TRAMA : " + CHR(10) + x-oXmlHttp:responseText.
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN 'ADM-ERROR'.
END.
/* Respuesta del API */
x-Xml = x-oXmlHttp:responseText.

x-Texto = STRING(x-Xml).

/* **************************************************************************************** */
/* Buscamos error */
/* **************************************************************************************** */
DEF VAR x-Cadena1 AS CHAR NO-UNDO.
DEF VAR x-Cadena2 AS CHAR NO-UNDO.
DEF VAR x-Inicio AS INTE NO-UNDO.
DEF VAR x-Fin AS INTE NO-UNDO.

IF INDEX(x-Texto, '<error>') > 0 THEN DO:
    x-Cadena1 = "<error>".
    x-Cadena2 = "</error>".
    x-Inicio = INDEX(x-Texto, x-Cadena1).
    x-Fin = INDEX(x-Texto, x-Cadena2).
    x-Texto = "API no responde".
    IF x-Inicio > x-Fin THEN DO:
        pMessage = "VALIDACION: Precio Pelda�o Bruto NO disponible para la venta" + CHR(10) + 
            "Porque NO est� configurado el precio pelda�o bruto" + CHR(10) + 
            "NO disponible para la venta" + CHR(10) + 
            "Art�culo: " + pArtCode + CHR(10) +
            "Pelda�o: " + pSalesChannel + CHR(10) +
            "Categor�a del cliente: " + pCategoryCustomer + CHR(10) +
            "Condici�n de venta: " + pSalesCondition + CHR(10) + CHR(10) +
            "Comunicarse con el Jefe de L�nea para su diagn�stico previo" + CHR(10) +
            "y de ser necesario se comunicar� con sistemas".
    END.
    ELSE DO:
        pMessage = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).
        pMessage = pMessage + CHR(10) + 
            "Porque NO est� configurado el precio pelda�o bruto" + CHR(10) + 
            "NO disponible para la venta" + CHR(10) + 
            "Art�culo: " + pArtCode + CHR(10) +
            "Pelda�o: " + pSalesChannel + CHR(10) +
            "Categor�a del cliente: " + pCategoryCustomer + CHR(10) +
            "Condici�n de venta: " + pSalesCondition + CHR(10) + CHR(10) +
            "Comunicarse con el Jefe de L�nea para su diagn�stico previo" + CHR(10) +
            "y de ser necesario se comunicar� con sistemas".
    END.
    RUN web_connection_log (x-Url, x-Texto).
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
/* **************************************************************************************** */
DEF VAR Inicio AS INT64 NO-UNDO.
DEF VAR Fin AS INT64 NO-UNDO.

/* Separamos la DATA */
x-Inicio = INDEX(x-Texto, "<Data>").
x-Fin = INDEX(x-Texto, "</Data>").

IF x-Inicio = 0 OR x-Fin = 0 THEN DO:
    pMessage = "XML no retorn� ninguna informaci�n" + CHR(10) +
        "Vuelva a intentar y de persistir el mismo mensaje, contactar a Sistemas".
    RUN web_connection_log (x-Url, "XML no retorn� ninguna informaci�n").
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.

x-Texto = SUBSTRING(x-Texto, x-Inicio + 6, (x-Fin - x-Inicio - 6)).

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
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
x-MonVta = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

pMonVta = LOOKUP(x-MonVta, x-Monedas).
IF pMonVta = 0 THEN DO:
    pMessage = "Art�culo: " + pArtCode + CHR(10) +
                "Moneda de Venta no est� en el formato correcto" + CHR(10) + x-texto.
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
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
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
x-TpoCmb = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

pTpoCmb = DECIMAL(x-TpoCmb) NO-ERROR.
IF ERROR-STATUS:ERROR = YES THEN DO:
    pMessage = "Art�culo: " + pArtCode + CHR(10) +
                "Tipo de Cambio no est� en el formato correcto" + CHR(10) + x-texto.
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
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
    pMessage = "Costo de Reposici�n: NO registrado en la lista de precios".
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
x-CostoReposicion = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

pRepositionCostInCurrencySale = DECIMAL(x-CostoReposicion) NO-ERROR.
IF ERROR-STATUS:ERROR = YES THEN DO:
    pMessage = "Art�culo: " + pArtCode + CHR(10) +
                "Costo de Reposici�n no est� en el formato correcto" + CHR(10) + x-texto.
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
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
    pMessage = "Precio Unitario Pelda�o Bruto: NO registrado en la lista de precios".
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
x-PrecioDescontado = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

pPrecioDescontado = DECIMAL(x-PrecioDescontado) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    pMessage = "Precio Unitario Pelda�o no est� en formato de n�meros" + CHR(10) + x-texto.
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
IF pPrecioDescontado <= 0 THEN DO:
    pMessage = "Precio Pelda�o Bruto est� mal configurado" + CHR(10) + CHR(10) +     
        "Por favor consultar al Jefe de L�nea". 
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN 'ADM-ERROR'.
END.

RELEASE OBJECT x-oXmlHttp NO-ERROR.

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-web_api-pricing-precio-costo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE web_api-pricing-precio-costo Procedure 
PROCEDURE web_api-pricing-precio-costo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pArtCode AS CHAR.
DEF INPUT PARAMETER pSalesChannel AS CHAR.
DEF INPUT PARAMETER pCategoryCustomer AS CHAR.
DEF INPUT PARAMETER pSalesCondition AS CHAR.
DEF OUTPUT PARAMETER pMonVta AS INTE NO-UNDO.
DEF OUTPUT PARAMETER pTpoCmb AS DECI NO-UNDO.
DEF OUTPUT PARAMETER pPrecioDescontado AS DECI NO-UNDO.
DEF OUTPUT PARAMETER pRepositionCostInCurrencySale AS DECI NO-UNDO.
DEF OUTPUT PARAMETER pMessage AS CHAR NO-UNDO.

DEFINE VAR x-Url AS LONGCHAR.
DEFINE VAR x-Xml AS LONGCHAR.
DEFINE VAR x-Texto AS CHAR.

FIND FIRST VtaTabla WHERE VtaTabla.CodCia = s-CodCia
    AND VtaTabla.Tabla = 'CONFIG-WEB-PRICING' NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaTabla THEN DO:
    pMessage = "NO se encontr� la configuraci�n CONFIG-WEB-PRICING".
    RETURN "ADM-ERROR".
END.

/* Capturamos URL y TOKEN */
x-Url = TRIM(VtaTabla.Llave_c1) +       /* URL */
        TRIM(VtaTabla.Llave_c2) .       /* Token */
/* Llave de b�squeda */
x-Url = x-Url + ~
'/' + pArtCode + ~
'/' + pSalesChannel + ~
'/' + pCategoryCustomer + ~
'/' + pSalesCondition.

/* Capturamos el API */
DEFINE VAR x-oXmlHttp AS COM-HANDLE NO-UNDO.

CREATE "MSXML2.ServerXMLHTTP.6.0" x-oXmlHttp.

x-oXmlHttp:OPEN( "GET", x-Url, NO ). 
x-oXmlHttp:setRequestHeader( "Content-Type", "application/xml;charset=utf-8" ).
x-oXmlHttp:setOption( 2, 13056 ) .  
x-oXmlHttp:SEND() NO-ERROR.

IF ERROR-STATUS:GET-NUMBER(1) > 0 THEN DO:
    pMessage = "Se detecta demasiadas peticiones de uso del servidor de aplicaciones (Servidor de aplicaciones detenido o no existe)" + CHR(10) +
        x-Url + CHR(10) + CHR(10) +
        "Vuelva a intentar y de persistir el mismo mensaje, contactar a Sistemas".
    RUN web_connection_log (x-Url, ERROR-STATUS:GET-MESSAGE(1)).
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN 'ADM-ERROR'.
END.
IF x-oXmlHttp:STATUS <> 200 THEN DO:
    pMessage = "ERROR AL ENVIAR TRAMA : " + CHR(10) + x-oXmlHttp:responseText.
    RUN web_connection_log (x-Url, "ERROR AL ENVIAR TRAMA : " + x-oXmlHttp:responseText).
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN 'ADM-ERROR'.
END.
/* Respuesta del API */
x-Xml = x-oXmlHttp:responseText.
x-Texto = STRING(x-Xml).
/* **************************************************************************************** */
/* Buscamos error */
/* **************************************************************************************** */
DEF VAR x-Cadena1 AS CHAR NO-UNDO.
DEF VAR x-Cadena2 AS CHAR NO-UNDO.
DEF VAR x-Inicio AS INTE NO-UNDO.
DEF VAR x-Fin AS INTE NO-UNDO.

IF INDEX(x-Texto, '<error>') > 0 THEN DO:
    x-Cadena1 = "<error>".
    x-Cadena2 = "</error>".
    x-Inicio = INDEX(x-Texto, x-Cadena1).
    x-Fin = INDEX(x-Texto, x-Cadena2).
    x-Texto = "API no responde".
    IF x-Inicio > x-Fin THEN DO:
        pMessage = "VALIDACION: Precio Pelda�o Bruto NO disponible para la venta" + CHR(10) + 
            "Porque NO est� configurado el precio pelda�o bruto" + CHR(10) + 
            "NO disponible para la venta" + CHR(10) + 
            "Art�culo: " + pArtCode + CHR(10) +
            "Pelda�o: " + pSalesChannel + CHR(10) +
            "Categor�a del cliente: " + pCategoryCustomer + CHR(10) +
            "Condici�n de venta: " + pSalesCondition + CHR(10) + CHR(10) +
            "Comunicarse con el Jefe de L�nea para su diagn�stico previo" + CHR(10) +
            "y de ser necesario se comunicar� con sistemas".
    END.
    ELSE DO:
        pMessage = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).
        pMessage = pMessage + CHR(10) + 
            "Porque NO est� configurado el precio pelda�o bruto" + CHR(10) + 
            "NO disponible para la venta" + CHR(10) + 
            "Art�culo: " + pArtCode + CHR(10) +
            "Pelda�o: " + pSalesChannel + CHR(10) +
            "Categor�a del cliente: " + pCategoryCustomer + CHR(10) +
            "Condici�n de venta: " + pSalesCondition + CHR(10) + CHR(10) +
            "Comunicarse con el Jefe de L�nea para su diagn�stico previo" + CHR(10) +
            "y de ser necesario se comunicar� con sistemas".
    END.
    RUN web_connection_log (x-Url, x-Texto).
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
/* **************************************************************************************** */

DEF VAR Inicio AS INT64 NO-UNDO.
DEF VAR Fin AS INT64 NO-UNDO.

/* Separamos la DATA */
x-Inicio = INDEX(x-Texto, "<Data>").
x-Fin = INDEX(x-Texto, "</Data>").

IF x-Inicio = 0 OR x-Fin = 0 THEN DO:
    pMessage = "XML no retorn� ninguna informaci�n" + CHR(10) +
        "Vuelva a intentar y de persistir el mismo mensaje, contactar a Sistemas".
    RUN web_connection_log (x-Url, "XML no retorn� ninguna informaci�n").
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.

x-Texto = SUBSTRING(x-Texto, x-Inicio + 6, (x-Fin - x-Inicio - 6)).

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
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
x-MonVta = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

pMonVta = LOOKUP(x-MonVta, x-Monedas).
IF pMonVta = 0 THEN DO:
    pMessage = "Art�culo: " + pArtCode + CHR(10) +
                "Moneda de Venta no est� en el formato correcto" + CHR(10) + x-texto.
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
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
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
x-TpoCmb = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

pTpoCmb = DECIMAL(x-TpoCmb) NO-ERROR.
IF ERROR-STATUS:ERROR = YES THEN DO:
    pMessage = "Art�culo: " + pArtCode + CHR(10) +
                "Tipo de Cambio no est� en el formato correcto" + CHR(10) + x-texto.
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
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
    pMessage = "Precio Unitario Pelda�o Bruto: NO registrado en la lista de precios".
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
x-PrecioDescontado = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

pPrecioDescontado = DECIMAL(x-PrecioDescontado) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    pMessage = "Precio Unitario Pelda�o no est� en formato de n�meros" + CHR(10) + x-texto.
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
IF pPrecioDescontado <= 0 THEN DO:
    pMessage = "Precio Pelda�o Bruto est� mal configurado" + CHR(10) + CHR(10) +     
        "Por favor consultar al Jefe de L�nea". 
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN 'ADM-ERROR'.
END.

/* **************************************************************************************** */
/* Buscamos el costo de reposici�n */
/* **************************************************************************************** */
DEF VAR x-CostoReposicion AS CHAR NO-UNDO.

x-Cadena1 = "<RepositionCostInCurrencySale>".
x-Cadena2 = "</RepositionCostInCurrencySale>".

x-Inicio = INDEX(x-Texto, x-Cadena1).
x-Fin = INDEX(x-Texto, x-Cadena2).
IF x-Inicio > x-Fin THEN DO:
    pMessage = "Costo de Reposici�n: NO registrado en la lista de precios".
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
x-CostoReposicion = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

pRepositionCostInCurrencySale = DECIMAL(x-CostoReposicion) NO-ERROR.
IF ERROR-STATUS:ERROR = YES THEN DO:
    pMessage = "Art�culo: " + pArtCode + CHR(10) +
                "Costo de Reposici�n no est� en el formato correcto" + CHR(10) + x-texto.
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.

RELEASE OBJECT x-oXmlHttp NO-ERROR.

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-web_api-pricing-precio-costo-socket) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE web_api-pricing-precio-costo-socket Procedure 
PROCEDURE web_api-pricing-precio-costo-socket :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       V�A SOCKET
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pArtCode AS CHAR.
DEF INPUT PARAMETER pSalesChannel AS CHAR.
DEF INPUT PARAMETER pCategoryCustomer AS CHAR.
DEF INPUT PARAMETER pSalesCondition AS CHAR.
DEF OUTPUT PARAMETER pMonVta AS INTE NO-UNDO.
DEF OUTPUT PARAMETER pTpoCmb AS DECI NO-UNDO.
DEF OUTPUT PARAMETER pPrecioDescontado AS DECI NO-UNDO.
DEF OUTPUT PARAMETER pRepositionCostInCurrencySale AS DECI NO-UNDO.
DEF OUTPUT PARAMETER pMessage AS CHAR NO-UNDO.

DEFINE VAR x-Url AS LONGCHAR.
DEFINE VAR x-Xml AS LONGCHAR.
DEFINE VAR x-Texto AS CHAR.

FIND FIRST VtaTabla WHERE VtaTabla.CodCia = s-CodCia
    AND VtaTabla.Tabla = 'CONFIG-WEB-PRICING' NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaTabla THEN DO:
    pMessage = "NO se encontr� la configuraci�n CONFIG-WEB-PRICING".
    RETURN "ADM-ERROR".
END.

/* Capturamos URL y TOKEN */
x-Url = TRIM(VtaTabla.Llave_c1) +       /* URL */
        TRIM(VtaTabla.Llave_c2) .       /* Token */
/* Llave de b�squeda */
x-Url = x-Url + ~
'/' + pArtCode + ~
'/' + pSalesChannel + ~
'/' + pCategoryCustomer + ~
'/' + pSalesCondition.

/* Capturamos el API */
DEF VAR x-Resultado AS CHAR NO-UNDO.
DEF VAR x-Respuesta AS LONGCHAR NO-UNDO.
DEF VAR x-Contenido AS LONGCHAR NO-UNDO.

RUN lib/http-get-contenido.p (INPUT x-Url,
                              OUTPUT x-Resultado,
                              OUTPUT x-Respuesta,
                              OUTPUT x-Contenido).      /* Respuesta del API */

/* Control de errores */
IF x-Resultado BEGINS "0:" THEN DO:
    pMessage = "Se detecta demasiadas peticiones de uso del servidor de aplicaciones (Servidor de aplicaciones detenido o no existe)" + CHR(10) +
        x-Url + CHR(10) + CHR(10) +
        "Vuelva a intentar y de persistir el mismo mensaje, contactar a Sistemas".
    RUN web_connection_log (x-Url, ERROR-STATUS:GET-MESSAGE(1)).
    RETURN 'ADM-ERROR'.
END.

/* Respuesta del API */
x-Xml = x-Contenido.
x-Texto = STRING(x-Xml).
/* **************************************************************************************** */
/* Buscamos error */
/* **************************************************************************************** */
DEF VAR x-Cadena1 AS CHAR NO-UNDO.
DEF VAR x-Cadena2 AS CHAR NO-UNDO.
DEF VAR x-Inicio AS INTE NO-UNDO.
DEF VAR x-Fin AS INTE NO-UNDO.

IF INDEX(x-Texto, '<error>') > 0 THEN DO:
    x-Cadena1 = "<error>".
    x-Cadena2 = "</error>".
    x-Inicio = INDEX(x-Texto, x-Cadena1).
    x-Fin = INDEX(x-Texto, x-Cadena2).
    x-Texto = "API no responde".
    IF x-Inicio > x-Fin THEN DO:
        pMessage = "VALIDACION: Precio Pelda�o Bruto NO disponible para la venta" + CHR(10) + 
            "Porque NO est� configurado el precio pelda�o bruto" + CHR(10) + 
            "NO disponible para la venta" + CHR(10) + 
            "Art�culo: " + pArtCode + CHR(10) +
            "Pelda�o: " + pSalesChannel + CHR(10) +
            "Categor�a del cliente: " + pCategoryCustomer + CHR(10) +
            "Condici�n de venta: " + pSalesCondition + CHR(10) + CHR(10) +
            "Comunicarse con el Jefe de L�nea para su diagn�stico previo" + CHR(10) +
            "y de ser necesario se comunicar� con sistemas".
    END.
    ELSE DO:
        pMessage = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).
        pMessage = pMessage + CHR(10) + 
            "Porque NO est� configurado el precio pelda�o bruto" + CHR(10) + 
            "NO disponible para la venta" + CHR(10) + 
            "Art�culo: " + pArtCode + CHR(10) +
            "Pelda�o: " + pSalesChannel + CHR(10) +
            "Categor�a del cliente: " + pCategoryCustomer + CHR(10) +
            "Condici�n de venta: " + pSalesCondition + CHR(10) + CHR(10) +
            "Comunicarse con el Jefe de L�nea para su diagn�stico previo" + CHR(10) +
            "y de ser necesario se comunicar� con sistemas".
    END.
    RUN web_connection_log (x-Url, x-Texto).
    RETURN "ADM-ERROR".
END.
/* **************************************************************************************** */

DEF VAR Inicio AS INT64 NO-UNDO.
DEF VAR Fin AS INT64 NO-UNDO.

/* Separamos la DATA */
x-Inicio = INDEX(x-Texto, "<Data>").
x-Fin = INDEX(x-Texto, "</Data>").

IF x-Inicio = 0 OR x-Fin = 0 THEN DO:
    pMessage = "XML no retorn� ninguna informaci�n" + CHR(10) +
        "Vuelva a intentar y de persistir el mismo mensaje, contactar a Sistemas".
    RUN web_connection_log (x-Url, "XML no retorn� ninguna informaci�n").
    RETURN "ADM-ERROR".
END.

x-Texto = SUBSTRING(x-Texto, x-Inicio + 6, (x-Fin - x-Inicio - 6)).

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
    pMessage = "Precio Unitario Pelda�o Bruto: NO registrado en la lista de precios".
    RETURN "ADM-ERROR".
END.
x-PrecioDescontado = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

pPrecioDescontado = DECIMAL(x-PrecioDescontado) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    pMessage = "Precio Unitario Pelda�o no est� en formato de n�meros" + CHR(10) + x-texto.
    RETURN "ADM-ERROR".
END.
IF pPrecioDescontado <= 0 THEN DO:
    pMessage = "Precio Pelda�o Bruto est� mal configurado" + CHR(10) + CHR(10) +     
        "Por favor consultar al Jefe de L�nea". 
    RETURN 'ADM-ERROR'.
END.

/* **************************************************************************************** */
/* Buscamos el costo de reposici�n */
/* **************************************************************************************** */
DEF VAR x-CostoReposicion AS CHAR NO-UNDO.

x-Cadena1 = "<RepositionCostInCurrencySale>".
x-Cadena2 = "</RepositionCostInCurrencySale>".

x-Inicio = INDEX(x-Texto, x-Cadena1).
x-Fin = INDEX(x-Texto, x-Cadena2).
IF x-Inicio > x-Fin THEN DO:
    pMessage = "Costo de Reposici�n: NO registrado en la lista de precios".
    RETURN "ADM-ERROR".
END.
x-CostoReposicion = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

pRepositionCostInCurrencySale = DECIMAL(x-CostoReposicion) NO-ERROR.
IF ERROR-STATUS:ERROR = YES THEN DO:
    pMessage = "Art�culo: " + pArtCode + CHR(10) +
                "Costo de Reposici�n no est� en el formato correcto" + CHR(10) + x-texto.
    RETURN "ADM-ERROR".
END.

RETURN "OK".

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
http://192.168.0.237:62/api/storeprocedure/XML
/002056
/1
/C
/000
*/
DEF INPUT PARAMETER pArtCode AS CHAR.
DEF INPUT PARAMETER pSalesChannel AS CHAR.
DEF INPUT PARAMETER pCategoryCustomer AS CHAR.
DEF INPUT PARAMETER pSalesCondition AS CHAR.
DEF OUTPUT PARAMETER pMonVta AS INTE NO-UNDO.
DEF OUTPUT PARAMETER pTpoCmb AS DECI NO-UNDO.
DEF OUTPUT PARAMETER pPrecioDescontado AS DECI NO-UNDO.
DEF OUTPUT PARAMETER pMessage AS CHAR NO-UNDO.

/* */
DEFINE VAR x-Url AS LONGCHAR.
DEFINE VAR x-Xml AS LONGCHAR.
DEFINE VAR x-Texto AS CHAR.

FIND FIRST VtaTabla WHERE VtaTabla.CodCia = s-CodCia
    AND VtaTabla.Tabla = 'CONFIG-WEB-PRICING' NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaTabla THEN DO:
    pMessage = "NO se encontr� la configuraci�n CONFIG-WEB-PRICING".
    RETURN "ADM-ERROR".
END.

/* Capturamos URL y TOKEN */
x-Url = TRIM(VtaTabla.Llave_c1) +       /* URL */
        TRIM(VtaTabla.Llave_c2) .       /* Token */
/* Llave de b�squeda */
x-Url = x-Url + ~
'/' + pArtCode + ~
'/' + pSalesChannel + ~
'/' + pCategoryCustomer + ~
'/' + pSalesCondition.

/* Capturamos el API */
DEF VAR pResult AS CHAR NO-UNDO.
DEF VAR pResponse AS LONGCHAR NO-UNDO.
DEF VAR pContent AS LONGCHAR NO-UNDO.

RUN lib/http-get-contenido.r(x-Url,
                             OUTPUT pResult,
                             OUTPUT pResponse,
                             OUTPUT pContent) 
    NO-ERROR.

IF pResult <> "1:Success"  THEN DO:
    /* ERROR DETECTADO */
    CASE pResult:
        WHEN "0:Not Content" THEN DO:
            pMessage = "VALIDACION: Precio Pelda�o Bruto NO disponible para la venta" + CHR(10) + 
                "Porque NO est� configurado el precio pelda�o bruto" + CHR(10) + 
                "NO disponible para la venta" + CHR(10) + 
                "Art�culo: " + pArtCode + CHR(10) +
                "Pelda�o: " + pSalesChannel + CHR(10) +
                "Categor�a del cliente: " + pCategoryCustomer + CHR(10) +
                "Condici�n de venta: " + pSalesCondition + CHR(10) + CHR(10) +
                "Comunicarse con el Jefe de L�nea para su diagn�stico previo" + CHR(10) +
                "y de ser necesario se comunicar� con sistemas".
        END.
        WHEN "0:No Socket" THEN DO:
            pMessage = "ERROR AL ENVIAR TRAMA : " + CHR(10) + x-Url.
        END.
        WHEN "0:Failure" THEN DO:
            pMessage = "Se detecta demasiadas peticiones de uso del servidor de aplicaciones (Servidor de aplicaciones detenido o no existe)" + CHR(10) +
                x-Url + CHR(10) + CHR(10) +
                "Vuelva a intentar y de persistir el mismo mensaje, contactar a Sistemas".
        END.
        OTHERWISE DO:
            pMessage = "ERROR API : " + CHR(10) + x-Url + "Vuelva a intentar y de persistir el mismo mensaje, contactar a Sistemas".
        END.
    END CASE.
    RETURN "ADM-ERROR".
END.

/* **************************************************************************************** */
/* Buscamos error */
/* **************************************************************************************** */
DEF VAR x-Cadena1 AS CHAR NO-UNDO.
DEF VAR x-Cadena2 AS CHAR NO-UNDO.
DEF VAR x-Inicio AS INTE NO-UNDO.
DEF VAR x-Fin AS INTE NO-UNDO.

DEF VAR Inicio AS INT64 NO-UNDO.
DEF VAR Fin AS INT64 NO-UNDO.

/* Separamos la DATA */
x-Texto = pContent.
x-Inicio = INDEX(x-Texto, "<Data>").
x-Fin = INDEX(x-Texto, "</Data>").

IF x-Inicio = 0 OR x-Fin = 0 THEN DO:
    pMessage = "XML no retorn� ninguna informaci�n" + CHR(10) +
        "Vuelva a intentar y de persistir el mismo mensaje, contactar a Sistemas".
    RETURN "ADM-ERROR".
END.

x-Texto = SUBSTRING(x-Texto, x-Inicio + 6, (x-Fin - x-Inicio - 6)).

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
    pMessage = "Precio Unitario Pelda�o Bruto: NO registrado en la lista de precios".
    RETURN "ADM-ERROR".
END.
x-PrecioDescontado = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

pPrecioDescontado = DECIMAL(x-PrecioDescontado) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    pMessage = "Precio Unitario Pelda�o no est� en formato de n�meros" + CHR(10) + x-texto.
    RETURN "ADM-ERROR".
END.
IF pPrecioDescontado <= 0 THEN DO:
    pMessage = "Precio Pelda�o Bruto est� mal configurado" + CHR(10) + CHR(10) +     
        "Por favor consultar al Jefe de L�nea". 
    RETURN 'ADM-ERROR'.
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

END.
ELSE DO:

END.



RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-web_api-pricing-preuni-contrato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE web_api-pricing-preuni-contrato Procedure 
PROCEDURE web_api-pricing-preuni-contrato :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* 
Sintaxis:
http://192.168.100.221:9000/sales/salespricelist/item/<cliente>/<art�culo>/view

Resultado:         
[
    {
        "id": 4,
        "idcustomerriqra": null,
        "pricelistid": null,
        "custcode": "11111111111",
        "name": "0",
        "artcode": "084471",
        "factor": null,
        "price": 41.9169,
        "undvta": "UNI"
    }
]
*/

DEF INPUT PARAMETER pCustomer AS CHAR.                                     
DEF INPUT PARAMETER pArtCode AS CHAR.
DEF OUTPUT PARAMETER pMonVta AS INTE NO-UNDO.
DEF OUTPUT PARAMETER pPrecioDescontado AS DECI NO-UNDO.
DEF OUTPUT PARAMETER pUndVta AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER pMessage AS CHAR NO-UNDO.

/* */
DEFINE VAR x-Url AS LONGCHAR.
DEFINE VAR x-Xml AS LONGCHAR.
DEFINE VAR x-Texto AS CHAR.

FIND FIRST VtaTabla WHERE VtaTabla.CodCia = s-CodCia
    AND VtaTabla.Tabla = 'CONFIG-WEB-PRICING' NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaTabla OR TRUE <> (VtaTabla.Libre_c01 > '') THEN DO:
    pMessage = "NO se encontr� la configuraci�n CONFIG-WEB-PRICING Libre_c01".
    RETURN "ADM-ERROR".
END.

/* Capturamos URL */
x-Url = TRIM(VtaTabla.Libre_c01).       /* URL http://192.168.100.221:9000 */

/* Llave de b�squeda */
x-Url = x-Url + '/sales/salespricelist/item/' + TRIM(pCustomer) + '/' +
    TRIM(pArtCode) + '/view'.

/* Capturamos el API */
/* Capturamos el API */
DEF VAR pResult AS CHAR NO-UNDO.
DEF VAR pResponse AS LONGCHAR NO-UNDO.
DEF VAR pContent AS LONGCHAR NO-UNDO.

RUN lib/http-get-contenido.r(x-Url,
                             OUTPUT pResult,
                             OUTPUT pResponse,
                             OUTPUT pContent) 
    NO-ERROR.

IF pResult <> "1:Success"  THEN DO:
    /* ERROR DETECTADO */
    CASE pResult:
        WHEN "0:Not Content" THEN DO:
            /* Si no tiene precio contrato se contin�a con el proceso sin mostrar errores */
            pPrecioDescontado = 0.
            RETURN "OK".
        END.
        WHEN "0:No Socket" THEN DO:
            pMessage = "ERROR AL ENVIAR TRAMA : " + CHR(10) + x-Url.
        END.
        WHEN "0:Failure" THEN DO:
            pMessage = "Se detecta demasiadas peticiones de uso del servidor de aplicaciones (Servidor de aplicaciones detenido o no existe)" + CHR(10) +
                x-Url + CHR(10) + CHR(10) +
                "Vuelva a intentar y de persistir el mismo mensaje, contactar a Sistemas".
        END.
        OTHERWISE DO:
            pMessage = "ERROR API : " + CHR(10) + x-Url + "Vuelva a intentar y de persistir el mismo mensaje, contactar a Sistemas".
        END.
    END CASE.
    RETURN "ADM-ERROR".
END.

/* Respuesta del API */
x-Texto = TRIM(STRING(pContent)).

/* **************************************************************************************** */
/* Buscamos error */
/* Si no tiene precio entonces NO est� definido en el contrato */
/* **************************************************************************************** */
IF x-Texto BEGINS "[]" THEN DO:
    pPrecioDescontado = 0.
    RETURN "OK".
END.

/* **************************************************************************************** */
/* Buscamos el precio contrato */
/* **************************************************************************************** */
DEF VAR x-PrecioDescontado AS CHAR NO-UNDO.
DEF VAR z AS INTE NO-UNDO.

/* 09/03/2024: Verificamos primero el "offerprice" */
z = INDEX(x-Texto, '"offerprice":') + LENGTH('"offerprice":').
DO WHILE TRUE:
    IF LOOKUP(SUBSTRING(x-Texto,z,1), ',|"|}|]', '|') > 0 THEN LEAVE.
    x-PrecioDescontado = x-PrecioDescontado + SUBSTRING(x-Texto,z,1).
    z = z + 1.
END.
IF TRIM(x-PrecioDescontado) = 'null' THEN DO:
    x-PrecioDescontado = ''.
    z = INDEX(x-Texto, '"price":') + LENGTH('"price":').
    DO WHILE TRUE:
        IF LOOKUP(SUBSTRING(x-Texto,z,1), ',|"|}|]', '|') > 0 THEN LEAVE.
        x-PrecioDescontado = x-PrecioDescontado + SUBSTRING(x-Texto,z,1).
        z = z + 1.
    END.
END.
ASSIGN
    pPrecioDescontado = DECIMAL(x-PrecioDescontado) NO-ERROR.
IF ERROR-STATUS:ERROR = YES THEN DO:
    pMessage = "Precio Unitario Contrato no est� en formato de n�meros" + CHR(10) + x-texto.
    RETURN "ADM-ERROR".
END.
/* **************************************************************************************** */
/* Buscamos la moneda de venta */
/* **************************************************************************************** */
/* 02/11/2023: B.Acu�a moneda siempre en soles */
pMonVta = 1.

/* **************************************************************************************** */
/* Buscamos la unidad de venta */
/* **************************************************************************************** */
DEF VAR x-UndVta AS CHAR NO-UNDO.

z = INDEX(x-Texto, '"undvta":') + LENGTH('"undvta":').
DO WHILE TRUE:
    IF LOOKUP(SUBSTRING(x-Texto,z,1), ',|}|]', '|') > 0 THEN LEAVE.
    x-UndVta = x-UndVta + SUBSTRING(x-Texto,z,1).
    z = z + 1.
END.
pUndVta = TRIM(x-UndVta).
pUndVta = REPLACE(pUndVta, '"','').

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-web_api-pricing-preuni-contrato-old) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE web_api-pricing-preuni-contrato-old Procedure 
PROCEDURE web_api-pricing-preuni-contrato-old :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* 
Sintaxis:
http://192.168.100.221:9000/sales/salespricelist/item/<cliente>/<art�culo>/view

Resultado:         
[
    {
        "id": 4,
        "idcustomerriqra": null,
        "pricelistid": null,
        "custcode": "11111111111",
        "name": "0",
        "artcode": "084471",
        "factor": null,
        "price": 41.9169,
        "undvta": "UNI"
    }
]
*/
DEF INPUT PARAMETER pCustomer AS CHAR.                                     
DEF INPUT PARAMETER pArtCode AS CHAR.
DEF OUTPUT PARAMETER pMonVta AS INTE NO-UNDO.
DEF OUTPUT PARAMETER pPrecioDescontado AS DECI NO-UNDO.
DEF OUTPUT PARAMETER pUndVta AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER pMessage AS CHAR NO-UNDO.

/* */
DEFINE VAR x-Url AS LONGCHAR.
DEFINE VAR x-Xml AS LONGCHAR.
DEFINE VAR x-Texto AS CHAR.

FIND FIRST VtaTabla WHERE VtaTabla.CodCia = s-CodCia
    AND VtaTabla.Tabla = 'CONFIG-WEB-PRICING' NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaTabla OR TRUE <> (VtaTabla.Libre_c01 > '') THEN DO:
    pMessage = "NO se encontr� la configuraci�n CONFIG-WEB-PRICING Libre_c01".
    RETURN "ADM-ERROR".
END.

/* Capturamos URL */
x-Url = TRIM(VtaTabla.Libre_c01).       /* URL http://192.168.100.221:9000 */

/* Llave de b�squeda */
x-Url = x-Url + '/sales/salespricelist/item/' + TRIM(pCustomer) + '/' +
    TRIM(pArtCode) + '/view'.

/* Capturamos el API */
DEFINE VAR x-oXmlHttp AS COM-HANDLE NO-UNDO.

CREATE "MSXML2.ServerXMLHTTP.6.0" x-oXmlHttp.

x-oXmlHttp:OPEN( "GET", x-Url, NO ). 
x-oXmlHttp:setRequestHeader( "Content-Type", "application/xml;charset=utf-8" ).
x-oXmlHttp:setOption( 2, 13056 ) .  
x-oXmlHttp:SEND() NO-ERROR.

IF ERROR-STATUS:GET-NUMBER(1) > 0 THEN DO:
    pMessage = "Se detecta demasiadas peticiones de uso del servidor de aplicaciones (Servidor de aplicaciones detenido o no existe)" + CHR(10) +
        x-Url + CHR(10) + CHR(10) +
        "Vuelva a intentar y de persistir el mismo mensaje, contactar a Sistemas".
    RUN web_connection_log (x-Url, ERROR-STATUS:GET-MESSAGE(1)).
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN 'ADM-ERROR'.
END.
IF x-oXmlHttp:STATUS <> 200 THEN DO:
    pMessage = "ERROR AL ENVIAR TRAMA : " + CHR(10) + x-oXmlHttp:responseText.
    RUN web_connection_log (x-Url, "ERROR AL ENVIAR TRAMA : " + x-oXmlHttp:responseText).
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN 'ADM-ERROR'.
END.
/* Respuesta del API */
x-Xml = x-oXmlHttp:responseText.
x-Texto = TRIM(STRING(x-Xml)).

/* **************************************************************************************** */
/* Buscamos error */
/* Si no tiene precio entonces NO est� definido en el contrato */
/* **************************************************************************************** */
IF x-Texto BEGINS "[]" THEN DO:
    pPrecioDescontado = 0.
    RETURN "OK".
END.
/*RUN lib/limpiar-texto-abc (x-Texto,'',OUTPUT x-Texto).*/

x-Texto = REPLACE(x-Texto,CHR(10),"").
x-Texto = REPLACE(x-Texto,"[","").
x-Texto = REPLACE(x-Texto,"]","").
IF x-Texto BEGINS "[]" THEN DO:
    pPrecioDescontado = 0.
    RETURN "OK".
END.
/*
RUN lib/limpiar-texto (x-texto, '', OUTPUT x-Texto).
MESSAGE x-texto.
*/

/* **************************************************************************************** */
/* Buscamos el precio contrato */
/* **************************************************************************************** */
DEF VAR x-PrecioDescontado AS CHAR NO-UNDO.
DEF VAR z AS INTE NO-UNDO.

z = INDEX(x-Texto, '"price":') + LENGTH('"price":').
DO WHILE TRUE:
    IF LOOKUP(SUBSTRING(x-Texto,z,1), ',|"|}|]', '|') > 0 THEN LEAVE.
    x-PrecioDescontado = x-PrecioDescontado + SUBSTRING(x-Texto,z,1).
    z = z + 1.
END.

pPrecioDescontado = DECIMAL(x-PrecioDescontado) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    pMessage = "Precio Unitario Contrato no est� en formato de n�meros" + CHR(10) + x-texto.
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
/* **************************************************************************************** */
/* Buscamos la moneda de venta */
/* **************************************************************************************** */
/* DEF VAR x-MonVta AS CHAR NO-UNDO.                                                               */
/*                                                                                                 */
/* z = INDEX(x-Texto, '"currency":') + LENGTH('"currency":').                                      */
/* DO WHILE TRUE:                                                                                  */
/*     IF LOOKUP(SUBSTRING(x-Texto,z,1), ',|"|}|]', '|') > 0 THEN LEAVE.                           */
/*     x-MonVta = x-MonVta + SUBSTRING(x-Texto,z,1).                                               */
/*     z = z + 1.                                                                                  */
/* END.                                                                                            */
/*                                                                                                 */
/* pMonVta = INTEGER(x-MonVta) NO-ERROR.                                                           */
/* IF ERROR-STATUS:ERROR THEN DO:                                                                  */
/*     pMessage = "La Moneda de Venta Contrato no est� en formato de n�meros" + CHR(10) + x-texto. */
/*     RELEASE OBJECT x-oXmlHttp NO-ERROR.                                                         */
/*     RETURN "ADM-ERROR".                                                                         */
/* END.                                                                                            */

/* 02/11/2023: B.Acu�a moneda siempre en soles */
pMonVta = 1.

/* **************************************************************************************** */
/* Buscamos la unidad de venta */
/* **************************************************************************************** */
DEF VAR x-UndVta AS CHAR NO-UNDO.

z = INDEX(x-Texto, '"undvta":') + LENGTH('"undvta":').
DO WHILE TRUE:
    IF LOOKUP(SUBSTRING(x-Texto,z,1), ',|}|]', '|') > 0 THEN LEAVE.
    x-UndVta = x-UndVta + SUBSTRING(x-Texto,z,1).
    z = z + 1.
END.
pUndVta = TRIM(x-UndVta).
pUndVta = REPLACE(pUndVta, '"','').

RELEASE OBJECT x-oXmlHttp NO-ERROR.

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-web_api-pricing-preuni-contrato-socket) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE web_api-pricing-preuni-contrato-socket Procedure 
PROCEDURE web_api-pricing-preuni-contrato-socket :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* 
Sintaxis:
http://192.168.100.221:9000/sales/salespricelist/item/<cliente>/<art�culo>/view

Resultado:         
[
    {
        "id": 4,
        "idcustomerriqra": null,
        "pricelistid": null,
        "custcode": "11111111111",
        "name": "0",
        "artcode": "084471",
        "factor": null,
        "price": 41.9169,
        "undvta": "UNI"
    }
]
*/
DEF INPUT PARAMETER pCustomer AS CHAR.                                     
DEF INPUT PARAMETER pArtCode AS CHAR.
DEF OUTPUT PARAMETER pMonVta AS INTE NO-UNDO.
DEF OUTPUT PARAMETER pPrecioDescontado AS DECI NO-UNDO.
DEF OUTPUT PARAMETER pUndVta AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER pMessage AS CHAR NO-UNDO.

/* */
DEFINE VAR x-Url AS LONGCHAR.
DEFINE VAR x-Xml AS LONGCHAR.
DEFINE VAR x-Texto AS CHAR.

FIND FIRST VtaTabla WHERE VtaTabla.CodCia = s-CodCia
    AND VtaTabla.Tabla = 'CONFIG-WEB-PRICING' NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaTabla OR TRUE <> (VtaTabla.Libre_c01 > '') THEN DO:
    pMessage = "NO se encontr� la configuraci�n CONFIG-WEB-PRICING Libre_c01".
    RETURN "ADM-ERROR".
END.

/* Capturamos URL */
x-Url = TRIM(VtaTabla.Libre_c01).       /* URL http://192.168.100.221:9000 */

/* Llave de b�squeda */
x-Url = x-Url + '/sales/salespricelist/item/' + TRIM(pCustomer) + '/' +
    TRIM(pArtCode) + '/view'.

/* Capturamos el API */
DEF VAR x-Resultado AS CHAR NO-UNDO.
DEF VAR x-Respuesta AS LONGCHAR NO-UNDO.
DEF VAR x-Contenido AS LONGCHAR NO-UNDO.

RUN lib/http-get-contenido.p (INPUT x-Url,
                              OUTPUT x-Resultado,
                              OUTPUT x-Respuesta,
                              OUTPUT x-Contenido).      /* Respuesta del API */
/* Control de errores */
IF x-Resultado BEGINS "0:" THEN DO:
    pMessage = "Se detecta demasiadas peticiones de uso del servidor de aplicaciones (Servidor de aplicaciones detenido o no existe)" + CHR(10) +
        x-Url + CHR(10) + CHR(10) +
        "Vuelva a intentar y de persistir el mismo mensaje, contactar a Sistemas".
    RUN web_connection_log (x-Url, ERROR-STATUS:GET-MESSAGE(1)).
    RETURN 'ADM-ERROR'.
END.

/* Respuesta del API */
x-Xml = x-Contenido.
x-Texto = TRIM(STRING(x-Xml)).

/* **************************************************************************************** */
/* Buscamos error */
/* Si no tiene precio entonces NO est� definido en el contrato */
/* **************************************************************************************** */
IF x-Texto BEGINS "[]" THEN DO:
    pPrecioDescontado = 0.
    RETURN "OK".
END.

x-Texto = REPLACE(x-Texto,CHR(10),"").
x-Texto = REPLACE(x-Texto,"[","").
x-Texto = REPLACE(x-Texto,"]","").
IF x-Texto BEGINS "[]" THEN DO:
    pPrecioDescontado = 0.
    RETURN "OK".
END.
/* **************************************************************************************** */
/* Buscamos el precio contrato */
/* **************************************************************************************** */
DEF VAR x-PrecioDescontado AS CHAR NO-UNDO.
DEF VAR z AS INTE NO-UNDO.

z = INDEX(x-Texto, '"price":') + LENGTH('"price":').
DO WHILE TRUE:
    IF LOOKUP(SUBSTRING(x-Texto,z,1), ',|"|}|]', '|') > 0 THEN LEAVE.
    x-PrecioDescontado = x-PrecioDescontado + SUBSTRING(x-Texto,z,1).
    z = z + 1.
END.

pPrecioDescontado = DECIMAL(x-PrecioDescontado) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    pMessage = "Precio Unitario Contrato no est� en formato de n�meros" + CHR(10) + x-texto.
    RETURN "ADM-ERROR".
END.
/* **************************************************************************************** */
/* Buscamos la moneda de venta */
/* **************************************************************************************** */
/* DEF VAR x-MonVta AS CHAR NO-UNDO.                                                               */
/*                                                                                                 */
/* z = INDEX(x-Texto, '"currency":') + LENGTH('"currency":').                                      */
/* DO WHILE TRUE:                                                                                  */
/*     IF LOOKUP(SUBSTRING(x-Texto,z,1), ',|"|}|]', '|') > 0 THEN LEAVE.                           */
/*     x-MonVta = x-MonVta + SUBSTRING(x-Texto,z,1).                                               */
/*     z = z + 1.                                                                                  */
/* END.                                                                                            */
/*                                                                                                 */
/* pMonVta = INTEGER(x-MonVta) NO-ERROR.                                                           */
/* IF ERROR-STATUS:ERROR THEN DO:                                                                  */
/*     pMessage = "La Moneda de Venta Contrato no est� en formato de n�meros" + CHR(10) + x-texto. */
/*     RELEASE OBJECT x-oXmlHttp NO-ERROR.                                                         */
/*     RETURN "ADM-ERROR".                                                                         */
/* END.                                                                                            */

/* 02/11/2023: B.Acu�a moneda siempre en soles */
pMonVta = 1.
/* **************************************************************************************** */
/* Buscamos la unidad de venta */
/* **************************************************************************************** */
DEF VAR x-UndVta AS CHAR NO-UNDO.

z = INDEX(x-Texto, '"undvta":') + LENGTH('"undvta":').
DO WHILE TRUE:
    IF LOOKUP(SUBSTRING(x-Texto,z,1), ',|}|]', '|') > 0 THEN LEAVE.
    x-UndVta = x-UndVta + SUBSTRING(x-Texto,z,1).
    z = z + 1.
END.
pUndVta = TRIM(x-UndVta).
pUndVta = REPLACE(pUndVta, '"','').

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-web_api-pricing-preuni-ctouni) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE web_api-pricing-preuni-ctouni Procedure 
PROCEDURE web_api-pricing-preuni-ctouni :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Sintaxis
http://192.168.0.237:62/api/storeprocedure/XML
/002056
/1
/C
/000
*/
DEF INPUT PARAMETER pArtCode AS CHAR.
DEF INPUT PARAMETER pSalesChannel AS CHAR.
DEF INPUT PARAMETER pCategoryCustomer AS CHAR.
DEF INPUT PARAMETER pSalesCondition AS CHAR.
DEF OUTPUT PARAMETER pMonVta AS INTE NO-UNDO.
DEF OUTPUT PARAMETER pTpoCmb AS DECI NO-UNDO.
DEF OUTPUT PARAMETER pPrecioDescontado AS DECI NO-UNDO.
DEF OUTPUT PARAMETER pRepositionCostInCurrencySale AS DECI NO-UNDO.
DEF OUTPUT PARAMETER pMessage AS CHAR NO-UNDO.

/* ******************************************************************************************* */
/* Valores por defecto */
/* ******************************************************************************************* */
IF TRUE <> (pSalesChannel > "") THEN pSalesChannel = "6".           /* Tiendas Mayoristas */
IF TRUE <> (pCategoryCustomer > "") THEN pCategoryCustomer = "C".   /* Por defecto */
IF TRUE <> (pSalesCondition > "") THEN pSalesCondition = "000".     /* Contado */  
/* ******************************************************************************************* */

DEFINE VAR x-Url AS LONGCHAR.
DEFINE VAR x-Xml AS LONGCHAR.
DEFINE VAR x-Texto AS CHAR.

FIND FIRST VtaTabla WHERE VtaTabla.CodCia = s-CodCia
    AND VtaTabla.Tabla = 'CONFIG-WEB-PRICING' NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaTabla THEN DO:
    pMessage = "NO se encontr� la configuraci�n CONFIG-WEB-PRICING".
    RETURN "ADM-ERROR".
END.

/* Capturamos URL y TOKEN */
x-Url = TRIM(VtaTabla.Llave_c1) +       /* URL */
        TRIM(VtaTabla.Llave_c2) .       /* Token */
/* Llave de b�squeda */
x-Url = x-Url + ~
'/' + pArtCode + ~
'/' + pSalesChannel + ~
'/' + pCategoryCustomer + ~
'/' + pSalesCondition.

/* Capturamos el API */
DEF VAR pResult AS CHAR NO-UNDO.
DEF VAR pResponse AS LONGCHAR NO-UNDO.
DEF VAR pContent AS LONGCHAR NO-UNDO.

RUN lib/http-get-contenido.r(x-Url,
                             OUTPUT pResult,
                             OUTPUT pResponse,
                             OUTPUT pContent) 
    NO-ERROR.

IF pResult <> "1:Success"  THEN DO:
    /* ERROR DETECTADO */
    CASE pResult:
        WHEN "0:Not Content" THEN DO:
            pMessage = "VALIDACION: Precio Pelda�o Bruto NO disponible para la venta" + CHR(10) + 
                "Porque NO est� configurado el precio pelda�o bruto" + CHR(10) + 
                "NO disponible para la venta" + CHR(10) + 
                "Art�culo: " + pArtCode + CHR(10) +
                "Pelda�o: " + pSalesChannel + CHR(10) +
                "Categor�a del cliente: " + pCategoryCustomer + CHR(10) +
                "Condici�n de venta: " + pSalesCondition + CHR(10) + CHR(10) +
                "Comunicarse con el Jefe de L�nea para su diagn�stico previo" + CHR(10) +
                "y de ser necesario se comunicar� con sistemas".
        END.
        WHEN "0:No Socket" THEN DO:
            pMessage = "ERROR AL ENVIAR TRAMA : " + CHR(10) + x-Url.
        END.
        WHEN "0:Failure" THEN DO:
            pMessage = "Se detecta demasiadas peticiones de uso del servidor de aplicaciones (Servidor de aplicaciones detenido o no existe)" + CHR(10) +
                x-Url + CHR(10) + CHR(10) +
                "Vuelva a intentar y de persistir el mismo mensaje, contactar a Sistemas".
        END.
        OTHERWISE DO:
            pMessage = "ERROR API : " + CHR(10) + x-Url + "Vuelva a intentar y de persistir el mismo mensaje, contactar a Sistemas".
        END.
    END CASE.
    RETURN "ADM-ERROR".
END.

/* Respuesta del API */
x-Xml = pContent.
x-Texto = STRING(x-Xml).

/* **************************************************************************************** */
/* Buscamos error */
/* **************************************************************************************** */
DEF VAR x-Cadena1 AS CHAR NO-UNDO.
DEF VAR x-Cadena2 AS CHAR NO-UNDO.
DEF VAR x-Inicio AS INTE NO-UNDO.
DEF VAR x-Fin AS INTE NO-UNDO.

IF INDEX(x-Texto, '<error>') > 0 THEN DO:
    x-Cadena1 = "<error>".
    x-Cadena2 = "</error>".
    x-Inicio = INDEX(x-Texto, x-Cadena1).
    x-Fin = INDEX(x-Texto, x-Cadena2).
    x-Texto = "API no responde".
    IF x-Inicio > x-Fin THEN DO:
        pMessage = "VALIDACION: Precio Pelda�o Bruto NO disponible para la venta" + CHR(10) + 
            "Porque NO est� configurado el precio pelda�o bruto" + CHR(10) + 
            "NO disponible para la venta" + CHR(10) + 
            "Art�culo: " + pArtCode + CHR(10) +
            "Pelda�o: " + pSalesChannel + CHR(10) +
            "Categor�a del cliente: " + pCategoryCustomer + CHR(10) +
            "Condici�n de venta: " + pSalesCondition + CHR(10) + CHR(10) +
            "Comunicarse con el Jefe de L�nea para su diagn�stico previo" + CHR(10) +
            "y de ser necesario se comunicar� con sistemas".
    END.
    ELSE DO:
        pMessage = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).
        pMessage = pMessage + CHR(10) + 
            "Porque NO est� configurado el precio pelda�o bruto" + CHR(10) + 
            "NO disponible para la venta" + CHR(10) + 
            "Art�culo: " + pArtCode + CHR(10) +
            "Pelda�o: " + pSalesChannel + CHR(10) +
            "Categor�a del cliente: " + pCategoryCustomer + CHR(10) +
            "Condici�n de venta: " + pSalesCondition + CHR(10) + CHR(10) +
            "Comunicarse con el Jefe de L�nea para su diagn�stico previo" + CHR(10) +
            "y de ser necesario se comunicar� con sistemas".
    END.
    RUN web_connection_log (x-Url, x-Texto).
    RETURN "ADM-ERROR".
END.
/* **************************************************************************************** */

DEF VAR Inicio AS INT64 NO-UNDO.
DEF VAR Fin AS INT64 NO-UNDO.

/* Separamos la DATA */
x-Inicio = INDEX(x-Texto, "<Data>").
x-Fin = INDEX(x-Texto, "</Data>").

IF x-Inicio = 0 OR x-Fin = 0 THEN DO:
    pMessage = "XML no retorn� ninguna informaci�n" + CHR(10) +
        "Vuelva a intentar y de persistir el mismo mensaje, contactar a Sistemas".
    RUN web_connection_log (x-Url, "XML no retorn� ninguna informaci�n").
    RETURN "ADM-ERROR".
END.

x-Texto = SUBSTRING(x-Texto, x-Inicio + 6, (x-Fin - x-Inicio - 6)).

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
    pMessage = "Precio Unitario Pelda�o Bruto: NO registrado en la lista de precios".
    RETURN "ADM-ERROR".
END.
x-PrecioDescontado = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

pPrecioDescontado = DECIMAL(x-PrecioDescontado) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    pMessage = "Precio Unitario Pelda�o no est� en formato de n�meros" + CHR(10) + x-texto.
    RETURN "ADM-ERROR".
END.
IF pPrecioDescontado <= 0 THEN DO:
    pMessage = "Precio Pelda�o Bruto est� mal configurado" + CHR(10) + CHR(10) +     
        "Por favor consultar al Jefe de L�nea". 
    RETURN 'ADM-ERROR'.
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
    pMessage = "Costo de Reposici�n: NO registrado en la lista de precios".
    RETURN "ADM-ERROR".
END.
x-CostoReposicion = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

pRepositionCostInCurrencySale = DECIMAL(x-CostoReposicion) NO-ERROR.
IF ERROR-STATUS:ERROR = YES THEN DO:
    pMessage = "Art�culo: " + pArtCode + CHR(10) +
                "Costo de Reposici�n no est� en el formato correcto" + CHR(10) + x-texto.
    RETURN "ADM-ERROR".
END.

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-web_api-pricing-preuni-ctouni-old) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE web_api-pricing-preuni-ctouni-old Procedure 
PROCEDURE web_api-pricing-preuni-ctouni-old :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Sintaxis
http://192.168.0.237:62/api/storeprocedure/XML
/002056
/1
/C
/000
*/
DEF INPUT PARAMETER pArtCode AS CHAR.
DEF INPUT PARAMETER pSalesChannel AS CHAR.
DEF INPUT PARAMETER pCategoryCustomer AS CHAR.
DEF INPUT PARAMETER pSalesCondition AS CHAR.
DEF OUTPUT PARAMETER pMonVta AS INTE NO-UNDO.
DEF OUTPUT PARAMETER pTpoCmb AS DECI NO-UNDO.
DEF OUTPUT PARAMETER pPrecioDescontado AS DECI NO-UNDO.
DEF OUTPUT PARAMETER pRepositionCostInCurrencySale AS DECI NO-UNDO.
DEF OUTPUT PARAMETER pMessage AS CHAR NO-UNDO.

/* ******************************************************************************************* */
/* Valores por defecto */
/* ******************************************************************************************* */
IF TRUE <> (pSalesChannel > "") THEN pSalesChannel = "6".           /* Tiendas Mayoristas */
IF TRUE <> (pCategoryCustomer > "") THEN pCategoryCustomer = "C".   /* Por defecto */
IF TRUE <> (pSalesCondition > "") THEN pSalesCondition = "000".     /* Contado */  
/* ******************************************************************************************* */

DEFINE VAR x-Url AS LONGCHAR.
DEFINE VAR x-Xml AS LONGCHAR.
DEFINE VAR x-Texto AS CHAR.

FIND FIRST VtaTabla WHERE VtaTabla.CodCia = s-CodCia
    AND VtaTabla.Tabla = 'CONFIG-WEB-PRICING' NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaTabla THEN DO:
    pMessage = "NO se encontr� la configuraci�n CONFIG-WEB-PRICING".
    RETURN "ADM-ERROR".
END.

/* Capturamos URL y TOKEN */
x-Url = TRIM(VtaTabla.Llave_c1) +       /* URL */
        TRIM(VtaTabla.Llave_c2) .       /* Token */
/* Llave de b�squeda */
x-Url = x-Url + ~
'/' + pArtCode + ~
'/' + pSalesChannel + ~
'/' + pCategoryCustomer + ~
'/' + pSalesCondition.

/* DEF VAR x-editor AS CHAR VIEW-AS EDITOR SIZE 60 BY 6. */
/* x-editor = string(x-url).                             */
/* UPDATE x-editor.                                      */
/* RETURN.                                               */

/* Capturamos el API */
DEFINE VAR x-oXmlHttp AS COM-HANDLE NO-UNDO.

CREATE "MSXML2.ServerXMLHTTP.6.0" x-oXmlHttp.

x-oXmlHttp:OPEN( "GET", x-Url, NO ). 
x-oXmlHttp:setRequestHeader( "Content-Type", "application/xml;charset=utf-8" ).
x-oXmlHttp:setOption( 2, 13056 ) .  
x-oXmlHttp:SEND() NO-ERROR.


IF ERROR-STATUS:GET-NUMBER(1) > 0 THEN DO:
    pMessage = "Se detecta demasiadas peticiones de uso del servidor de aplicaciones (Servidor de aplicaciones detenido o no existe)" + CHR(10) +
        x-Url + CHR(10) + CHR(10) +
        "Vuelva a intentar y de persistir el mismo mensaje, contactar a Sistemas".
    RUN web_connection_log (x-Url, ERROR-STATUS:GET-MESSAGE(1)).
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN 'ADM-ERROR'.
END.
IF x-oXmlHttp:STATUS <> 200 THEN DO:
    pMessage = "ERROR AL ENVIAR TRAMA : " + CHR(10) + x-oXmlHttp:responseText.
    RUN web_connection_log (x-Url, "ERROR AL ENVIAR TRAMA : " + x-oXmlHttp:responseText).
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN 'ADM-ERROR'.
END.
/* Respuesta del API */
x-Xml = x-oXmlHttp:responseText.
x-Texto = STRING(x-Xml).
/*
MESSAGE string(x-Url) SKIP
        x-Texto.
*/
/* **************************************************************************************** */
/* Buscamos error */
/* **************************************************************************************** */
DEF VAR x-Cadena1 AS CHAR NO-UNDO.
DEF VAR x-Cadena2 AS CHAR NO-UNDO.
DEF VAR x-Inicio AS INTE NO-UNDO.
DEF VAR x-Fin AS INTE NO-UNDO.

IF INDEX(x-Texto, '<error>') > 0 THEN DO:
    x-Cadena1 = "<error>".
    x-Cadena2 = "</error>".
    x-Inicio = INDEX(x-Texto, x-Cadena1).
    x-Fin = INDEX(x-Texto, x-Cadena2).
    x-Texto = "API no responde".
    IF x-Inicio > x-Fin THEN DO:
        pMessage = "VALIDACION: Precio Pelda�o Bruto NO disponible para la venta" + CHR(10) + 
            "Porque NO est� configurado el precio pelda�o bruto" + CHR(10) + 
            "NO disponible para la venta" + CHR(10) + 
            "Art�culo: " + pArtCode + CHR(10) +
            "Pelda�o: " + pSalesChannel + CHR(10) +
            "Categor�a del cliente: " + pCategoryCustomer + CHR(10) +
            "Condici�n de venta: " + pSalesCondition + CHR(10) + CHR(10) +
            "Comunicarse con el Jefe de L�nea para su diagn�stico previo" + CHR(10) +
            "y de ser necesario se comunicar� con sistemas".
    END.
    ELSE DO:
        pMessage = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).
        pMessage = pMessage + CHR(10) + 
            "Porque NO est� configurado el precio pelda�o bruto" + CHR(10) + 
            "NO disponible para la venta" + CHR(10) + 
            "Art�culo: " + pArtCode + CHR(10) +
            "Pelda�o: " + pSalesChannel + CHR(10) +
            "Categor�a del cliente: " + pCategoryCustomer + CHR(10) +
            "Condici�n de venta: " + pSalesCondition + CHR(10) + CHR(10) +
            "Comunicarse con el Jefe de L�nea para su diagn�stico previo" + CHR(10) +
            "y de ser necesario se comunicar� con sistemas".
    END.
    RUN web_connection_log (x-Url, x-Texto).
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
/* **************************************************************************************** */

DEF VAR Inicio AS INT64 NO-UNDO.
DEF VAR Fin AS INT64 NO-UNDO.

/* Separamos la DATA */
x-Inicio = INDEX(x-Texto, "<Data>").
x-Fin = INDEX(x-Texto, "</Data>").

IF x-Inicio = 0 OR x-Fin = 0 THEN DO:
    pMessage = "XML no retorn� ninguna informaci�n" + CHR(10) +
        "Vuelva a intentar y de persistir el mismo mensaje, contactar a Sistemas".
    RUN web_connection_log (x-Url, "XML no retorn� ninguna informaci�n").
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.

x-Texto = SUBSTRING(x-Texto, x-Inicio + 6, (x-Fin - x-Inicio - 6)).

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
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
x-MonVta = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

pMonVta = LOOKUP(x-MonVta, x-Monedas).
IF pMonVta = 0 THEN DO:
    pMessage = "Art�culo: " + pArtCode + CHR(10) +
                "Moneda de Venta no est� en el formato correcto" + CHR(10) + x-texto.
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
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
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
x-TpoCmb = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

pTpoCmb = DECIMAL(x-TpoCmb) NO-ERROR.
IF ERROR-STATUS:ERROR = YES THEN DO:
    pMessage = "Art�culo: " + pArtCode + CHR(10) +
                "Tipo de Cambio no est� en el formato correcto" + CHR(10) + x-texto.
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
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
    pMessage = "Precio Unitario Pelda�o Bruto: NO registrado en la lista de precios".
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
x-PrecioDescontado = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

pPrecioDescontado = DECIMAL(x-PrecioDescontado) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    pMessage = "Precio Unitario Pelda�o no est� en formato de n�meros" + CHR(10) + x-texto.
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
IF pPrecioDescontado <= 0 THEN DO:
    pMessage = "Precio Pelda�o Bruto est� mal configurado" + CHR(10) + CHR(10) +     
        "Por favor consultar al Jefe de L�nea". 
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN 'ADM-ERROR'.
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
    pMessage = "Costo de Reposici�n: NO registrado en la lista de precios".
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
x-CostoReposicion = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

pRepositionCostInCurrencySale = DECIMAL(x-CostoReposicion) NO-ERROR.
IF ERROR-STATUS:ERROR = YES THEN DO:
    pMessage = "Art�culo: " + pArtCode + CHR(10) +
                "Costo de Reposici�n no est� en el formato correcto" + CHR(10) + x-texto.
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.

RELEASE OBJECT x-oXmlHttp NO-ERROR.

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-web_api-pricing-preuni-old) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE web_api-pricing-preuni-old Procedure 
PROCEDURE web_api-pricing-preuni-old :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Devuelve el precio unitario ya afectado por el descuento x clf y x cond vta
------------------------------------------------------------------------------*/
/* Sintaxis
http://192.168.0.237:62/api/storeprocedure/XML
/002056
/1
/C
/000
*/
DEF INPUT PARAMETER pArtCode AS CHAR.
DEF INPUT PARAMETER pSalesChannel AS CHAR.
DEF INPUT PARAMETER pCategoryCustomer AS CHAR.
DEF INPUT PARAMETER pSalesCondition AS CHAR.
DEF OUTPUT PARAMETER pMonVta AS INTE NO-UNDO.
DEF OUTPUT PARAMETER pTpoCmb AS DECI NO-UNDO.
DEF OUTPUT PARAMETER pPrecioDescontado AS DECI NO-UNDO.
DEF OUTPUT PARAMETER pMessage AS CHAR NO-UNDO.

/*MESSAGE s-aplic-id s-prog-name.*/

/* */
/* IF s-user-id = 'ADMIN' THEN DO: */
/*     pMonVta = 1.                */
/*     ptpoCmb = 1.                */
/*     pPrecioDescontado = 91.35.  */
/*     RETURN 'OK'.                */
/* END.                            */
/* */
DEFINE VAR x-Url AS LONGCHAR.
DEFINE VAR x-Xml AS LONGCHAR.
DEFINE VAR x-Texto AS CHAR.

FIND FIRST VtaTabla WHERE VtaTabla.CodCia = s-CodCia
    AND VtaTabla.Tabla = 'CONFIG-WEB-PRICING' NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaTabla THEN DO:
    pMessage = "NO se encontr� la configuraci�n CONFIG-WEB-PRICING".
    RETURN "ADM-ERROR".
END.

/* Capturamos URL y TOKEN */
x-Url = TRIM(VtaTabla.Llave_c1) +       /* URL */
        TRIM(VtaTabla.Llave_c2) .       /* Token */
/* Llave de b�squeda */
x-Url = x-Url + ~
'/' + pArtCode + ~
'/' + pSalesChannel + ~
'/' + pCategoryCustomer + ~
'/' + pSalesCondition.

/* DEF VAR x-editor AS CHAR VIEW-AS EDITOR SIZE 60 BY 6. */
/* x-editor = string(x-url).                             */
/* UPDATE x-editor.                                      */
/* RETURN.                                               */

/* Capturamos el API */
DEFINE VAR x-oXmlHttp AS COM-HANDLE NO-UNDO.

CREATE "MSXML2.ServerXMLHTTP.6.0" x-oXmlHttp.

RUN lib/p-write-log-txt.p("VENTAS-UTILEX","CODIGO:" + pArtCode + ' api precio - OPEN').
x-oXmlHttp:OPEN( "GET", x-Url, NO ). 
RUN lib/p-write-log-txt.p("VENTAS-UTILEX","CODIGO:" + pArtCode + ' api precio - setRequestHeader').
x-oXmlHttp:setRequestHeader( "Content-Type", "application/xml;charset=utf-8" ).
RUN lib/p-write-log-txt.p("VENTAS-UTILEX","CODIGO:" + pArtCode + ' api precio - setOption').
x-oXmlHttp:setOption( 2, 13056 ) .  
RUN lib/p-write-log-txt.p("VENTAS-UTILEX","CODIGO:" + pArtCode + ' api precio - SEND').
x-oXmlHttp:SEND() NO-ERROR.
RUN lib/p-write-log-txt.p("VENTAS-UTILEX","CODIGO:" + pArtCode + ' api precio - SEND-FIN').

IF ERROR-STATUS:GET-NUMBER(1) > 0 THEN DO:
    pMessage = "Se detecta demasiadas peticiones de uso del servidor de aplicaciones (Servidor de aplicaciones detenido o no existe)" + CHR(10) +
        x-Url + CHR(10) + CHR(10) +
        "Vuelva a intentar y de persistir el mismo mensaje, contactar a Sistemas".
    RUN web_connection_log (x-Url, ERROR-STATUS:GET-MESSAGE(1)).
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN 'ADM-ERROR'.
END.
IF x-oXmlHttp:STATUS <> 200 THEN DO:
    pMessage = "ERROR AL ENVIAR TRAMA : " + CHR(10) + x-oXmlHttp:responseText.
    RUN web_connection_log (x-Url, "ERROR AL ENVIAR TRAMA : " + x-oXmlHttp:responseText).
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN 'ADM-ERROR'.
END.
/* Respuesta del API */
x-Xml = x-oXmlHttp:responseText.
x-Texto = STRING(x-Xml).
/*
MESSAGE string(x-Url) SKIP
        x-Texto.
*/
/* **************************************************************************************** */
/* Buscamos error */
/* **************************************************************************************** */
DEF VAR x-Cadena1 AS CHAR NO-UNDO.
DEF VAR x-Cadena2 AS CHAR NO-UNDO.
DEF VAR x-Inicio AS INTE NO-UNDO.
DEF VAR x-Fin AS INTE NO-UNDO.

IF INDEX(x-Texto, '<error>') > 0 THEN DO:
    x-Cadena1 = "<error>".
    x-Cadena2 = "</error>".
    x-Inicio = INDEX(x-Texto, x-Cadena1).
    x-Fin = INDEX(x-Texto, x-Cadena2).
    x-Texto = "API no responde".
    IF x-Inicio > x-Fin THEN DO:
        pMessage = "VALIDACION: Precio Pelda�o Bruto NO disponible para la venta" + CHR(10) + 
            "Porque NO est� configurado el precio pelda�o bruto" + CHR(10) + 
            "NO disponible para la venta" + CHR(10) + 
            "Art�culo: " + pArtCode + CHR(10) +
            "Pelda�o: " + pSalesChannel + CHR(10) +
            "Categor�a del cliente: " + pCategoryCustomer + CHR(10) +
            "Condici�n de venta: " + pSalesCondition + CHR(10) + CHR(10) +
            "Comunicarse con el Jefe de L�nea para su diagn�stico previo" + CHR(10) +
            "y de ser necesario se comunicar� con sistemas".
    END.
    ELSE DO:
        pMessage = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).
        pMessage = pMessage + CHR(10) + 
            "Porque NO est� configurado el precio pelda�o bruto" + CHR(10) + 
            "NO disponible para la venta" + CHR(10) + 
            "Art�culo: " + pArtCode + CHR(10) +
            "Pelda�o: " + pSalesChannel + CHR(10) +
            "Categor�a del cliente: " + pCategoryCustomer + CHR(10) +
            "Condici�n de venta: " + pSalesCondition + CHR(10) + CHR(10) +
            "Comunicarse con el Jefe de L�nea para su diagn�stico previo" + CHR(10) +
            "y de ser necesario se comunicar� con sistemas".
    END.
    RUN web_connection_log (x-Url, x-Texto).
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
/* **************************************************************************************** */

DEF VAR Inicio AS INT64 NO-UNDO.
DEF VAR Fin AS INT64 NO-UNDO.

/* Separamos la DATA */
x-Inicio = INDEX(x-Texto, "<Data>").
x-Fin = INDEX(x-Texto, "</Data>").

IF x-Inicio = 0 OR x-Fin = 0 THEN DO:
    pMessage = "XML no retorn� ninguna informaci�n" + CHR(10) +
        "Vuelva a intentar y de persistir el mismo mensaje, contactar a Sistemas".
    RUN web_connection_log (x-Url, "XML no retorn� ninguna informaci�n").
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.

x-Texto = SUBSTRING(x-Texto, x-Inicio + 6, (x-Fin - x-Inicio - 6)).

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
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
x-MonVta = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

pMonVta = LOOKUP(x-MonVta, x-Monedas).
IF pMonVta = 0 THEN DO:
    pMessage = "Art�culo: " + pArtCode + CHR(10) +
                "Moneda de Venta no est� en el formato correcto" + CHR(10) + x-texto.
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
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
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
x-TpoCmb = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

pTpoCmb = DECIMAL(x-TpoCmb) NO-ERROR.
IF ERROR-STATUS:ERROR = YES THEN DO:
    pMessage = "Art�culo: " + pArtCode + CHR(10) +
                "Tipo de Cambio no est� en el formato correcto" + CHR(10) + x-texto.
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
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
    pMessage = "Precio Unitario Pelda�o Bruto: NO registrado en la lista de precios".
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
x-PrecioDescontado = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), (x-Fin - x-Inicio - LENGTH(x-Cadena1))).

pPrecioDescontado = DECIMAL(x-PrecioDescontado) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    pMessage = "Precio Unitario Pelda�o no est� en formato de n�meros" + CHR(10) + x-texto.
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
IF pPrecioDescontado <= 0 THEN DO:
    pMessage = "Precio Pelda�o Bruto est� mal configurado" + CHR(10) + CHR(10) +     
        "Por favor consultar al Jefe de L�nea". 
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN 'ADM-ERROR'.
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

END.
ELSE DO:

END.

RELEASE OBJECT x-oXmlHttp NO-ERROR.

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-web_api-riqra-import-horizontal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE web_api-riqra-import-horizontal Procedure 
PROCEDURE web_api-riqra-import-horizontal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Sintaxis
http://192.168.0.232:5000/synchronizefromsatellite/pedidossatelitehorizontal?codvendor=690
*/

DEF INPUT PARAMETER pCodVendedor AS CHAR.
DEF OUTPUT PARAMETER pMessage AS CHAR NO-UNDO.

/* */
DEFINE VAR x-Url AS LONGCHAR.
DEFINE VAR x-Xml AS LONGCHAR.
DEFINE VAR x-Texto AS CHAR.

FIND FIRST VtaTabla WHERE VtaTabla.CodCia = s-CodCia
    AND VtaTabla.Tabla = 'CONFIG-WEB-RIQRA-HORIZONTAL' 
    AND VtaTabla.Llave_c1 = 'PPX' NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaTabla OR TRUE <> (VtaTabla.Libre_c01 > '') THEN DO:
    pMessage = "NO se encontr� la configuraci�n CONFIG-WEB-RIQRA PPX".
    RETURN "ADM-ERROR".
END.

/* Capturamos URL y TOKEN */
x-Url = TRIM(VtaTabla.Libre_c01).    /* URL */

/* Llave de b�squeda */
x-Url = x-Url + '?' + 'codvendor=' + pCodVendedor.
/*MESSAGE STRING(x-url).*/
/* Capturamos el API */

DEFINE VAR x-oXmlHttp AS COM-HANDLE NO-UNDO.

CREATE "MSXML2.ServerXMLHTTP.6.0" x-oXmlHttp.

x-oXmlHttp:OPEN( "GET", x-Url, NO ). 
x-oXmlHttp:setRequestHeader( "Content-Type", "application/xml;charset=utf-8" ).
x-oXmlHttp:setOption( 2, 13056 ) .  
x-oXmlHttp:SEND() NO-ERROR.

IF ERROR-STATUS:GET-NUMBER(1) > 0 THEN DO:
    pMessage = "Se detecta demasiadas peticiones de uso del servidor de aplicaciones (Servidor de aplicaciones detenido o no existe)" + CHR(10) +
        x-Url + CHR(10) + CHR(10) +
        "Vuelva a intentar y de persistir el mismo mensaje, contactar a Sistemas".
    RUN web_connection_log (x-Url, ERROR-STATUS:GET-MESSAGE(1)).
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN 'ADM-ERROR'.
END.

DEF VAR xyz AS LONGCHAR.
xyz = x-oXmlHttp:responseText.
/*
MESSAGE "xyz" SKIP 
     STRING(xyz).   
*/
IF x-oXmlHttp:STATUS <> 200 THEN DO:
    pMessage = "ERROR AL ENVIAR TRAMA : " + CHR(10) + x-oXmlHttp:responseText.
    RUN web_connection_log (x-Url, "ERROR AL ENVIAR TRAMA : " + x-oXmlHttp:responseText).
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN 'ADM-ERROR'.
END.
/* Respuesta del API */
x-Xml = x-oXmlHttp:responseText.
x-Texto = STRING(x-Xml).

/* **************************************************************************************** */
/* Buscamos error */
/* **************************************************************************************** */
DEF VAR x-Cadena1 AS CHAR NO-UNDO.
DEF VAR x-Cadena2 AS CHAR NO-UNDO.
DEF VAR x-Inicio AS INTE NO-UNDO.
DEF VAR x-Fin AS INTE NO-UNDO.

IF INDEX(x-Texto, '<error>') > 0 THEN DO:
    x-Cadena1 = "<error>".
    x-Cadena2 = "</error>".
    x-Inicio = INDEX(x-Texto, x-Cadena1).
    x-Fin = INDEX(x-Texto, x-Cadena2).
    x-Texto = "API no responde (CONFIG-WEB-RIQRA-HORIZONTAL)".
    pMessage = x-Texto.
    RUN web_connection_log (x-Url, x-Texto).
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
/* **************************************************************************************** */
RELEASE OBJECT x-oXmlHttp NO-ERROR.

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-web_api-riqra-import-ppx) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE web_api-riqra-import-ppx Procedure 
PROCEDURE web_api-riqra-import-ppx :
/*------------------------------------------------------------------------------
  Purpose:     Importa la informaci�n de RIQRA y a PROGRESS PPX
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Sintaxis
http://192.168.0.232:5000/synchronizefromsatellite/pedidossateliteferias?codevento=<division>&codclie=<cliente>
*/

DEF INPUT PARAMETER pCodDiv AS CHAR.
DEF INPUT PARAMETER pCustomer AS CHAR.

DEF OUTPUT PARAMETER pMessage AS CHAR NO-UNDO.

/* */
DEFINE VAR x-Url AS LONGCHAR.
DEFINE VAR x-Xml AS LONGCHAR.
DEFINE VAR x-Texto AS CHAR.

FIND FIRST VtaTabla WHERE VtaTabla.CodCia = s-CodCia
    AND VtaTabla.Tabla = 'CONFIG-WEB-RIQRA' 
    AND VtaTabla.Llave_c1 = 'PPX' NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaTabla OR TRUE <> (VtaTabla.Libre_c01 > '') THEN DO:
    pMessage = "NO se encontr� la configuraci�n CONFIG-WEB-RIQRA PPX".
    RETURN "ADM-ERROR".
END.

/* Capturamos URL y TOKEN */
x-Url = TRIM(VtaTabla.Libre_c01).    /* URL */

/* Llave de b�squeda */
x-Url = x-Url + '?' + 'codevento=' + pCodDiv + '&' + 'codclie=' + pCustomer.
/*MESSAGE STRING(x-url).*/
/* Capturamos el API */
DEFINE VAR x-oXmlHttp AS COM-HANDLE NO-UNDO.

CREATE "MSXML2.ServerXMLHTTP.6.0" x-oXmlHttp.

x-oXmlHttp:OPEN( "GET", x-Url, NO ). 
x-oXmlHttp:setRequestHeader( "Content-Type", "application/xml;charset=utf-8" ).
x-oXmlHttp:setOption( 2, 13056 ) .  
x-oXmlHttp:SEND() NO-ERROR.

IF ERROR-STATUS:GET-NUMBER(1) > 0 THEN DO:
    pMessage = "Se detecta demasiadas peticiones de uso del servidor de aplicaciones (Servidor de aplicaciones detenido o no existe)" + CHR(10) +
        x-Url + CHR(10) + CHR(10) +
        "Vuelva a intentar y de persistir el mismo mensaje, contactar a Sistemas".
    RUN web_connection_log (x-Url, ERROR-STATUS:GET-MESSAGE(1)).
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN 'ADM-ERROR'.
END.

DEF VAR xyz AS LONGCHAR.
xyz = x-oXmlHttp:responseText.

IF x-oXmlHttp:STATUS <> 200 THEN DO:
    pMessage = "ERROR AL ENVIAR TRAMA : " + CHR(10) + x-oXmlHttp:responseText.
    RUN web_connection_log (x-Url, "ERROR AL ENVIAR TRAMA : " + x-oXmlHttp:responseText).
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN 'ADM-ERROR'.
END.
/* Respuesta del API */
x-Xml = x-oXmlHttp:responseText.
x-Texto = STRING(x-Xml).

/* **************************************************************************************** */
/* Buscamos error */
/* **************************************************************************************** */
DEF VAR x-Cadena1 AS CHAR NO-UNDO.
DEF VAR x-Cadena2 AS CHAR NO-UNDO.
DEF VAR x-Inicio AS INTE NO-UNDO.
DEF VAR x-Fin AS INTE NO-UNDO.

IF INDEX(x-Texto, '<error>') > 0 THEN DO:
    x-Cadena1 = "<error>".
    x-Cadena2 = "</error>".
    x-Inicio = INDEX(x-Texto, x-Cadena1).
    x-Fin = INDEX(x-Texto, x-Cadena2).
    x-Texto = "API no responde (CONFIG-WEB-RIQRA)".
    pMessage = x-Texto.
    RUN web_connection_log (x-Url, x-Texto).
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
/* **************************************************************************************** */
RELEASE OBJECT x-oXmlHttp NO-ERROR.

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-web_api-riqra-stock-disponible) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE web_api-riqra-stock-disponible Procedure 
PROCEDURE web_api-riqra-stock-disponible :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Sintaxis
http://192.168.100.170:4500/api/stock-aviable?stockdepo=11&artcode=061112
*/

DEF INPUT PARAMETER pCodAlm AS CHAR.
DEF INPUT PARAMETER pCodMat AS CHAR.
DEF INPUT PARAMETER pCanDes AS DECI.

DEF OUTPUT PARAMETER pMessage AS CHAR NO-UNDO.

/* */
DEFINE VAR x-Url AS LONGCHAR.
DEFINE VAR x-Xml AS LONGCHAR.
DEFINE VAR x-Texto AS CHAR.

FIND FIRST VtaTabla WHERE VtaTabla.CodCia = s-CodCia
    AND VtaTabla.Tabla = 'CONFIG-WEB-RIQRA-HORIZONTAL' 
    AND VtaTabla.Llave_c1 = 'STOCK-DISPONIBLE' NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaTabla OR TRUE <> (VtaTabla.Libre_c01 > '') THEN DO:
    pMessage = "NO se encontr� la configuraci�n CONFIG-WEB-RIQRA-HORIZONTAL STOCK-DISPONIBLE".
    RETURN "ADM-ERROR".
END.

/* Capturamos URL y TOKEN */
x-Url = TRIM(VtaTabla.Libre_c01).    /* URL */

/* Llave de b�squeda */
x-Url = x-Url + '?' + 'stockdepo=' + pCodAlm + '&artcode=' + pCodMat + '&qty=' + STRING(pCanDes).
/*MESSAGE STRING(x-url).*/
/* Capturamos el API */

DEFINE VAR x-oXmlHttp AS COM-HANDLE NO-UNDO.

CREATE "MSXML2.ServerXMLHTTP.6.0" x-oXmlHttp.

x-oXmlHttp:OPEN( "GET", x-Url, NO ). 
x-oXmlHttp:setRequestHeader( "Content-Type", "application/xml;charset=utf-8" ).
x-oXmlHttp:setOption( 2, 13056 ) .  
x-oXmlHttp:SEND() NO-ERROR.

IF ERROR-STATUS:GET-NUMBER(1) > 0 THEN DO:
    pMessage = "Se detecta demasiadas peticiones de uso del servidor de aplicaciones (Servidor de aplicaciones detenido o no existe)" + CHR(10) +
        x-Url + CHR(10) + CHR(10) +
        "Vuelva a intentar y de persistir el mismo mensaje, contactar a Sistemas".
    RUN web_connection_log (x-Url, ERROR-STATUS:GET-MESSAGE(1)).
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN 'ADM-ERROR'.
END.

DEF VAR xyz AS LONGCHAR.
xyz = x-oXmlHttp:responseText.
/*
MESSAGE "xyz" SKIP 
     STRING(xyz).   
*/
IF x-oXmlHttp:STATUS <> 200 THEN DO:
    pMessage = "ERROR AL ENVIAR TRAMA : " + CHR(10) + x-oXmlHttp:responseText.
    RUN web_connection_log (x-Url, "ERROR AL ENVIAR TRAMA : " + x-oXmlHttp:responseText).
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN 'ADM-ERROR'.
END.
/* Respuesta del API */
x-Xml = x-oXmlHttp:responseText.
x-Texto = STRING(x-Xml).

/* **************************************************************************************** */
/* Buscamos error */
/* **************************************************************************************** */
DEF VAR x-Cadena1 AS CHAR NO-UNDO.
DEF VAR x-Cadena2 AS CHAR NO-UNDO.
DEF VAR x-Inicio AS INTE NO-UNDO.
DEF VAR x-Fin AS INTE NO-UNDO.

IF INDEX(x-Texto, '<error>') > 0 THEN DO:
    x-Cadena1 = "<error>".
    x-Cadena2 = "</error>".
    x-Inicio = INDEX(x-Texto, x-Cadena1).
    x-Fin = INDEX(x-Texto, x-Cadena2).
    x-Texto = "API no responde (CONFIG-WEB-RIQRA-HORIZONTAL)".
    pMessage = x-Texto.
    RUN web_connection_log (x-Url, x-Texto).
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN "ADM-ERROR".
END.
/* **************************************************************************************** */
RELEASE OBJECT x-oXmlHttp NO-ERROR.

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-web_api-satelite-vendedor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE web_api-satelite-vendedor Procedure 
PROCEDURE web_api-satelite-vendedor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Sintaxis
http://192.168.0.232:5000/otherqueries/sellerriqra?code=690&idenviroment=15
*/
DEF INPUT PARAMETER pCodVen AS CHAR.
DEF INPUT PARAMETER pIdEnviroment AS CHAR.  /* 15 para Horizontal */
DEF OUTPUT PARAMETER pMessage AS CHAR NO-UNDO.

pMessage = "0".
RETURN.

/* */
DEFINE VAR x-Url AS LONGCHAR.
DEFINE VAR x-Xml AS LONGCHAR.
DEFINE VAR x-Texto AS CHAR.

FIND FIRST VtaTabla WHERE VtaTabla.CodCia = s-CodCia
    AND VtaTabla.Tabla = 'CONFIG-WEB-SATELITE' 
    AND VtaTabla.Llave_c1 = "VENDEDOR"
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaTabla THEN DO:
    pMessage = "NO se encontr� la configuraci�n CONFIG-WEB-SATELITE VENDEDOR".
    RETURN "ADM-ERROR".
END.

/* Capturamos URL */
x-Url = TRIM(VtaTabla.Libre_c01).       /* URL */

/* Llave de b�squeda */
x-Url = x-Url + ~
    '?code=' + pCodVen +
    '&idenviroment=' + pIdEnviroment.

/* Capturamos el API */
DEF VAR pResult AS CHAR NO-UNDO.
DEF VAR pResponse AS LONGCHAR NO-UNDO.
DEF VAR pContent AS LONGCHAR NO-UNDO.

RUN lib/http-get-contenido.r(x-Url,
                             OUTPUT pResult,
                             OUTPUT pResponse,
                             OUTPUT pContent) 
    NO-ERROR.
IF TRIM(STRING(pContent)) = "[]"  THEN DO:     /* Devuelve una valor vac�o */
    pMessage = "0".     /* NO registrado como vendedor SATELITE */
    RETURN.
END.

pMessage = "1".    /* Detectado como vendedor SATELITE */

/* **************************************************************************************** */
/* Buscamos error */
/* **************************************************************************************** */
DEF VAR x-Cadena1 AS CHAR NO-UNDO.
DEF VAR x-Cadena2 AS CHAR NO-UNDO.
DEF VAR x-Inicio AS INTE NO-UNDO.
DEF VAR x-Fin AS INTE NO-UNDO.

DEF VAR Inicio AS INT64 NO-UNDO.
DEF VAR Fin AS INT64 NO-UNDO.

/* Separamos la DATA */
x-Cadena1 = '"status": '.
x-Texto = pContent.
x-Inicio = INDEX(x-Texto, x-Cadena1).

IF x-Inicio = 0 THEN DO:
    pMessage = "XML no retorn� ninguna informaci�n" + CHR(10) +
        "Vuelva a intentar y de persistir el mismo mensaje, contactar a Sistemas".
    RETURN "ADM-ERROR".
END.
x-Fin = INDEX(x-Texto, ",", x-Inicio).
x-Cadena2 = SUBSTRING(x-Texto, x-Inicio + LENGTH(x-Cadena1), x-Fin - x-Inicio - LENGTH(x-Cadena1)).
pMessage = x-Cadena2.   /* Puede ser "1" o "0" */
/*
"1": Vendedor activo en Sat�lite
"0": Vendedor NO activo en Sat�lite 
*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-web_connection_log) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE web_connection_log Procedure 
PROCEDURE web_connection_log :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pUrl AS CHAR.
DEFINE INPUT PARAMETER pError AS CHAR.

DEFINE VAR lClientComputerName  AS CHAR.
DEFINE VAR lClientName          AS CHAR.
DEFINE VAR lComputerName        AS CHAR.
DEFINE VAR lRemote-user         AS CHAR.
DEFINE VAR lxClientName         AS CHAR.

lClientComputerName = OS-GETENV ( "CLIENTCOMPUTERNAME").
lClientName         = OS-GETENV ( "CLIENTNAME").
lComputerName       = OS-GETENV ( "COMPUTERNAME").
lRemote-user        = OS-GETENV ( "REMOTE_USER").

lxClientName        = IF (lClientComputerName = ? OR lClientComputerName = "") THEN lClientName ELSE lClientComputerName.
lxClientName        = IF (CAPS(lxClientName) = "CONSOLE") THEN "" ELSE lxClientName.
lxClientName        = IF (lxClientName = ? OR lxClientName = "") THEN lComputerName ELSE lxClientName.

CREATE connection_log.
ASSIGN
    connection_log.connect_ClientType = s-user-id
    connection_log.connect_device = lxClientName
    connection_log.connect_estado = "FAIL_API_PRICING"
    /*connection_log.connect_id = */
    connection_log.connect_name = pUrl
    /*connection_log.connect_pid =*/
    connection_log.connect_thora_procesada = STRING(TIME, 'HH:MM:SS')
    connection_log.connect_time = STRING(TODAY, '99/99/9999')
    connection_log.connect_type = pError
    /*connection_log.connect_usr = */
    .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

