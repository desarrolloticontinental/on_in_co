DEFINE VAR x-url-webservice AS LONGCHAR.
DEFINE VAR x-oXmlHttp AS COM-HANDLE NO-UNDO.
                                 
x-url-webservice = "http://192.168.100.221:62/api/Sales/Record/PriceSalesChannelUnitView/sha256$5uqVbpQ6dWv0z2Tw$8cfcfcd13571eba4873e06dd6dd25674503c498657f7da6969aae41a0249c64a?FormatXML=True&FieldName=DiscountedPriceCategoryCustomerAndConditionSale,ExchangeRate,CurrencySale,PercentageDiscountSalesCondition,PercentageDiscountCategoryCustomer,CurrencyRepositionCost,AmountRepositionCost&ArtCode=000981&SalesChannel=1&CategoryCustomer=C&SalesCondition=001".



CREATE "MSXML2.ServerXMLHTTP.6.0" x-oXmlHttp.
                                 
x-oXmlHttp:OPEN( "GET", x-url-webservice, NO ). 

x-oXmlHttp:setRequestHeader( "Content-Type", "application/xml;charset=utf-8" ).
/*x-oXmlHttp:setRequestHeader( "Content-Length", LENGTH(x-xml-documento)).    */

x-oXmlHttp:setOption( 2, 13056 ) .  

x-oXmlHttp:SEND() NO-ERROR.

IF ERROR-STATUS:GET-NUMBER(1) > 0 THEN DO:
    /**/
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
END.
IF x-oXmlHttp:STATUS <> 200 THEN DO:
    MESSAGE "ERROR AL ENVIAR TRAMA : " SKIP 
        x-oXmlHttp:responseText VIEW-AS ALERT-BOX ERROR.
    RELEASE OBJECT x-oXmlHttp NO-ERROR.
    RETURN .
END.

/* La Respuesta */
DEFINE VAR x-rspta AS LONGCHAR.
DEFINE VAR x-orspta AS COM-HANDLE NO-UNDO.

x-rspta = x-oXmlHttp:responseText.

RELEASE OBJECT x-oXmlHttp NO-ERROR.
RELEASE OBJECT x-oRspta NO-ERROR.

MESSAGE STRING(x-rspta).
