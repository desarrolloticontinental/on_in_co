DEF VAR pPrecioDescontado AS DECI NO-UNDO.
DEF VAR pMessage AS CHAR NO-UNDO.
DEF VAR pMonVta AS INTE NO-UNDO.

DEFINE VAR hProc AS HANDLE NO-UNDO.

RUN web/web-library.p PERSISTENT SET hProc.

RUN web_api-pricing-preuni IN hProc ('027093',
                                     '8',
                                     'C',
                                     '001',
                                     OUTPUT pMonVta,
                                     OUTPUT pPrecioDescontado,
                                     OUTPUT pMessage).
DELETE PROCEDURE hProc.
MESSAGE ppreciodescontado SKIP pmessage.

/*
DEF INPUT PARAMETER pArtCode AS CHAR.
DEF INPUT PARAMETER pSalesChannel AS CHAR.
DEF INPUT PARAMETER pCategoryCustomer AS CHAR.
DEF INPUT PARAMETER pSalesCondition AS CHAR.
DEF OUTPUT PARAMETER pMonVta AS INTE NO-UNDO.
/*DEF OUTPUT PARAMETER pPrecioLista AS DECI NO-UNDO.*/
DEF OUTPUT PARAMETER pPrecioDescontado AS DECI NO-UNDO.
DEF OUTPUT PARAMETER pMessage AS CHAR NO-UNDO.
*/

