DEF VAR  pMonVta AS INTE NO-UNDO.
DEF VAR  pTpoCmb AS DECI NO-UNDO.
DEF VAR  pPrecioDescontado AS DECI NO-UNDO.
DEF VAR  pMessage AS CHAR NO-UNDO.

DEFINE VAR hProc AS HANDLE NO-UNDO.

RUN web/web-library PERSISTENT SET hProc.

RUN web_api-pricing-ctoreposicion IN hProc ('00001',
                                            '601148',
                                     'C',  
                                     '000',
                                     OUTPUT pMonVta,
                                     OUTPUT pTpoCmb,
                                     OUTPUT pPrecioDescontado,
                                     OUTPUT pMessage).

DELETE PROCEDURE hProc.
MESSAGE pmonvta ptpocmb pPrecioDescontado SKIP pMessage.
