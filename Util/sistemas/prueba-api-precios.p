DEF NEW SHARED VAR s-codcia AS INTE INIT 001.
DEF NEW SHARED VAR s-user-id AS CHAR INIT 'ADMIN'.
DEF VAR  pMonVta AS INTE NO-UNDO.
DEF VAR  pTpoCmb AS DECI NO-UNDO.
DEF VAR  pPrecioDescontado AS DECI NO-UNDO.
DEF VAR pCtoUni AS DECI NO-UNDO.
DEF VAR  pMessage AS CHAR NO-UNDO.

DEF VAR a AS INT64 NO-UNDO.

DEFINE VAR hProc AS HANDLE NO-UNDO.
RUN web/web-library PERSISTENT SET hProc.

a = ETIME(YES).

    RUN web_api-pricing-preuni IN hProc ('000421',
                                         '6',
                                         'C',  
                                         '001',
                                         OUTPUT pMonVta,
                                         OUTPUT pTpoCmb,
                                         OUTPUT pPrecioDescontado,
                                         OUTPUT pMessage).


DELETE PROCEDURE hProc.

a = ETIME.

MESSAGE a ppreciodescontado pmessage.



