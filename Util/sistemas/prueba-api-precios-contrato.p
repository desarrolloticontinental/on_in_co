DEF NEW SHARED VAR s-codcia AS INTE INIT 001.
DEF NEW SHARED VAR s-user-id AS CHAR INIT 'ADMIN'.
DEF VAR pMonVta AS INTE NO-UNDO.
DEF VAR pTpoCmb AS DECI NO-UNDO.
DEF VAR pPrecioDescontado AS DECI NO-UNDO.
DEF VAR pCtoUni AS DECI NO-UNDO.
DEF VAR pMessage AS CHAR NO-UNDO.
DEF VAR pUndVta AS CHAR NO-UNDO.

DEF VAR a AS INT64 NO-UNDO.

DEFINE VAR hProc AS HANDLE NO-UNDO.
RUN web/web-library PERSISTENT SET hProc.

a = ETIME(YES).

    RUN web_api-pricing-preuni-contrato IN hProc ('00047621400',
                                         '032776',
                                         OUTPUT pMonVta,
                                         OUTPUT pPrecioDescontado,
                                         OUTPUT pUndVta,
                                         OUTPUT pMessage).


DELETE PROCEDURE hProc.

a = ETIME.

MESSAGE a ppreciodescontado pmessage.



