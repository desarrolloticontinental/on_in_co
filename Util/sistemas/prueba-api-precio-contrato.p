DEF VAR pmonvta AS INTE.
DEF VAR pPrecioDescontado AS DECI.
DEF VAR pMessage AS CHAR.
DEF VAR pundvta AS CHAR.

DEFINE VAR hProc AS HANDLE NO-UNDO.

RUN aplic/web/web-library.p PERSISTENT SET hProc.

RUN web_api-pricing-preuni-contrato IN hproc (
    '20100076072',
    '000752',
    OUTPUT pmonvta,
    OUTPUT pPrecioDescontado,
    OUTPUT pundvta,
    OUTPUT pMessage).

DELETE PROCEDURE hProc.
MESSAGE pmonvta SKIP
    ppreciodescontado SKIP
    '>' pundvta SKIP 
    '>>' pmessage.
