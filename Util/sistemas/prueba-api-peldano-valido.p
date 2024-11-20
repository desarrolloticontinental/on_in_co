DEF VAR  pEstadoValido AS LOG NO-UNDO.
DEF VAR  pMessage AS CHAR NO-UNDO.

DEFINE VAR hProc AS HANDLE NO-UNDO.

RUN web/web-library PERSISTENT SET hProc.

RUN web_api-captura-peldano-valido IN hProc ('20015',
                                             OUTPUT pEstadoValido,
                                             OUTPUT pMessage).

DELETE PROCEDURE hProc.
MESSAGE pEstadoValido pMessage.
