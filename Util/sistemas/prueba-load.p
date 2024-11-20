DEF VAR pPreUni AS DECI.
DEF VAR pMensaje AS CHAR.
DEF VAR pMonVta AS INTE.

DEFINE VAR hProc AS HANDLE NO-UNDO.

RUN web/web-library.p PERSISTENT SET hProc.

DEF VAR x-codmat AS CHAR.
DEF VAR x-exitos AS INTE.
DEF VAR x-intentos AS INTE.
DEF VAR a AS INT64.
DEF VAR x-clfcli AS CHAR INIT 'A,B,C'.
DEF VAR k AS INTE.

a = ETIME(YES).
INPUT FROM d:\top100.prn.
REPEAT:
    IMPORT UNFORMATTED x-codmat.
    IF TRUE <> (x-codmat > '') THEN LEAVE.

    DO k = 1 TO NUM-ENTRIES(x-clfcli):
        x-intentos = x-intentos + 1.
        RUN web_api-pricing-preuni IN hProc (x-codmat,
                                             "8",
                                             ENTRY(k,x-clfcli),
                                             "001",
                                             OUTPUT pMonVta,
                                             OUTPUT pPreUni,
                                             OUTPUT pMensaje).
        IF ppreuni > 0 THEN x-exitos = x-exitos + 1.
    END.
END.
INPUT CLOSE.

DELETE PROCEDURE hProc.

MESSAGE 'intentos' x-intentos 'éxitos' x-exitos (ETIME / 1000 / 60) 'minutos'.
