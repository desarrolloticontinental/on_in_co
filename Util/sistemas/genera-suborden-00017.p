DEFINE VAR hProc AS HANDLE NO-UNDO.
DEFINE VAR pMensaje AS CHAR NO-UNDO.

DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF NEW SHARED VAR s-user-id AS CHAR INIT 'SYSTEM'.
DEF NEW SHARED VAR s-coddiv AS CHAR INIT '00000'.

RUN gn/master-library PERSISTENT SET hProc. 

FIND faccpedi WHERE codcia = s-codcia
    AND coddoc = 'o/d'
    AND nroped = '000128932'.

RUN Genera-SubOrden IN hProc (ROWID(Faccpedi),
                              '00000',
                              OUTPUT pMensaje).

DELETE PROCEDURE hProc.

MESSAGE 'Mensaje:' SKIP pMensaje.

