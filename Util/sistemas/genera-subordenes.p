DEF NEW SHARED VAR s-codcia  AS INTE INIT 001.
DEF NEW SHARED VAR cl-codcia AS INTE INIT 000.
DEF NEW SHARED VAR s-user-id AS CHAR INIT 'MRC-00'.
DEF NEW SHARED VAR s-coddiv AS CHAR INIT '00000'.

DEFINE VAR hProc AS HANDLE NO-UNDO.
DEFINE VAR pMensaje AS CHAR NO-UNDO.

RUN gn/master-library PERSISTENT SET hProc.

FIND faccpedi WHERE codcia = 1
    AND coddoc = 'OTR'
    AND nroped = '814016399'
    NO-LOCK.
    
RUN Genera-SubOrden IN hProc (ROWID(Faccpedi),
                              INPUT Faccpedi.divdes,
                              OUTPUT pMensaje).

IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
    IF TRUE <> (pMensaje > '') THEN pMensaje = 'NO se pudo generar la sub-orden'.
    UNDO, RETURN 'ADM-ERROR'.
END.

DELETE PROCEDURE hProc.
