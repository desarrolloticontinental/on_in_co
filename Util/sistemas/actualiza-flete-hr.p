DEFINE VAR hProc AS HANDLE NO-UNDO.
DEF VAR pmensaje AS CHAR.
DEF NEW SHARED VAR s-coddiv AS CHAR.

FOR EACH flete_resumen_hr EXCLUSIVE-LOCK:
    DELETE flete_resumen_hr.
END.
FOR EACH flete_detallado_hr:
    DELETE flete_detallado_hr.
END.                        

RUN logis/logis-librerias PERSISTENT SET hProc.

FOR EACH di-rutac NO-LOCK WHERE codcia = 1
    /*AND coddiv = '00000' */
    AND coddoc = 'h/r'
    AND fchdoc >= DATE(07,01,2021):
    RUN FLETE_resumen-HR IN hProc (di-rutac.coddiv,
                                   di-rutac.coddoc,
                                   di-rutac.nrodoc,
                                   OUTPUT pmensaje).

END.


DELETE PROCEDURE hProc.

