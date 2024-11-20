DEF VAR i AS INT NO-UNDO.
DEF VAR j AS INT NO-UNDO.
DEF VAR x-user-name AS CHAR NO-UNDO.

OUTPUT TO d:\tmp\accesos.txt.
rloop:
FOR EACH pf-g004 NO-LOCK:
    DO i = 1 TO NUM-ENTRIES(seguridad):
            x-user-name = ''.
            FIND FIRST _user WHERE _user._userid = pf-g004.USER-ID NO-LOCK NO-ERROR.
            IF AVAILABLE _user THEN x-user-name = _user._user-name.
            PUT UNFORMATTED
                pf-g004.USER-ID '|'
                x-user-name '|'
                pf-g004.seguridad
                SKIP.
    END.
END.
