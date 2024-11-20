OUTPUT TO d:\tmp\usuarios-por-aplicacion.txt.
PUT UNFORMATTED
    "APLICACION|DESCRIPCION|USUARIO|NOMBRE|GRUPOS" SKIP.
FOR EACH pf-g004 NO-LOCK, 
    FIRST pf-g003 OF pf-g004 NO-LOCK,
    FIRST _user NO-LOCK WHERE _user._userid = pf-g004.USER-ID:
    PUT UNFORMATTED 
        PF-G004.Aplic-Id '|'
        PF-G003.Detalle '|'
        PF-G004.User-Id '|'
        _user._user-name '|'
        PF-G004.Seguridad
        SKIP.
END.
OUTPUT CLOSE.

