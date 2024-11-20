FOR EACH pf-g004 WHERE aplic-id = 'CBD' NO-LOCK:
    FIND DICTDB._user WHERE DICTDB._user._userid = PF-G004.User-Id NO-LOCK NO-ERROR.
    PUT UNFORMATTED
        PF-G004.User-Id  "?"
        PF-G004.Seguridad "?".
    IF AVAILABLE DICTDB._user THEN PUT UNFORMATTED DICTDB._user._user-name.
    PUT UNFORMATTED SKIP.
END.
