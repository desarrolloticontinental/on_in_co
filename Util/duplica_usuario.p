
DEFINE VARIABLE old_userid AS CHARACTER NO-UNDO.
DEFINE VARIABLE new_userid AS CHARACTER NO-UNDO.
DEFINE VARIABLE new_username AS CHARACTER NO-UNDO.

old_userid = "CHD-08".
new_userid = "ARR-08".
new_username = "Alfredo Rodriguez Rodriguez".

DEFINE BUFFER b_pf-g004 FOR pf-g004.

FOR EACH pf-g004 where
    PF-G004.User-Id = old_userid:
    display PF-G004.Aplic-Id PF-G004.User-Id.
    CREATE b_pf-g004.
    BUFFER-COPY PF-G004 TO b_pf-g004
        ASSIGN b_pf-g004.User-Id = new_userid.
    RUN crea_user.
END.

PROCEDURE crea_user:

    FIND DICTDB._user WHERE DICTDB._user._userid = new_userid NO-ERROR.
    IF NOT AVAILABLE DICTDB._user THEN DO:
        CREATE DICTDB._user.
        ASSIGN
            DICTDB._user._userid = new_userid
            DICTDB._User._Password = ENCODE("").
    END.
    ASSIGN DICTDB._user._user-name = new_username.

END PROCEDURE.
