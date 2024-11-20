
DEFINE VARIABLE old_userid AS CHARACTER NO-UNDO.
DEFINE VARIABLE new_userid AS CHARACTER NO-UNDO.

old_userid = "CHD-14".
new_userid = "ARR-14".

DEFINE BUFFER b_almusers FOR almusers.

FOR EACH almusers where
    almusers.User-Id = old_userid:
    display almusers.user-id almusers.Codalm.
    CREATE b_almusers.
    BUFFER-COPY almusers TO b_almusers
        ASSIGN b_almusers.User-Id = new_userid.
END.

