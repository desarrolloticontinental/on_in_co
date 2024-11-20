DEF VAR i AS INT INIT 1.
FOR EACH ccbcmvto NO-LOCK WHERE codcia = 1
    AND coddoc = 'per':
    RUN vta2/r-imppercepcion ( ROWID(ccbcmvto ) ).
    i = i + 1.
    IF i > 2 THEN leave.
END.
