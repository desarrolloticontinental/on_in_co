DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR x-usuario AS CHAR NO-UNDO.
DEF VAR x-fchcie AS DATE NO-UNDO.
DEF VAR x-horcie AS CHAR NO-UNDO.

ASSIGN
    x-usuario = 'ACC-00'
    x-fchcie = DATE(10, 09, 2014)
    x-horcie = '18:55'.

FOR EACH ccbccaja WHERE codcia = s-codcia
    AND flgcie = 'C'
    AND usuario = x-usuario
    AND fchcie = x-fchcie
    AND horcie = x-horcie:
    ccbccaja.flgcie = 'P'.
END.
FOR EACH ccbcierr WHERE ccbcierr.codcia = s-codcia
    AND CcbCierr.FchCie = x-fchcie
    AND CcbCierr.HorCie = x-horcie 
    AND CcbCierr.usuario = x-usuario:
    DELETE ccbcierr.
END.
FOR EACH ccbdecl WHERE ccbdecl.codcia = s-codcia
    AND ccbdecl.FchCie = x-fchcie
    AND ccbdecl.HorCie = x-horcie 
    AND ccbdecl.usuario = x-usuario:
    DELETE ccbdecl.
END.
