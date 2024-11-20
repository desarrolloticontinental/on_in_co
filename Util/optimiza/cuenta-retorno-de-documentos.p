DEF VAR x-cuenta-privado AS INT INIT 0.
DEF VAR x-cuenta-estado AS INT INIT 0.

FOR EACH di-rutac NO-LOCK WHERE codcia = 1
    AND coddiv = '00000'
    AND flgest = 'C'
    AND fchdoc >= 05/01/09
    AND fchdoc <= 05/30/09,
    EACH di-rutad OF di-rutac NO-LOCK WHERE di-rutad.flgest = 'D'
    OR di-rutad.flgest = 'X' OR di-rutad.flgest = 'R'
    OR di-rutad.flgest = 'NR',
    FIRST ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = di-rutad.codcia
    AND ccbcdocu.coddoc = di-rutad.codref
    AND ccbcdocu.nrodoc = di-rutad.nroref
    AND (codven = '110' OR codven = '185' or codven = '111'
         OR codven = '184' OR codven = '168'
         OR codven = '003'):
    x-cuenta-privado = x-cuenta-privado + 1.
END.
FOR EACH di-rutac NO-LOCK WHERE codcia = 1
    AND coddiv = '00000'
    AND flgest = 'C'
    AND fchdoc >= 05/01/09
    AND fchdoc <= 05/30/09,
    EACH di-rutad OF di-rutac NO-LOCK WHERE di-rutad.flgest = 'D'
    OR di-rutad.flgest = 'X' OR di-rutad.flgest = 'R'
    OR di-rutad.flgest = 'NR',
    FIRST ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = di-rutad.codcia
    AND ccbcdocu.coddoc = di-rutad.codref
    AND ccbcdocu.nrodoc = di-rutad.nroref
    AND (codven = '185' OR codven = '184'):
    x-cuenta-estado = x-cuenta-estado + 1.
END.
DISPLAY 'Retorno documentos sector privado' x-cuenta-privado SKIP
        'Retorno documentos sector estado'  x-cuenta-estado .

