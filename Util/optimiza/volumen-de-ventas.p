DEF VAR x-privado AS INT INIT 0.
DEF VAR x-estado AS INT INIT 0.

FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND coddiv = '00000'
    AND coddoc = 'FAC'
    AND fchdoc >= 05/01/09
    AND fchdoc <= 05/30/09
    AND flgest <> 'A'
    AND (codven = '110' OR codven = '111'
         OR codven = '168'
         OR codven = '003'):
    IF codmon = 1 
        THEN x-privado = x-privado + imptot.
        ELSE x-privado = x-privado + imptot * tpocmb.
END.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND coddiv = '00000'
    AND coddoc = 'FAC'
    AND fchdoc >= 05/01/09
    AND fchdoc <= 05/30/09
    AND flgest <> 'A'
    AND (codven = '185' OR codven = '184'):
    IF codmon = 1 
        THEN x-estado = x-estado + imptot.
        ELSE x-estado = x-estado + imptot * tpocmb.
END.
DISPLAY 'Volumen de ventas secor privado S/.' x-privado SKIP 
        'Volumen de ventas sector estado S/.' x-estado.
