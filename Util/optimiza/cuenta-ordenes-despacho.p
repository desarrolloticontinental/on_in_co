DEF VAR x-cuenta-privado AS INT INIT 0.
DEF VAR x-cuenta-estado AS INT INIT 0.

FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddiv = '00000'
    AND coddoc = 'O/D'
    AND fchped >= 05/01/09 AND fchped <= 05/30/09
    AND flgest <> 'A'
    AND (codven = '110' OR codven = '111' OR
         codven = '168'
         OR codven = '003'):
    x-cuenta-privado = x-cuenta-privado + 1.
END.
FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddiv = '00000'
    AND coddoc = 'O/D'
    AND fchped >= 05/01/09 AND fchped <= 05/30/09
    AND flgest <> 'A'
    AND (codven = '185' OR codven = '184'):
    x-cuenta-estado = x-cuenta-estado + 1.
END.
DISPLAY 'Ordenes Despacho sector privado' x-cuenta-privado SKIP
        'Ordenes Despacho sector estado'  x-cuenta-estado .
