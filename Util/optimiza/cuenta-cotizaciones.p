DEF VAR x-total-privado AS INT INIT 0.
DEF VAR x-anulada-privado AS INT INIT 0.
DEF VAR x-total-estado AS INT INIT 0.
DEF VAR x-anulada-estado AS INT INIT 0.

FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddiv = '00000'
    AND coddoc = 'COT'
    AND fchped >= 05/01/09
    AND fchped <= 05/30/09
    AND (codven = '110' OR codven = '111' OR
         codven = '168'
         OR codven = '003'):
    x-total-privado = x-total-privado + 1.
    IF flgest = 'A' THEN x-anulada-privado = x-anulada-privado + 1.
END.
FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddiv = '00000'
    AND coddoc = 'COT'
    AND fchped >= 05/01/09
    AND fchped <= 05/30/09
    AND (codven = '185' OR codven = '184'):
    x-total-estado = x-total-estado + 1.
    IF flgest = 'A' THEN x-anulada-estado = x-anulada-estado + 1.
END.
DISPLAY 'sector privado' x-total-privado x-anulada-privado (x-total-privado - x-anulada-privado) SKIP
    'sector estado' x-total-estado x-anulada-estado (x-total-estado - x-anulada-estado).

/*
A) Augusto Junchaya
> - 110 :Privado
> - 185 : Estado
> 
> B) Javier Yalan Aedo
> - 111 :Privado
> - 184 : Estado
> 
> C) Marcial Muñoz
> - 168 :Privado
> 
> D) Raúl Loza
> - 003 :Privado
*/

