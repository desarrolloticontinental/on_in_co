/* Cambiar fecha a movimientos de almacen */

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-codalm AS CHAR NO-UNDO.
DEF VAR NumeroD AS INT NO-UNDO.
DEF VAR NumeroH AS INT NO-UNDO.
DEF VAR s-fchdoc AS DATE NO-UNDO.
DEF VAR s-tipmov AS CHAR NO-UNDO.
DEF VAR s-codmov AS INT NO-UNDO.
DEF VAR s-nroser AS INT NO-UNDO.

ASSIGN
    s-codalm = '11e'
    s-tipmov = 'I'
    s-codmov = 02
    s-nroser = 000
    NumeroD = 1231
    NumeroH = 1231
    s-fchdoc = 01/15/2015.

FOR EACH almcmov WHERE codcia = s-codcia
    AND codalm = s-codalm
    AND tipmov = s-tipmov
    AND codmov = s-codmov
    AND nroser = s-nroser
    AND nrodoc >= NumeroD
    AND nrodoc <= NumeroH:
    DISPLAY fchdoc nroser nrodoc nomref WITH STREAM-IO.
    PAUSE 0.
    fchdoc = s-fchdoc.
    FOR EACH almdmov OF almcmov:
        almdmov.fchdoc = almcmov.fchdoc.
    END.
END.
