DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF NEW SHARED VAR s-coddiv AS CHAR.
DEF NEW SHARED VAR s-user-id AS CHAR.

DEF VAR FechaD AS DATE NO-UNDO.
DEF VAR FechaH AS DATE NO-UNDO.

ASSIGN
    FechaD = 01/01/2015
    FechaH = 02/28/2015.

FOR EACH ccbccaja NO-LOCK WHERE codcia = s-codcia
    AND coddoc = 'I/C'
    AND fchdoc >= FechaD
    AND fchdoc <= FechaH
    AND LOOKUP(Ccbccaja.Tipo, 'MOSTRADOR,CANCELACION') > 0
    AND flgest <> 'A':
    DISPLAY coddiv coddoc nrodoc fchdoc.
    PAUSE 0.
    ASSIGN
        s-coddiv = ccbccaja.coddiv
        s-user-id = ccbccaja.usuario.
    RUN vta2/genera-comprobante-percepcion ( ROWID(ccbccaja) ).
END.
