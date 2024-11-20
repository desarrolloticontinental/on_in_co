DEF VAR ptexto AS CHAR NO-UNDO.
DEF VAR x-linea AS CHAR.

INPUT FROM d:\tmp\clientes.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    FIND gn-clie WHERE codcia = 0 AND codcli = x-linea
        EXCLUSIVE-LOCK.
    DISPLAY gn-clie.codcli. PAUSE 0.

    pTexto = gn-clie.nomcli.
    RUN limpia-Texto (INPUT-OUTPUT pTexto).
    gn-clie.nomcli = pTexto.

    pTexto = gn-clie.nombre.
    RUN limpia-Texto (INPUT-OUTPUT pTexto).
    gn-clie.nombre = pTexto.

    pTexto = gn-clie.dircli.
    RUN limpia-Texto (INPUT-OUTPUT pTexto).
    gn-clie.dircli = pTexto.
END.

RETURN.

PROCEDURE Limpia-Texto:
/* ******************* */

    DEF INPUT-OUTPUT PARAMETER pTexto AS CHAR.

    pTexto = REPLACE(pTexto,"&quot;",'"').

    pTexto = REPLACE(pTexto,"&lt;",'<').

    pTexto = REPLACE(pTexto,"&gt;",'>').

    pTexto = REPLACE(pTexto,"&amp;",'&').

    pTexto = REPLACE(pTexto,"&apos;",'&').


END PROCEDURE.
