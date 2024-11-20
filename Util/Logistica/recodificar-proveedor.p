/* corregir proveedor */
DEF TEMP-TABLE t-provee LIKE gn-prov.
DEF VAR x-codold LIKE gn-prov.codpro NO-UNDO.
DEF VAR x-codnew LIKE gn-prov.codpro NO-UNDO.
DEF VAR pv-codcia AS INT INIT 000 NO-UNDO.

ASSIGN
    x-codold = '251971631'
    x-codnew = '52197163'.

FIND gn-prov WHERE codcia = pv-codcia
    AND codpro = x-codold
    NO-ERROR.
IF NOT AVAILABLE gn-prov THEN RETURN.
CREATE t-provee.
BUFFER-COPY gn-prov
    TO t-provee
    ASSIGN t-provee.codpro = x-codnew.
DELETE gn-prov.
CREATE gn-prov.
BUFFER-COPY t-provee TO gn-prov.
MESSAGE 'cambio exitoso' VIEW-AS ALERT-BOX INFORMATION.

