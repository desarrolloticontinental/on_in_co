DEF NEW SHARED VAR s-codcia AS INTE INIT 001.
DEF NEW SHARED VAR s-coddiv AS CHAR INIT '00000'.
DEF NEW SHARED VAR s-user-id AS CHAR INIT 'MRC-00'.
DEF NEW SHARED VAR pv-codcia AS INTE INIT 000.

DEF VAR pmensaje AS CHAR NO-UNDO.

/*000016190 000016209 000016265 000016271 */

DEFINE VAR x-nro AS INT.
DEFINE VAR x-nro-desde AS INT.
DEFINE VAR x-nro-hasta AS INT.
DEFINE VAR x-cnro AS CHAR.

x-nro-desde = 16093.
x-nro-hasta = 16365.

REPEAT x-nro = x-nro-desde TO x-nro-hasta :

    x-cnro = STRING(x-nro,"999999999").

    FIND FIRST di-rutac WHERE di-rutac.codcia = 1
        AND di-rutac.coddiv = '00000'
        AND di-rutac.coddoc = 'PHR'
        AND di-rutac.nrodoc = x-cnro    /*'000016271'*/
        NO-LOCK NO-ERROR.

    pmensaje = "".

    IF AVAILABLE di-rutac THEN
    RUN logis/p-genera-hpk-v2 ( ROWID(di-rutac), OUTPUT pmensaje).
    ELSE MESSAGE 'NUMERO EQUIVOCADO'.

    IF pmensaje > '' THEN MESSAGE x-cnro SKIP
                                pmensaje.

END.


