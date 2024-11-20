DEF VAR F-PRECIO AS DEC NO-UNDO.      
def VAR x-inicio AS DATETIME.
DEF VAR x-fin AS DATETIME.

DEF NEW SHARED VAR s-codcia AS INTE INIT 001.
DEF NEW SHARED VAR s-user-id AS CHAR INIT 'SYSTEM'.

RUN LIMPIA.
DISPLAY 'LIMPIA OK' WITH 1 DOWN STREAM-IO. PAUSE 0.

x-inicio = NOW.

RUN UTILEX.
DISPLAY 'UTILEX OK' WITH 1 DOWN STREAM-IO. PAUSE 0.

x-fin = NOW.

MESSAGE x-inicio SKIP x-fin VIEW-AS ALERT-BOX WARNING.

RETURN.


PROCEDURE LIMPIA:

    FOR EACH vtadctoprommin EXCLUSIVE-LOCK:
        DELETE vtadctoprommin.
    END.

END PROCEDURE.


PROCEDURE UTILEX:

    FOR EACH VtaListaMinGn NO-LOCK WHERE VtaListaMinGn.codcia = 001,
        FIRST Almmmatg OF VtaListaMinGn NO-LOCK,
        EACH VtaDctoProm EXCLUSIVE-LOCK WHERE VtaDctoProm.CodCia = VtaListaMinGn.CodCia
        AND VtaDctoProm.CodMat = VtaListaMinGn.codmat
        /*AND (TODAY >= VtaDctoProm.FchIni AND TODAY <= VtaDctoProm.FchFin)*/,
        FIRST GN-DIVI NO-LOCK WHERE GN-DIVI.CodCia = VtaDctoProm.CodCia
            AND GN-DIVI.CodDiv = VtaDctoProm.CodDiv
            AND LOOKUP(gn-divi.canalventa, 'MIN') > 0:  /* Utilex */
        /* TODO EN SOLES */
        CREATE VtaDctoPromMin.
        BUFFER-COPY VtaDctoProm TO VtaDctoPromMin.
        DELETE VtaDctoProm.
    END.

END PROCEDURE.

