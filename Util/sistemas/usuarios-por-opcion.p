FIND pf-g002 WHERE aplic-id = 'alm'
    AND codmnu = '1001'
    NO-LOCK.
DEF TEMP-TABLE t-usuario
    FIELD usuario AS CHAR
    FIELD nombre AS CHAR
    FIELD grupo AS CHAR
    FIELD DISABLED AS LOG
    .

DEF VAR k AS INTE.
DO  k = 1 TO NUM-ENTRIES(PF-G002.Seguridad-Grupos):
    FOR EACH pf-g004 NO-LOCK WHERE pf-g004.aplic-id = 'alm'
        AND INDEX(pf-g004.seguridad, ENTRY(k,PF-G002.Seguridad-Grupos)) > 0:
        FIND t-usuario WHERE t-usuario.usuario = pf-g004.USER-ID NO-LOCK NO-ERROR.
        IF NOT AVAILABLE t-usuario THEN DO:
            CREATE t-usuario.
            ASSIGN 
                t-usuario.usuario = pf-g004.USER-ID
                t-usuario.grupo = ENTRY(k,PF-G002.Seguridad-Grupos)
                .
            FIND _user WHERE _userid = pf-g004.USER-ID NO-LOCK NO-ERROR.
            IF AVAILABLE _user THEN 
                ASSIGN
                t-usuario.nombre = _user._user-name
                t-usuario.DISABLED = _user._disabled
                .
        END.

    END.
END.

OUTPUT TO d:\usuarios.txt.
FOR EACH t-usuario NO-LOCK WHERE t-usuario.DISABLED = NO:
    DISPLAY t-usuario.usuario FORMAT 'x(15)' 
        t-usuario.nombre FORMAT 'x(60)'
        t-usuario.grupo FORMAT 'x(30)'
        /*t-usuario.DISABLED*/
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
