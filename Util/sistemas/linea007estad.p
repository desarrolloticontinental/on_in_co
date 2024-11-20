&SCOPED-DEFINE Tabla ventasxproducto

DISABLE TRIGGERS FOR LOAD OF {&Tabla}.

FOR EACH almmmatg NO-LOCK WHERE codcia = 1 AND codfam = '007' AND aftigv = NO
    BY almmmatg.codmat DESC:
    DISPLAY almmmatg.codmat. PAUSE 0.
    RUN actualiza.
END.


PROCEDURE actualiza:

    FOR EACH {&Tabla} WHERE {&Tabla}.codmat = almmmatg.codmat
        AND {&Tabla}.datekey >= 01/01/2016:
        {&Tabla}.impnacsigv = {&Tabla}.impnaccigv.
        {&Tabla}.impextsigv = {&Tabla}.impextcigv.
        {&Tabla}.promnaccigv = {&Tabla}.promnacsigv.
        {&Tabla}.promextcigv = {&Tabla}.promextsigv.
        {&Tabla}.costonacsigv = {&Tabla}.costonaccigv.
        {&Tabla}.costoextsigv = {&Tabla}.costoextcigv.
    END.

END PROCEDURE.

