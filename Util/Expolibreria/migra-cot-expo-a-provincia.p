/* SOLO SE VA A CAMBIAR LA SERIE DE LA COTIZACION */
/* SIEMPRE Y CUANDO NO CHOQUE CON DOCUMENTOS REGISTRADOS ANTERIORMENTE */

DEF VAR s-codcia AS INT     INIT 001    NO-UNDO.
DEF VAR s-coddiv AS CHAR    INIT '00018' NO-UNDO.
DEF VAR s-coddoc AS CHAR    INIT 'COT'  NO-UNDO.
DEF VAR s-nroser AS INT     INIT 018    NO-UNDO.
DEF VAR s-user-id AS CHAR   INIT 'SAC-00'   NO-UNDO.

/* Archivo de entrada */
DISABLE TRIGGERS FOR LOAD OF faccpedi.
DISABLE TRIGGERS FOR LOAD OF facdpedi.

DEF BUFFER b-cpedi FOR faccpedi.
DEF BUFFER b-dpedi FOR facdpedi.
DEF VAR x-nroped AS CHAR NO-UNDO.
DEF VAR x-linea AS CHAR NO-UNDO.
INPUT FROM d:\tmp\provincias.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    FIND faccpedi WHERE codcia = s-codcia
        AND coddoc = s-coddoc
        AND nroped = x-linea
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE faccpedi THEN DO:
        MESSAGE 'cotizacion' x-linea 'en uso por otro usuario'.
        NEXT.
    END.
    DISPLAY faccpedi.nroped faccpedi.fchped. PAUSE 0.
    x-nroped = STRING(s-nroser,'999') + SUBSTRING(faccpedi.nroped,4).
    CREATE b-cpedi.
    BUFFER-COPY faccpedi TO b-cpedi
        ASSIGN
            b-cpedi.coddiv = s-coddiv
            b-cpedi.nroped = x-nroped
            b-cpedi.Libre_c05 = Faccpedi.NroPed     /* Como referencia */
            b-cpedi.usuario = s-user-id.
    FOR EACH facdpedi OF faccpedi NO-LOCK:
        CREATE b-dpedi.
        BUFFER-COPY facdpedi TO b-dpedi
            ASSIGN
                b-dpedi.coddiv = s-coddiv
                b-dpedi.nroped = x-nroped.
    END.
    ASSIGN
        FacCPedi.FlgEst = 'X'.  /* CERRADA */
END.
INPUT CLOSE.

