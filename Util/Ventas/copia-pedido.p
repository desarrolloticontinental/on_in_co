/* GENERA UN PEDIDO SIMILAR A OTRO */
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-coddoc AS CHAR INIT 'COT' NO-UNDO.
DEF VAR s-nrodoc AS CHAR NO-UNDO.
DEF VAR s-nroser AS INT NO-UNDO.
DEF VAR s-user-id AS CHAR NO-UNDO.
DEF VAR s-coddiv AS CHAR NO-UNDO.
DEF VAR pMensaje AS CHAR NO-UNDO.

DEF BUFFER b-cpedi FOR faccpedi.
DEF BUFFER b-dpedi FOR facdpedi.

ASSIGN
    s-nroser = 018
    s-nrodoc = '018140608'.
FIND b-cpedi WHERE codcia = s-codcia
    AND coddoc = s-coddoc
    AND nroped = s-nrodoc
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE b-cpedi THEN RETURN.

ASSIGN
    s-user-id = b-cpedi.usuario
    s-coddiv = b-cpedi.coddiv.

DO TRANSACTION ON ERROR UNDO, RETURN ON STOP UNDO, RETURN:
    /* bloqueamos correlativo */
    FIND FacCorre WHERE FacCorre.CodCia = s-CodCia AND
        FacCorre.CodDoc = s-coddoc AND
        FacCorre.NroSer = s-nroser EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE FacCorre THEN UNDO, RETURN.

    {vta2/icorrelativosecuencial.i &Codigo = s-coddoc &Serie = s-nroser}

    CREATE Faccpedi.
    BUFFER-COPY b-cpedi
        TO Faccpedi
        ASSIGN 
            Faccpedi.NroPed = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999").
    ASSIGN
        FacCorre.Correlativo = FacCorre.Correlativo + 1.
    /* TRACKING */
    RUN vtagn/pTracking-04 (s-CodCia,
                            s-CodDiv,
                            Faccpedi.CodDoc,
                            Faccpedi.NroPed,
                            s-User-Id,
                            'GNP',
                            'P',
                            DATETIME(TODAY, MTIME),
                            DATETIME(TODAY, MTIME),
                            Faccpedi.CodDoc,
                            Faccpedi.NroPed,
                            Faccpedi.CodRef,
                            Faccpedi.NroRef).
    /* Detalle del Pedido */
    FOR EACH b-dpedi OF b-cpedi NO-LOCK:
        CREATE Facdpedi.
        BUFFER-COPY b-dpedi
            TO Facdpedi
            ASSIGN
            Facdpedi.nroped = Faccpedi.nroped.
    END.
END.
DISPLAY faccpedi.nroped.
