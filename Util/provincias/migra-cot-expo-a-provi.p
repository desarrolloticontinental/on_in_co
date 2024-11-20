DEF BUFFER b-cab FOR faccpedi.
DEF BUFFER b-det FOR facdpedi.

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-coddoc AS CHAR INIT "COT" NO-UNDO.
DEF VAR s-nroser AS INT INIT 018 NO-UNDO.
DEF VAR s-coddiv AS CHAR INIT "20018" NO-UNDO.

FIND FIRST faccorre WHERE faccorre.codcia = s-codcia
    AND faccorre.coddoc = s-coddoc
    AND faccorre.coddiv = s-coddiv
    AND faccorre.flgest = YES
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE faccorre THEN DO:
    MESSAGE 'no registrado correlaivo' VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.
s-nroser = faccorre.nroser.

DEFINE VAR x-conteo AS INT.

DISABLE TRIGGERS FOR LOAD OF Faccpedi.
DISABLE TRIGGERS FOR LOAD OF Facdpedi.

DEFINE BUFFER b-faccpedi FOR faccpedi.

x-conteo = 0.

PROCESO:
FOR EACH b-cab WHERE codcia = s-codcia
    AND coddiv = '00015'
    AND coddoc = s-coddoc
    AND fchped >= 09/01/2022
    AND lookup(flgest,'A,C') = 0
    AND usuario = 'VTA-118' NO-LOCK:
    IF CAN-FIND(FIRST b-det OF b-cab WHERE b-det.canate <> 0 NO-LOCK) THEN NEXT.

    x-conteo = x-conteo + 1.
    
    {vtagn/i-faccorre-01.i &Codigo = s-coddoc &Serie = s-nroser}
    CREATE Faccpedi.
    BUFFER-COPY b-cab
        TO Faccpedi
        ASSIGN 
        FacCPedi.CodCia = S-CODCIA
        FacCPedi.CodDiv = S-CODDIV
        FacCPedi.CodDoc = s-coddoc 
        FacCPedi.NroPed = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
        FacCPedi.CodOrigen = b-cab.coddoc
        FacCPedi.NroOrigen = b-cab.nroped
        FacCPedi.Usuario = "VTA-218".
    ASSIGN
        FacCorre.Correlativo = FacCorre.Correlativo + 1.
    FOR EACH b-det OF b-cab NO-LOCK:
        CREATE Facdpedi.
        BUFFER-COPY b-det
            TO Facdpedi
            ASSIGN
            Facdpedi.coddiv = Faccpedi.coddiv
            Facdpedi.nroped = Faccpedi.nroped.
    END.

    FIND FIRST b-faccpedi WHERE ROWID(b-faccpedi) = ROWID(b-cab) EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE b-faccpedi THEN DO:
        ASSIGN
            b-faccpedi.flgest = "A"
            b-faccpedi.libre_c05 = faccpedi.nroped.
    END.
    RELEASE b-faccpedi.

    IF x-conteo >= 3 THEN DO:
        /*LEAVE PROCESO.*/
    END.
    
END.

RELEASE faccpedi NO-ERROR.
RELEASE facdpedi NO-ERROR.

MESSAGE x-conteo.
