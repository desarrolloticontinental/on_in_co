/* MIGRACION DE COTIZACIONES A OTRA DIVISION */

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-coddoc AS CHAR INIT 'COT' NO-UNDO.
DEF VAR s-coddiv AS CHAR INIT '00015' NO-UNDO.
DEF VAR s-nroser AS INT INIT 040 NO-UNDO.
DEF VAR s-divdes AS CHAR INIT '00000' NO-UNDO.

DEF BUFFER b-cpedi FOR faccpedi.
DEF BUFFER b-dpedi FOR facdpedi.

FOR EACH faccpedi WHERE codcia = s-codcia
    AND coddiv = s-coddiv
    AND coddoc = s-coddoc
    AND fchped >= 10/25/2010
    AND LOOKUP(codven, '015,173,901,902,017') > 0
    AND flgest <> 'a':

    {vtagn/i-faccorre-01.i &Codigo = s-coddoc &Serie = s-nroser}

    CREATE b-cpedi.
    BUFFER-COPY faccpedi
        TO b-cpedi
        ASSIGN
        b-cpedi.coddiv = s-divdes
        b-CPedi.NroPed = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
        b-cpedi.Libre_c05 = Faccpedi.NroPed
        b-cpedi.usuario = 'SAC-40'.
    ASSIGN
        FacCorre.Correlativo = FacCorre.Correlativo + 1.
    ASSIGN
        FacCPedi.FlgEst = 'X'.
    FOR EACH facdpedi OF faccpedi NO-LOCK:
        CREATE b-dpedi.
        BUFFER-COPY facdpedi 
            TO b-dpedi
            ASSIGN
                b-dpedi.coddiv = b-cpedi.coddiv
                b-dpedi.nroped = b-cpedi.nroped.
    END.
END.
