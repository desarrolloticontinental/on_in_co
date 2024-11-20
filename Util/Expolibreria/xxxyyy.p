DEF TEMP-TABLE t-cpedi LIKE faccpedi.
DEF TEMP-TABLE t-dpedi LIKE facdpedi
    FIELD segmento AS INT.

DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-coddoc AS CHAR INIT 'COT' NO-UNDO.

DEF VAR x-linea AS CHAR NO-UNDO.
INPUT FROM d:\tmp\separar.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    FIND faccpedi WHERE faccpedi.codcia = s-codcia
        AND faccpedi.coddiv = SUBSTRING(x-linea, 1, 5)
        AND faccpedi.coddoc = s-coddoc
        AND faccpedi.nroped = SUBSTRING(x-linea, 11, 9)
        NO-LOCK.
    FIND t-cpedi WHERE t-cpedi.codcia = faccpedi.codcia
        AND t-cpedi.coddiv = faccpedi.coddiv
        AND t-cpedi.coddoc = faccpedi.coddoc
        AND t-cpedi.nroped = faccpedi.nroped
        NO-ERROR.
    IF NOT AVAILABLE t-cpedi THEN CREATE t-cpedi.
    BUFFER-COPY faccpedi TO t-cpedi.
    FOR EACH facdpedi OF faccpedi NO-LOCK WHERE facdpedi.codmat = SUBSTRING(x-linea, 41, 6):
        CREATE t-dpedi.
        BUFFER-COPY facdpedi TO t-dpedi
            ASSIGN 
            t-dpedi.segmento = INT(SUBSTRING(x-linea,61))
            t-dpedi.codcli   = faccpedi.codcli.
    END.
END.
INPUT CLOSE.

DEF VAR x-Rowid AS ROWID NO-UNDO.
DEF VAR s-nroser AS INT NO-UNDO.
DEF VAR s-coddiv AS CHAR.
DEF VAR s-codalm AS CHAR.
DEF VAR I-NITEM AS INTEGER NO-UNDO.

FOR EACH t-dpedi, FIRST t-cpedi OF t-dpedi BREAK BY t-dpedi.codcli BY t-dpedi.segmento BY t-dpedi.nroitm:
    IF FIRST-OF(t-dpedi.codcli) OR FIRST-OF(t-dpedi.segmento) THEN DO:
        /* Cabecera */
        ASSIGN
            s-coddiv = t-cpedi.coddiv
            s-nroser = INTEGER(SUBSTRING(t-cpedi.nroped,1,3))
            s-codalm = t-cpedi.codalm.

        CREATE Faccpedi.
        {vta2/icorrelativosecuencial.i &Codigo = s-coddoc &Serie = s-nroser}
        BUFFER-COPY t-cpedi TO Faccpedi
            ASSIGN 
            FacCPedi.FchPed = TODAY 
            FacCPedi.Hora   = STRING(TIME, 'HH:MM:SS')
            FacCPedi.NroPed = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999").
        ASSIGN
            FacCorre.Correlativo = FacCorre.Correlativo + 1.
        x-Rowid = ROWID(Faccpedi).
        I-NITEM = 0.
    END.
    FIND Faccpedi WHERE ROWID(Faccpedi) = x-Rowid.
    I-NITEM = I-NITEM + 1.
    CREATE Facdpedi.
    BUFFER-COPY t-dpedi TO Facdpedi
        ASSIGN
        FacDPedi.CodCia = FacCPedi.CodCia
        FacDPedi.CodDiv = FacCPedi.CodDiv
        FacDPedi.coddoc = FacCPedi.coddoc
        FacDPedi.NroPed = FacCPedi.NroPed
        FacDPedi.FchPed = FacCPedi.FchPed
        FacDPedi.Hora   = FacCPedi.Hora 
        FacDPedi.FlgEst = FacCPedi.FlgEst
        FacDPedi.NroItm = I-NITEM.
    IF LAST-OF(t-dpedi.codcli) OR LAST-OF(t-dpedi.segmento) THEN DO:
        DISPLAY faccpedi.coddiv faccpedi.coddoc faccpedi.nroped WITH STREAM-IO NO-BOX.
        PAUSE 0.
        {vta2/graba-totales-cotizacion-cred.i}
    END.
END.

FOR EACH t-cpedi:
    FIND faccpedi WHERE faccpedi.codcia = t-cpedi.codcia
        AND faccpedi.coddiv = t-cpedi.coddiv
        AND faccpedi.coddoc = t-cpedi.coddoc
        AND faccpedi.nroped = t-cpedi.nroped.
    ASSIGN faccpedi.flgest = "X".
END.
