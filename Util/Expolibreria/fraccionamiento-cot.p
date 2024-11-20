DEF VAR x-linea AS CHAR.
DEF VAR x-coddiv AS CHAR.
DEF VAR x-nroped AS CHAR.
DEF VAR x-codalm1 AS CHAR.
DEF VAR x-codalm2 AS CHAR.
DEF BUFFER b-cpedi FOR faccpedi.
DEF BUFFER b-dpedi FOR facdpedi.
DEF VAR x-valid1 AS LOG.
DEF VAR x-valid2 AS LOG.
DEF VAR s-coddoc AS CHAR INIT 'COT'.
DEF VAR s-nroser AS INT.
DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-coddiv AS CHAR.

INPUT FROM d:\tmp\cotalm2.prn.
OUTPUT TO d:\tmp\asignado.txt.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    ASSIGN
        /*x-coddiv = SUBSTRING(x-linea,1,10)*/
        x-nroped = SUBSTRING(x-linea,1,10) 
        x-codalm1 = SUBSTRING(x-linea,11,10)
        x-codalm2 = SUBSTRING(x-linea,21).
    /* fraccionamos las COTs en dos almacenes */
    FIND b-cpedi WHERE b-cpedi.codcia = s-codcia
        /*AND b-cpedi.coddiv = x-coddiv*/
        AND b-cpedi.coddoc = s-coddoc
        AND b-cpedi.nroped = x-nroped.
    ASSIGN
        s-coddiv = b-cpedi.coddiv
        x-valid1 = NO
        x-valid2 = NO.
    FOR EACH b-dpedi OF b-cpedi NO-LOCK, FIRST almmmatg OF b-dpedi NO-LOCK:
        IF almmmatg.codfam = '010'  THEN x-valid1 = YES.
        IF almmmatg.codfam <> '010' THEN x-valid2 = YES.
    END.
    /* creamos el primero */
    FIND FIRST FacCorre WHERE FacCorre.CodCia = S-CODCIA 
        AND FacCorre.CodDiv = S-CODDIV
        AND FacCorre.CodDoc = S-CODDOC
        AND FacCorre.FlgEst = YES
        NO-LOCK NO-ERROR.
    ASSIGN
        s-NroSer = FacCorre.NroSer.
    IF x-valid1 = YES THEN DO:
        RUN Primero.
    END.
    /* creamos el segundo */
    IF x-valid2 = YES THEN DO:
        RUN Segundo.
    END.
    ASSIGN
        b-cpedi.FlgEst = "A".
    PUT UNFORMATTED
        b-cpedi.nroped '|'
        b-cpedi.nroref
        SKIP.

END.

PROCEDURE Primero:

    {vta2/icorrelativosecuencial.i &Codigo = s-coddoc &Serie = s-nroser}
    CREATE Faccpedi.
    BUFFER-COPY b-cpedi
        TO Faccpedi
        ASSIGN 
        FacCPedi.NroPed = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
        FacCPedi.LugEnt2 = x-codalm1.
    ASSIGN
        FacCorre.Correlativo = FacCorre.Correlativo + 1.
    FOR EACH b-dpedi OF b-cpedi NO-LOCK, FIRST Almmmatg OF b-dpedi NO-LOCK WHERE Almmmatg.codfam = '010':
        CREATE Facdpedi.
        BUFFER-COPY b-dpedi
            TO Facdpedi
            ASSIGN Facdpedi.nroped = Faccpedi.nroped.
    END.
    ASSIGN 
        b-cpedi.nroref = Faccpedi.nroped.
    {vta2/graba-totales-cotizacion-cred.i}

END PROCEDURE.

PROCEDURE Segundo:

    {vta2/icorrelativosecuencial.i &Codigo = s-coddoc &Serie = s-nroser}
    CREATE Faccpedi.
    BUFFER-COPY b-cpedi
        TO Faccpedi
        ASSIGN 
        FacCPedi.NroPed = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
        FacCPedi.LugEnt2 = x-codalm2.
    ASSIGN
        FacCorre.Correlativo = FacCorre.Correlativo + 1.
    FOR EACH b-dpedi OF b-cpedi NO-LOCK, FIRST Almmmatg OF b-dpedi NO-LOCK WHERE Almmmatg.codfam <> '010':
        CREATE Facdpedi.
        BUFFER-COPY b-dpedi
            TO Facdpedi
            ASSIGN Facdpedi.nroped = Faccpedi.nroped.
    END.
    ASSIGN 
        b-cpedi.nroref = b-cpedi.nroref + (IF b-cpedi.nroref = '' THEN '' ELSE ',') + Faccpedi.nroped.
    {vta2/graba-totales-cotizacion-cred.i}

END PROCEDURE.
