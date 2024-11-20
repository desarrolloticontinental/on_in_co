DISABLE TRIGGERS FOR LOAD OF Almmmate.
DEF VAR s-codcia AS INT INIT 001.
DEF VAR pComprometido AS DEC NO-UNDO.
DEF VAR i AS INT INIT 0.
FOR EACH Almacen NO-LOCK WHERE Almacen.codcia = 1 
    AND Almacen.campo-c[9] <> "I"
    AND Almacen.flgrep = YES,
    EACH Almmmate OF Almacen:
    i = i + 1.
    IF i > 1000 AND (i MODULO 1000 = 0) THEN DO:
        DISPLAY Almmmate.codalm Almmmate.codmat.
        PAUSE 0.
    END.
    pComprometido = 0.
    RUN Comprometido (Almmmate.codcia, Almmmate.codalm, Almmmate.codmat, OUTPUT pComprometido).
    ASSIGN Almmmate.StkComprometido = pComprometido.
END.
RETURN.

PROCEDURE Comprometido:

DEF INPUT PARAMETER pCodCia AS INT.
DEF INPUT PARAMETER pCodAlm AS CHAR.
DEF INPUT PARAMETER pCodMat AS CHAR.
DEF OUTPUT PARAMETER pComprometido AS DEC NO-UNDO.


DEF BUFFER B-DPEDI FOR Facdpedi.
DEF BUFFER B-CPEDI FOR Faccpedi.
DEF BUFFER B-MATE  FOR Almmmate.
DEF BUFFER B-CREPO FOR Almcrepo.
DEF BUFFER B-DREPO FOR Almdrepo.


LOOPGENERAL:
DO:
    FIND FacCfgGn WHERE faccfggn.codcia = pCodCia NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Faccfggn THEN LEAVE.
    pComprometido = 0.
    /**********   Barremos para los PEDIDOS AL CREDITO   ***********************/ 
    FOR EACH B-DPEDI USE-INDEX Llave04 NO-LOCK WHERE B-DPEDI.codcia = pCodCia
        AND B-DPEDI.almdes = pCodAlm
        AND B-DPEDI.codmat = pCodMat
        AND B-DPEDI.coddoc = 'PED'
        AND B-DPEDI.flgest = 'P':
        /* RHC 12.12.2011 agregamos los nuevos estados */
        FIND FIRST B-CPEDI OF B-DPEDI WHERE LOOKUP(B-CPEDI.FlgEst, "G,X,P,W,WX,WL") > 0 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE B-CPEDI THEN NEXT.
        pComprometido = pComprometido + B-DPEDI.Factor * (B-DPEDI.CanPed - B-DPEDI.canate).
    END.
    /* ORDENES DE DESPACHO CREDITO */
    FOR EACH B-DPEDI USE-INDEX Llave04 NO-LOCK WHERE B-DPEDI.codcia = pCodCia
        AND B-DPEDI.almdes = pCodAlm
        AND B-DPEDI.codmat = pCodMat
        AND B-DPEDI.coddoc = 'O/D'
        AND B-DPEDI.flgest = 'P':
        FIND FIRST B-CPEDI OF B-DPEDI WHERE B-CPEDI.flgest = 'P' NO-LOCK NO-ERROR.
        IF NOT AVAILABLE B-CPEDI THEN NEXT.
        pComprometido = pComprometido + B-DPEDI.Factor * (B-DPEDI.CanPed - B-DPEDI.canate).
    END.
    /* Stock Comprometido por Pedidos por Reposicion Automatica */
    /* OJO ver tambien el programa vtamay/c-conped.w */
    FOR EACH B-CREPO USE-INDEX Llave02 NO-LOCK WHERE B-CREPO.codcia = pCodCia
        AND B-CREPO.AlmPed = pCodAlm
        AND B-CREPO.FlgEst = 'P'
        AND LOOKUP(B-CREPO.FlgSit, 'A,P') > 0,     /* Aprobados y x Aprobar */
        EACH B-DREPO OF B-CREPO NO-LOCK WHERE B-DREPO.codmat = pCodMat
        AND B-DREPO.CanApro > B-DREPO.CanAten:
        pComprometido = pComprometido + (B-DREPO.CanApro - B-DREPO.CanAten).
    END.
    /* POR ORDENES DE TRANSFERENCIA */
    FOR EACH B-DPEDI USE-INDEX Llave04 NO-LOCK WHERE B-DPEDI.codcia = pCodCia
        AND B-DPEDI.almdes = pCodAlm
        AND B-DPEDI.codmat = pCodMat
        AND B-DPEDI.coddoc = 'OTR'
        AND B-DPEDI.flgest = 'P':
        FIND FIRST B-CPEDI OF B-DPEDI WHERE B-CPEDI.flgest = 'P' NO-LOCK NO-ERROR.
        IF NOT AVAILABLE B-CPEDI THEN NEXT.
        pComprometido = pComprometido + B-DPEDI.Factor * (B-DPEDI.CanPed - B-DPEDI.CanAte).
    END.
END.



END PROCEDURE.
