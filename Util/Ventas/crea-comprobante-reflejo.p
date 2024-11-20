/* CREA FACTURAS, DEVOLUCION DE MERCADERIA Y NOTAS DE CREDITO POR DEVOLUCION */
DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF NEW SHARED VAR cl-codcia AS INT INIT 000.
DEF NEW SHARED VAR cb-codcia AS INT INIT 000.
DEF NEW SHARED VAR s-user-id AS CHAR.

DEF VAR s-coddoc AS CHAR INIT "FAC" NO-UNDO.
DEF VAR x-nrodoc AS CHAR FORMAT 'x(11)' NO-UNDO.
DEF VAR s-coddiv AS CHAR NO-UNDO.
DEF VAR s-nroser AS INT  NO-UNDO.
DEF VAR s-codalm AS CHAR NO-UNDO.
DEF VAR I-NroSer AS INT  NO-UNDO.
DEF VAR I-NroDoc AS INT  NO-UNDO.
DEF VAR s-codmov AS INT INIT 09 NO-UNDO.
DEF VAR R-ROWID  AS ROWID NO-UNDO. 

DEF BUFFER B-CDOCU FOR Ccbcdocu.
DEF BUFFER FACTURA FOR Ccbcdocu.
DEF BUFFER B-DDOCU FOR Ccbddocu.
DEF BUFFER B-ADocu FOR CcbADocu.

DISABLE TRIGGERS FOR LOAD OF ccbcdocu.
DISABLE TRIGGERS FOR LOAD OF ccbddocu.


REPEAT :
    ASSIGN
        s-coddoc = "FAC".
    PROMPT-FOR x-nrodoc LABEL "Número de FAC"    
        WITH ROW 2 SIDE-LABELS.
    FIND B-CDOCU WHERE B-CDOCU.codcia = s-codcia
        AND B-CDOCU.coddoc = s-coddoc
        AND B-CDOCU.nrodoc = INPUT x-nrodoc
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE B-CDOCU THEN DO:
        MESSAGE 'Documento NO registrado' VIEW-AS ALERT-BOX ERROR.
        NEXT.
    END.
    ASSIGN
        s-coddiv = B-CDOCU.coddiv
        s-nroser = INTEGER(SUBSTRING(B-CDOCU.nrodoc,1,3))
        s-codalm = B-CDOCU.codalm
        s-user-id = B-CDOCU.usuario.
    s-NroSer = 170. /* Forzamos la serie de FAC's */
    RUN Proceso-Principal.
END.

PROCEDURE Proceso-Principal:
/* ************************ */

    DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
        RUN Crea-Comprobante.
        IF RETURN-VALUE = "ADM-ERROR" THEN DO:
            MESSAGE 'NO se pudo crear el comprobante' VIEW-AS ALERT-BOX ERROR.
            UNDO, RETURN.
        END.
    END.

END PROCEDURE.


PROCEDURE Crea-Comprobante:
/* *********************** */

trloop:
DO TRANSACTION ON ERROR UNDO trloop, RETURN 'ADM-ERROR' ON STOP UNDO trloop, RETURN 'ADM-ERROR':
    ASSIGN
        s-coddoc = "FAC".
    /* COPIA LA FACTURA */
    FIND FacCorre WHERE FacCorre.CodCia = s-CodCia AND
        FacCorre.CodDiv = s-CodDiv AND
        FacCorre.CodDoc = s-CodDoc AND
        FacCorre.NroSer = s-NroSer
        EXCLUSIVE-LOCK NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE 'Error en el control de correlativo' s-CodDoc s-CodDiv s-NroSer
            VIEW-AS ALERT-BOX ERROR.
        UNDO trloop, RETURN 'ADM-ERROR'.
    END.

    /* Crea Cabecera */
    CREATE CcbCDocu.
    BUFFER-COPY B-CDOCU
        TO CcbCDocu
        ASSIGN
        CcbCDocu.NroDoc =  STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999") 
        CcbCDocu.FchDoc = TODAY
        CcbCDocu.FchVto = TODAY + (B-CDOCU.FchVto - B-CDOCU.FchDoc)
        CcbCDocu.SdoAct = CcbCDocu.ImpTot
        CcbCDocu.FlgEst = "P"
        /*CcbCDocu.CodRef = ""
        CcbCDocu.NroRef = ""*/
        CcbCDocu.Glosa  = "".
    ASSIGN
        FacCorre.Correlativo = FacCorre.Correlativo + 1.
    FIND gn-clie WHERE gn-clie.CodCia = cl-codcia 
        AND gn-clie.CodCli = CcbCDocu.CodCli 
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-clie  THEN DO:
        ASSIGN
            CcbCDocu.NomCli  = gn-clie.NomCli
            CcbCDocu.DirCli  = gn-clie.DirCli
            CcbCDocu.CodDpto = gn-clie.CodDept 
            CcbCDocu.CodProv = gn-clie.CodProv 
            CcbCDocu.CodDist = gn-clie.CodDist.
    END.
    /* Crea Detalle */
    FOR EACH B-DDOCU OF B-CDOCU NO-LOCK:
        CREATE Ccbddocu.
        BUFFER-COPY B-DDOCU
            TO Ccbddocu
            ASSIGN
            Ccbddocu.nrodoc = Ccbcdocu.nrodoc
            Ccbddocu.fchdoc = Ccbcdocu.fchdoc
            Ccbddocu.candev = 0.
        DISPLAY ccbddocu.coddoc ccbddocu.nrodoc ccbddocu.codmat.
        /*UPDATE ccbddocu.implin.*/
    END.
    RUN Graba-Totales.
    MESSAGE 'Comprobante Generado:' ccbcdocu.coddoc ccbcdocu.nrodoc.
    /* GENERACION DE CONTROL DE PERCEPCIONES */
    RUN vta2/control-percepcion-cargos (ROWID(Ccbcdocu)) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE 'ERROR Percepciones' ccbcdocu.coddoc ccbcdocu.nrodoc
            VIEW-AS ALERT-BOX ERROR.
        UNDO trloop, RETURN 'ADM-ERROR'.
    END.
    /* RHC 25-06-2012 ASIENTO DE TRANSFERENCIA PARA SPEED */           
    RUN aplic/sypsa/registroventas (INPUT ROWID(ccbcdocu), INPUT "I", YES). 
END.

RETURN "OK".

END PROCEDURE.


PROCEDURE Graba-Totales:
/* ******************** */

    {vta2/graba-totales-factura-cred.i}

END PROCEDURE.

