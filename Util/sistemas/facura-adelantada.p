DEF TEMP-TABLE ITEM LIKE ccbddocu.

/* Carga temporal */
EMPTY temp-table ITEM.

/* ************** */


DEF NEW SHARED VAR s-codcia AS INTE INIT 001.
DEF NEW SHARED VAR s-coddiv AS CHAR INIT '00000'.
DEF NEW SHARED VAR s-codcli AS CHAR INIT '??????????????????????'.
DEF NEW SHARED VAR s-fmapgo AS CHAR INIT '??????????????????????'.
DEF NEW SHARED VAR s-codalm AS CHAR INIT '??????????????????????'.

DEF VAR x-Articulo-ICBPer AS CHAR INIT '099268'.

FIND FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK NO-ERROR.

FIND gn-clie WHERE gn-clie.codcia = 0
    AND gn-clie.codcli = s-codcli
    NO-LOCK.

FIND faccorre WHERE faccorre.codcia = s-codcia
    AND faccorre.coddoc = 'FAC'
    AND faccorre.nroser = 284
    EXCLUSIVE-LOCK.

/* Creamos cabecera */
CREATE ccbcdocu.
ASSIGN
    ccbcdocu.codcia = s-codcia
    ccbcdocu.coddiv = s-coddiv
    ccbcdocu.coddoc = 'FAC'
    ccbcdocu.nrodoc = "284" + STRING(faccorre.correlativo, '99999999')
    ccbcdocu.codalm = s-codalm
    ccbcdocu.codmov = 02
    ccbcdocu.sede   = '@@@'
    ccbcdocu.tipvta = "2"
    ccbcdocu.fchdoc = TODAY
    ccbcdocu.fchvto = TODAY
    ccbcdocu.codcli = s-codcli
    ccbcdocu.ruccli = gn-clie.ruc
    ccbcdocu.usuario = "SYSTEM"
    ccbcdocu.nomcli = gn-clie.nomcli
    ccbcdocu.dircli = gn-clie.dircli
    ccbcdocu.tpocmb = faccfggn.tpocmb[1]
    ccbcdocu.codmon = 1
    ccbcdocu.codven = '020'
    ccbcdocu.fmapgo = s-fmapgo
    ccbcdocu.tpofac = "CR"
    ccbcdocu.flgest = "P"
    ccbcdocu.tipo = "CREDITO"
    ccbcdocu.porigv = faccfggn.porigv
    .
ASSIGN 
    CcbCDocu.CodDpto = gn-clie.CodDept 
    CcbCDocu.CodProv = gn-clie.CodProv 
    CcbCDocu.CodDist = gn-clie.CodDist.
FIND gn-ven WHERE gn-ven.codcia = s-codcia AND
    gn-ven.codven = ccbcdocu.codven
    NO-LOCK NO-ERROR.
IF AVAILABLE gn-ven THEN ccbcdocu.cco = gn-ven.cco.

ASSIGN 
    faccorre.correlativo = faccorre.correlativo + 1.

DEF VAR iCountItem AS INTE INIT 1 NO-UNDO.
FOR EACH ITEM NO-LOCK, FIRST Almmmatg OF ITEM NO-LOCK:
    CREATE ccbddocu.
    BUFFER-COPY Ccbcdocu 
        TO Ccbddocu
        ASSIGN
        ccbddocu.nroitm = iCountItem
        Ccbddocu.codmat = ITEM.codmat
        Ccbddocu.undvta = "UNI"
        Ccbddocu.candes = ITEM.candes
        Ccbddocu.factor = 1
        Ccbddocu.preuni = ITEM.preuni
        Ccbddocu.implin = ITEM.implin
        Ccbddocu.aftigv = Almmmatg.AftIgv
        Ccbddocu.Pesmat = Almmmatg.Pesmat * (Ccbddocu.Candes * Ccbddocu.Factor)
        .
    iCountItem = iCountItem + 1.
    IF ccbddocu.AftIsc 
        THEN ccbddocu.ImpIsc = ROUND(ccbddocu.PreBas * ccbddocu.CanDes * (Almmmatg.PorIsc / 100),4).
    ELSE ccbddocu.ImpIsc = 0.
    IF ccbddocu.AftIgv 
        THEN ccbddocu.ImpIgv = ccbddocu.ImpLin - ROUND( ccbddocu.ImpLin  / ( 1 + (ccbcdocu.PorIgv / 100) ), 4 ).
    ELSE ccbddocu.ImpIgv = 0.
END.

/* Rutina General */
{vtagn/i-total-factura.i &Cabecera="ccbcdocu" &Detalle="ccbddocu"}
