/* cargamos temporal */
DEF TEMP-TABLE detalle
    FIELD codcia AS INTE
    FIELD codmat AS CHAR
    FIELD undvta AS CHAR
    FIELD candes AS DECI
    FIELD preuni AS DECI.

DEF VAR x-linea AS CHAR NO-UNDO.
INPUT FROM d:\oc27793.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    CREATE detalle.
    ASSIGN
        detalle.codcia = 001
        detalle.codmat = SUBSTRING(x-linea,1,10)
        detalle.undvta = SUBSTRING(x-linea,11,10)
        detalle.candes = DECIMAL(SUBSTRING(x-linea,21,10))
        detalle.preuni = DECIMAL(SUBSTRING(x-linea,31)).
END.

DEF NEW SHARED VAR s-codcia AS INTE INIT 001.
DEF NEW SHARED VAR s-coddiv AS CHAR INIT '80014'.
DEF NEW SHARED VAR s-nroser AS INTE INIT 283.
DEF NEW SHARED VAR s-codalm AS CHAR INIT '14e'.

DEF VAR x-Articulo-ICBPer AS CHAR INIT '099268'.

FIND FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK NO-ERROR.

FIND faccorre WHERE faccorre.codcia = s-codcia
    AND faccorre.coddoc = 'FAC'
    AND faccorre.nroser = s-nroser
    EXCLUSIVE-LOCK.

FIND faccpedi WHERE faccpedi.codcia = s-codcia
    AND faccpedi.coddoc = 'COT'
    AND faccpedi.nroped = '002103142'
    NO-LOCK.
FIND gn-clie WHERE gn-clie.codcia = 0
    AND gn-clie.codcli = faccpedi.codcli
    NO-LOCK.

CREATE ccbcdocu.
BUFFER-COPY faccpedi TO ccbcdocu
    ASSIGN
    ccbcdocu.codcia = s-codcia
    ccbcdocu.coddiv = s-coddiv
    ccbcdocu.divori = faccpedi.coddiv
    ccbcdocu.codalm = s-codalm
    ccbcdocu.coddoc = 'FAC'
    ccbcdocu.nrodoc = STRING(s-nroser, '999') + 
                        STRING(faccorre.correlativo, '99999999')
    ccbcdocu.fchdoc = TODAY
    ccbcdocu.fchvto = TODAY
    ccbcdocu.usuario = "SYSTEM"
    ccbcdocu.tpofac = "CR"
    ccbcdocu.flgest = "P"
    ccbcdocu.tipo = "CREDITO"
    .
ASSIGN
    ccbcdocu.nroord = ENTRY(2,faccpedi.glosa,' ').

ASSIGN 
    faccorre.correlativo = faccorre.correlativo + 1.

DEF VAR x-numitem AS INTE INIT 1 NO-UNDO.
FOR EACH detalle, FIRST almmmatg OF detalle NO-LOCK:
    CREATE ccbddocu.
    BUFFER-COPY Ccbcdocu TO Ccbddocu
        ASSIGN
        ccbddocu.nroitm = x-numitem
        ccbddocu.codmat = detalle.codmat 
        ccbddocu.undvta = detalle.undvta 
        ccbddocu.candes = detalle.candes 
        ccbddocu.preuni = detalle.preuni 
        ccbddocu.almdes = ccbcdocu.codalm
        .
    ASSIGN
        ccbddocu.aftigv = almmmatg.aftigv
        ccbddocu.aftisc = almmmatg.aftisc
        Ccbddocu.Pesmat = Almmmatg.Pesmat * (Ccbddocu.Candes * Ccbddocu.Factor)
        .
    x-numitem = x-numitem + 1.
    /* CORREGIMOS IMPORTES: El Precio Unitario ya está afectado con el FLETE */
    ASSIGN
        ccbddocu.ImpLin = ROUND ( ccbddocu.CanDes * ccbddocu.PreUni * 
                                  ( 1 - ccbddocu.Por_Dsctos[1] / 100 ) *
                                  ( 1 - ccbddocu.Por_Dsctos[2] / 100 ) *
                                  ( 1 - ccbddocu.Por_Dsctos[3] / 100 ), 2 ).
    IF ccbddocu.Por_Dsctos[1] = 0 AND ccbddocu.Por_Dsctos[2] = 0 AND ccbddocu.Por_Dsctos[3] = 0 
        THEN ccbddocu.ImpDto = 0.
    ELSE ccbddocu.ImpDto = ccbddocu.CanDes * ccbddocu.PreUni - ccbddocu.ImpLin.
    ASSIGN
        ccbddocu.ImpLin = ROUND(ccbddocu.ImpLin, 2)
        ccbddocu.ImpDto = ROUND(ccbddocu.ImpDto, 2).
    IF ccbddocu.AftIsc 
        THEN ccbddocu.ImpIsc = ROUND(ccbddocu.PreBas * ccbddocu.CanDes * (Almmmatg.PorIsc / 100),4).
    ELSE ccbddocu.ImpIsc = 0.
    IF ccbddocu.AftIgv 
        THEN ccbddocu.ImpIgv = ccbddocu.ImpLin - ROUND( ccbddocu.ImpLin  / ( 1 + (ccbcdocu.PorIgv / 100) ), 4 ).
    ELSE ccbddocu.ImpIgv = 0.
END.

/* Rutina General */
{vtagn/i-total-factura.i &Cabecera="ccbcdocu" &Detalle="ccbddocu"}


/* *********************************************************************** */
/* ACTUALIZAMOS ALMACENES */
/* NO quieren que actualiza almacenes */
/* *********************************************************************** */
/* DEF VAR pMensaje AS CHAR NO-UNDO.                                             */
/* RUN vta2/act_almv2.r ( INPUT ROWID(CcbCDocu), OUTPUT pMensaje ).              */
/* IF RETURN-VALUE = 'ADM-ERROR' THEN DO:                                        */
/*     IF TRUE <> (pMensaje > "") THEN MESSAGE pMensaje VIEW-AS ALERT-BOX ERROR. */
/*     UNDO, RETURN 'ADM-ERROR'.                                                 */
/* END.                                                                          */

MESSAGE ccbcdocu.coddoc ccbcdocu.nrodoc ccbcdocu.imptot.



