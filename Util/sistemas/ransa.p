DEF VAR s-codcia AS INT INIT 001.
DEF VAR x-fchini AS DATE.

x-fchini = DATE(09,01,2018).

DEF BUFFER PEDIDO FOR faccpedi.

DEF STREAM venta-cab.
DEF STREAM venta-det.

OUTPUT STREAM venta-cab TO d:\venta-cabecera.txt.
OUTPUT STREAM venta-det TO d:\venta-detalle.txt.
FOR EACH faccpedi NO-LOCK WHERE faccpedi.codcia = s-codcia AND
    faccpedi.coddoc = 'COT' AND
    faccpedi.fchped >= x-fchini AND
    faccpedi.fchped <= (TODAY - 1),
    FIRST gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia AND
    gn-divi.coddiv = faccpedi.libre_c01 AND
    gn-divi.canalventa = "FER",
    EACH PEDIDO NO-LOCK WHERE PEDIDO.codcia = s-codcia AND
    PEDIDO.coddoc = "PED" AND
    PEDIDO.codref = faccpedi.coddoc AND
    PEDIDO.nroref = faccpedi.nroped,
    EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia AND
    LOOKUP(ccbcdocu.coddoc, 'FAC,BOL') > 0 AND
    ccbcdocu.codped = PEDIDO.coddoc AND
    ccbcdocu.nroped = PEDIDO.nroped,
    EACH almcmov NO-LOCK WHERE almcmov.codcia = s-codcia AND
    almcmov.tipmov = 'S' AND
    almcmov.codmov = 02 AND
    almcmov.codref = ccbcdocu.coddoc AND
    almcmov.nroref = ccbcdocu.nrodoc:
    EXPORT STREAM venta-cab 
        Almcmov.CodCia 
        Almcmov.CodAlm 
        Almcmov.TipMov 
        Almcmov.CodMov 
        Almcmov.NroDoc
        Almcmov.FchDoc
        Almcmov.TotItm
        Almcmov.AlmDes
        Almcmov.NroSer
        Almcmov.FlgEst
        Almcmov.HraDoc.
    FOR EACH almdmov OF almcmov NO-LOCK:
        EXPORT STREAM venta-det 
            Almdmov.CodCia 
            Almdmov.CodAlm 
            Almdmov.TipMov 
            Almdmov.CodMov 
            Almdmov.NroDoc 
            Almdmov.FchDoc 
            Almdmov.NroItm 
            Almdmov.CanDes 
            Almdmov.Factor 
            Almdmov.CodUnd 
            Almdmov.AlmOri 
            Almdmov.codmat 
            Almdmov.NroSer 
            Almdmov.HraDoc
            .
    END.
END.


