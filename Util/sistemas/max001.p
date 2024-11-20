DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF NEW SHARED VAR cl-codcia AS INT INIT 000.
DEF NEW SHARED VAR pv-codcia AS INT INIT 000.
DEF VAR x-fchini AS DATE NO-UNDO.
DEF VAR x-fchfin AS DATE NO-UNDO.
DEF VAR x-coddoc AS CHAR INIT 'O/D,OTR,O/M' NO-UNDO.

ASSIGN
    x-fchini = DATE(01,01,2018)
    x-fchfin = DATE(12,31,2018).

DEF VAR x-Item AS INT.

OUTPUT TO d:\tmp\max001.txt.
DO x-Item = 1 TO 3:
    RUN Carga-Pedidos (ENTRY(x-Item,x-coddoc)).
END.
OUTPUT CLOSE.


PROCEDURE Carga-Pedidos:

    DEF INPUT PARAMETER x-CodDoc AS CHAR.

    DEF VAR pUbigeo AS CHAR NO-UNDO.
    DEF VAR pLongitud AS DEC NO-UNDO.
    DEF VAR pLatitud AS DEC NO-UNDO.

    DEF VAR itemsq AS INT NO-UNDO.
    DEF VAR weightsum AS DEC NO-UNDO.
    DEF VAR volumesum AS DEC NO-UNDO.
    DEF VAR amounttotal AS DEC NO-UNDO.
    DEF VAR monto AS DEC NO-UNDO.
    DEF VAR repositionamount AS DEC NO-UNDO.

    FOR EACH faccpedi NO-LOCK WHERE faccpedi.codcia = s-codcia
        AND faccpedi.coddoc = x-coddoc
        AND faccpedi.fchped >= x-fchini
        AND faccpedi.fchped <= x-fchfin
        AND faccpedi.flgest <> 'A':
        FIND FIRST vtacdocu WHERE vtacdocu.codcia = s-codcia
            AND vtacdocu.codped = 'HPK'
            AND vtacdocu.codref = faccpedi.coddoc
            AND vtacdocu.nroref = faccpedi.nroped
            AND vtacdocu.flgest <> 'A'
            NO-LOCK NO-ERROR.

        FIND FIRST gn-ven WHERE gn-ven.codcia = s-codcia
            AND gn-ven.codven = faccpedi.codven
            NO-LOCK NO-ERROR.
        
        RUN logis/p-datos-sede-auxiliar (INPUT faccpedi.ubigeo[2],
                                         INPUT faccpedi.ubigeo[3],
                                         INPUT faccpedi.ubigeo[1],
                                         OUTPUT pUbigeo,
                                         OUTPUT pLongitud,
                                         OUTPUT pLatitud).
        IF TRUE <> (pUbigeo > '') THEN
            CASE faccpedi.ubigeo[2]:
                WHEN "@CL" THEN DO:
                    FIND gn-clie WHERE gn-clie.codcia = cl-codcia
                        AND gn-clie.codcli = faccpedi.ubigeo[3]
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE gn-clie THEN pUbigeo = gn-clie.CodDept + gn-clie.CodProv + gn-clie.CodDist.
                END.
                WHEN "@PV" THEN DO:
                    FIND gn-prov WHERE gn-prov.codcia = pv-codcia
                        AND gn-prov.codpro = faccpedi.ubigeo[3]
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE gn-prov THEN pUbigeo = gn-prov.CodDept + gn-prov.CodProv + gn-prov.CodDist.
                END.
            END CASE.

        ASSIGN
            itemsq = 0
            weightsum = 0
            volumesum = 0
            amounttotal = 0
            monto = 0
            repositionamount = 0.
        FOR EACH Facdpedi OF Faccpedi NO-LOCK, FIRST Almmmatg OF Facdpedi NO-LOCK:
            itemsq = itemsq + 1.
            weightsum = weightsum + (facdpedi.canped * facdpedi.factor * almmmatg.pesmat).
            volumesum = volumesum + (facdpedi.canped * facdpedi.factor * almmmatg.libre_d02 / 1000000).
            amounttotal = amounttotal + (IF faccpedi.codmon = 1 THEN facdpedi.implin ELSE (facdpedi.implin * faccpedi.tpocmb)).
            monto = (facdpedi.canped * facdpedi.factor * almmmatg.ctotot).
            IF almmmatg.monvta = 2 THEN monto = monto * almmmatg.tpocmb.
            repositionamount = repositionamount + monto.
        END.

        FIND FIRST ccbcdocu WHERE ccbcdocu.codcia = s-codcia 
            AND ccbcdocu.codped = faccpedi.codref
            AND ccbcdocu.nroped = faccpedi.nroref
            AND ccbcdocu.flgest <> 'A'
            NO-LOCK NO-ERROR.

        FIND FIRST ccbcbult WHERE CcbCBult.CodCia = s-codcia
            AND CcbCBult.CodDoc = faccpedi.coddoc
            AND CcbCBult.NroDoc = faccpedi.nroped
            NO-LOCK NO-ERROR.

        FIND FIRST vtadtrkped WHERE vtadtrkped.CodCia = s-codcia
            AND vtadtrkped.CodDoc = faccpedi.coddoc
            AND vtadtrkped.NroPed = faccpedi.nroped
            AND vtadtrkped.CodUbic = 'PANPX'
            NO-LOCK NO-ERROR.

        PUT UNFORMATTED
            (IF AVAILABLE vtacdocu THEN vtacdocu.nroped ELSE '') '|'
            faccpedi.nroped '|'
            faccpedi.coddoc '|'
            faccpedi.nroref '|'
            faccpedi.divdes '|'
            faccpedi.coddiv '|'
            faccpedi.codcli '|'
            faccpedi.nomcli '|'
            faccpedi.codalm '|'
            faccpedi.codven '|'
            (IF AVAILABLE gn-ven THEN gn-ven.nomven ELSE '') '|'
            faccpedi.sede '|'
            faccpedi.fchent '|'
            faccpedi.fchped '|'
            faccpedi.hora '|'
            pUbigeo '|'
            repositionamount '|'
            faccpedi.fmapgo '|'
            faccpedi.fchped '|'
            faccpedi.hora '|'
            FacCPedi.HorChq '|'
            faccpedi.flgest '|'
            faccpedi.flgsit '|'
            faccpedi.divdes '|'
            amounttotal '|'
            itemsq '|'
            weightsum '|'
            volumesum '|'
            (IF AVAILABLE ccbcdocu THEN ccbcdocu.nrodoc ELSE '') '|'
            (IF AVAILABLE ccbcdocu THEN ccbcdocu.imptot ELSE 0) '|'
            (IF AVAILABLE ccbcdocu THEN ccbcdocu.fchdoc ELSE ?) '|'
            (IF AVAILABLE ccbcdocu THEN ccbcdocu.horcie ELSE '') '|'
            (IF AVAILABLE ccbcbult THEN ccbcbult.fchdoc ELSE ?) '|'
            (IF AVAILABLE ccbcbult THEN ccbcbult.bultos ELSE 0) '|'
            (IF AVAILABLE vtadtrkped THEN DATE(vtadtrkped.fechat) ELSE ?) '|'
            (IF AVAILABLE vtadtrkped THEN ENTRY(2,STRING(vtadtrkped.fechat),' ') ELSE '') '|'
            SKIP.
    END.

END PROCEDURE.
