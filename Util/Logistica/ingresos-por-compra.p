DEF TEMP-TABLE detalle LIKE almdmov
    FIELD codpro LIKE almcmov.codpro
    FIELD nomref LIKE almcmov.nomref
    FIELD nrorf1 LIKE almcmov.nrorf1.

FOR EACH almacen NO-LOCK WHERE codcia = 1:
    FOR EACH almcmov NO-LOCK WHERE almcmov.codcia = 1
        AND almcmov.codalm = almacen.codalm
        AND almcmov.tipmov = 'i'
        AND almcmov.codmov = 02
        AND almcmov.fchdoc >= 11/01/2010
        AND almcmov.fchdoc <= 05/31/2011
        AND almcmov.flgest <> 'a',
        EACH almdmov OF almcmov NO-LOCK:
        DISPLAY almcmov.codalm almcmov.fchdoc.
        PAUSE 0.
        FIND lg-docmp WHERE lg-docmp.codcia = 1
            AND lg-docmp.coddiv = '00000'
            AND lg-docmp.tpodoc = "N"
            AND lg-docmp.nrodoc = INTEGER(almcmov.nrorf1)
            AND lg-docmp.codmat = almdmov.codmat
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE lg-docmp THEN NEXT.
        CREATE detalle.
        BUFFER-COPY almdmov
            TO detalle
            ASSIGN
                detalle.codpro = almcmov.codpro
                detalle.nrorf1 = almcmov.nrorf1
                detalle.nomref = almcmov.nomref
                detalle.fchdoc = almcmov.fchdoc.
        FIND gn-prov WHERE gn-prov.codcia = 0
            AND gn-prov.codpro = almcmov.codpro NO-LOCK NO-ERROR.
        IF AVAILABLE gn-prov THEN detalle.nomref = gn-prov.nompro.
        IF almcmov.codmon = 2 THEN detalle.preuni = almdmov.preuni * almcmov.tpocmb.
        detalle.impcto = detalle.candes * detalle.preuni * (1 + LG-DOCmp.IgvMat / 100).
    END.
END.

OUTPUT TO c:\tmp\INFO.txt.
FOR EACH detalle:
    PUT UNFORMATTED
        detalle.codalm '|'
        detalle.fchdoc '|'
        detalle.nrodoc '|'
        detalle.nrorf1 '|'
        detalle.codpro ' ' detalle.nomref '|'
        detalle.impcto
        SKIP.
END.
OUTPUT CLOSE.

