DEF VAR f-estado AS CHAR.
OUTPUT TO d:\tmp\uso-interno.txt.
PUT UNFORMATTED
    'ESTADO|USUARIO|NUMERO|FECHA|OBSERVACIONES|ALMACEN|CCO|ARTICULO|DESCRIPCION|CANTIDAD|UNIDAD'
    SKIP.
FOR EACH almcdocu NO-LOCK WHERE codcia = 1
    AND coddoc = 'SUI'
    AND LOOKUP(flgest, 'V,R,A,C,S,E') = 0,
    EACH almddocu NO-LOCK WHERE AlmDDocu.CodCia = almcdocu.codcia
    AND AlmDDocu.CodLlave = almcdocu.codllave
    AND AlmDDocu.CodDoc = almcdocu.coddoc
    AND AlmDDocu.NroDoc = almcdocu.nrodoc,
    FIRST almmmatg NO-LOCK WHERE almmmatg.codcia = almddocu.codcia
    AND almmmatg.codmat = almddocu.codigo:
    RUN vta2/p-faccpedi-flgest (AlmCDocu.flgest, AlmCDocu.coddoc, OUTPUT f-Estado).
    PUT UNFORMATTED
        f-estado '|'
        almcdocu.codllave '|'
        almcdocu.nrodoc '|'
        almcdocu.fchdoc '|'
        almcdocu.libre_c01 '|'
        almcdocu.libre_c02 '|'
        almcdocu.libre_c05 '|'
        almddocu.codigo '|'
        almmmatg.desmat '|'
        AlmDDocu.Libre_d02 '|'
        almmmatg.undstk
        SKIP.

END.

/*
        CASE pFlgEst:
            WHEN 'T' THEN pEstado = "POR APROBAR".
            WHEN 'WL' THEN pEstado = "POR APROBAR POR CONTRALORIA".
            WHEN 'P' THEN pEstado = "APROBADO".
            WHEN 'V' THEN pEstado = "VENCIDO".
            WHEN 'R' THEN pEstado = "RECHAZADO".
            WHEN 'A' THEN pEstado = "ANULADO".
            WHEN 'C' THEN pEstado = "ATENDIDO TOTALMENTE".
            WHEN 'S' THEN pEstado = "SUSPENDIDO".
            WHEN 'E' THEN pEstado = "CERRADO MANUALMENTE".
        END CASE.
*/
