DEFINE TEMP-TABLE t-Almacen_Stocks NO-UNDO LIKE Almacen_Stocks.

DEF VAR s-codcia AS INTE INIT 001.
DEF VAR x-codmat AS CHAR INIT '077722'.

/* REPOSICIONES */
FOR EACH integral.Almacen NO-LOCK WHERE integral.Almacen.codcia = s-codcia
        AND LOOKUP(integral.Almacen.CodAlm, '997,998') = 0,
    EACH integral.Almcrepo NO-LOCK WHERE integral.Almcrepo.codcia = integral.Almacen.codcia
        AND lookup(tipmov, 'A,M') > 0
        AND integral.Almcrepo.CodAlm = integral.Almacen.CodAlm
        AND integral.Almcrepo.FlgEst = 'P',
    EACH integral.Almdrepo OF integral.Almcrepo NO-LOCK WHERE integral.almdrepo.CanApro > integral.almdrepo.CanAten
    AND integral.almdrepo.codmat = x-codmat:
    FIND FIRST t-Almacen_Stocks WHERE t-Almacen_Stocks.codalm = integral.Almcrepo.CodAlm
        AND t-Almacen_Stocks.codmat = integral.Almdrepo.CodMat
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE t-Almacen_Stocks THEN DO:
        CREATE t-Almacen_Stocks.
        ASSIGN
            t-Almacen_Stocks.codalm = TRIM(integral.Almcrepo.CodAlm)
            t-Almacen_Stocks.codmat = TRIM(integral.Almdrepo.CodMat).
    END.
    ASSIGN 
        t-Almacen_Stocks.TrfTransito = t-Almacen_Stocks.TrfTransito + 
        (integral.Almdrepo.CanApro - integral.Almdrepo.CanAten).
END.

/* TRANSFERENCIAS */
DEF VAR cAlmDes AS CHAR NO-UNDO.
FOR EACH integral.Almacen NO-LOCK WHERE integral.Almacen.codcia = s-codcia,
    EACH integral.Almcmov NO-LOCK WHERE integral.Almcmov.codcia = integral.Almacen.codcia
        AND integral.Almcmov.codalm = integral.Almacen.codalm
        AND integral.Almcmov.tipmov = "S"
        AND integral.Almcmov.codmov = 03
        AND integral.Almcmov.flgest <> "A"
        AND integral.Almcmov.flgsit = "T",
    EACH integral.Almdmov OF integral.Almcmov NO-LOCK WHERE integral.almdmov.codmat = x-codmat:
    /* RHC 05/02/2018 Dos casos: 
    1. Salida con Cross Docking
    2. Salida sin Cross Docking 
    */
    IF Almcmov.CrossDocking = YES THEN cAlmDes = INTEGRAL.Almcmov.AlmacenXD.     /* Destino Final */
    ELSE cAlmDes = INTEGRAL.Almcmov.AlmDes.  /* ALmacén Destino */
    FIND FIRST t-Almacen_Stocks WHERE t-Almacen_Stocks.codalm = cAlmDes
        AND t-Almacen_Stocks.codmat = integral.Almdmov.CodMat
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE t-Almacen_Stocks THEN DO:
        CREATE t-Almacen_Stocks.
        ASSIGN
            t-Almacen_Stocks.codalm = TRIM(cAlmDes)
            t-Almacen_Stocks.codmat = TRIM(integral.Almdmov.CodMat).
    END.
    ASSIGN 
        t-Almacen_Stocks.TrfTransito = t-Almacen_Stocks.TrfTransito + integral.Almdmov.candes.
END.

/* ORDENES DE TRANSFERENCIA */
FOR EACH integral.gn-divi NO-LOCK WHERE INTEGRAL.GN-DIVI.CodCia = s-codcia,
    EACH INTEGRAL.FacCPedi NO-LOCK WHERE INTEGRAL.FacCPedi.CodCia = INTEGRAL.GN-DIVI.CodCia
        AND INTEGRAL.FacCPedi.CodDiv = INTEGRAL.GN-DIVI.CodDiv
        AND INTEGRAL.FacCPedi.CodDoc = "OTR"
        AND INTEGRAL.FacCPedi.FlgEst = "P"
        AND INTEGRAL.FacCPedi.CodRef = "R/A",
    EACH INTEGRAL.Facdpedi OF INTEGRAL.FacCPedi NO-LOCK WHERE INTEGRAL.Facdpedi.flgest = 'P' AND facdpedi.codmat = x-codmat:
    /* RHC 05/02/2018 Dos casos: 
    1. Salida con Cross Docking
    2. Salida sin Cross Docking 
    */
    IF INTEGRAL.FacCPedi.CrossDocking = YES THEN cAlmDes = INTEGRAL.FacCPedi.AlmacenXD.     /* Destino Final */
    ELSE cAlmDes = INTEGRAL.FacCPedi.CodCli.  /* Almacén Destino */
    FIND FIRST t-Almacen_Stocks WHERE t-Almacen_Stocks.codalm = cAlmDes
        AND t-Almacen_Stocks.codmat = integral.Facdpedi.CodMat
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE t-Almacen_Stocks THEN DO:
        CREATE t-Almacen_Stocks.
        ASSIGN
            t-Almacen_Stocks.codalm = cAlmDes
            t-Almacen_Stocks.codmat = TRIM(integral.Facdpedi.CodMat).
    END.
    ASSIGN
        t-Almacen_Stocks.TrfTransito = t-Almacen_Stocks.TrfTransito +
        integral.Facdpedi.Factor * (integral.Facdpedi.CanPed - integral.Facdpedi.CanAte).
END.
/* ********************************************************************************************************* */
/* RHC 16/06/2021 Sloting */
/* ********************************************************************************************************* */
FOR EACH integral.OOMoviAlmacen NO-LOCK WHERE integral.OOMoviAlmacen.codcia = s-codcia
    AND integral.OOMoviAlmacen.FlagMigracion = "N" 
    AND integral.OOMoviAlmacen.TipMov = "I" 
    AND integral.OOMoviAlmacen.CodMov = 03
    AND integral.oomovialmacen.codmat = x-codmat:
    FIND FIRST t-Almacen_Stocks WHERE t-Almacen_Stocks.codalm = integral.OOMoviAlmacen.CodAlm
        AND t-Almacen_Stocks.codmat = integral.OOMoviAlmacen.CodMat
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE t-Almacen_Stocks THEN DO:
        CREATE t-Almacen_Stocks.
        ASSIGN
            t-Almacen_Stocks.codalm = integral.OOMoviAlmacen.CodAlm
            t-Almacen_Stocks.codmat = integral.OOMoviAlmacen.CodMat.
    END.
    ASSIGN 
        t-Almacen_Stocks.TrfTransito = t-Almacen_Stocks.TrfTransito + 
                                                (integral.OOMoviAlmacen.CanDes * integral.OOMoviAlmacen.Factor).
END.
FOR EACH integral.OOMoviAlmacen NO-LOCK WHERE integral.OOMoviAlmacen.codcia = s-codcia
    AND integral.OOMoviAlmacen.FlagMigracion = "N" 
    AND integral.OOMoviAlmacen.FchDoc >= DATE(06,01,2019) 
    AND integral.OOMoviAlmacen.TipMov = "I" 
    AND integral.OOMoviAlmacen.CodMov = 90
    AND integral.OOMoviAlmacen.UseInDropShipment = "NO"
    AND integral.OOMoviAlmacen.CodMat = x-codmat:
    FIND FIRST t-Almacen_Stocks WHERE t-Almacen_Stocks.codalm = integral.OOMoviAlmacen.CodAlm
        AND t-Almacen_Stocks.codmat = integral.OOMoviAlmacen.CodMat
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE t-Almacen_Stocks THEN DO:
        CREATE t-Almacen_Stocks.
        ASSIGN
            t-Almacen_Stocks.codalm = integral.OOMoviAlmacen.CodAlm
            t-Almacen_Stocks.codmat = integral.OOMoviAlmacen.CodMat.
    END.
    ASSIGN 
        t-Almacen_Stocks.TrfTransito = t-Almacen_Stocks.TrfTransito + 
                                                (integral.OOMoviAlmacen.CanDes * integral.OOMoviAlmacen.Factor).
END.
FOR EACH integral.OOMoviAlmacen NO-LOCK WHERE integral.OOMoviAlmacen.codcia = s-codcia
    AND integral.OOMoviAlmacen.FlagMigracion = "N" 
    AND integral.OOMoviAlmacen.TipMov = "I" 
    AND (integral.OOMoviAlmacen.CodMov = 09 OR integral.OOMoviAlmacen.CodMov = 30)
    AND integral.OOMoviAlmacen.UseInDropShipment = "NO"
    AND integral.OOMoviAlmacen.CodMat = x-codmat:
    FIND FIRST t-Almacen_Stocks WHERE t-Almacen_Stocks.codalm = integral.OOMoviAlmacen.CodAlm
        AND t-Almacen_Stocks.codmat = integral.OOMoviAlmacen.CodMat
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE t-Almacen_Stocks THEN DO:
        CREATE t-Almacen_Stocks.
        ASSIGN
            t-Almacen_Stocks.codalm = integral.OOMoviAlmacen.CodAlm
            t-Almacen_Stocks.codmat = integral.OOMoviAlmacen.CodMat.
    END.
    ASSIGN 
        t-Almacen_Stocks.TrfTransito = t-Almacen_Stocks.TrfTransito + 
                                                (integral.OOMoviAlmacen.CanDes * integral.OOMoviAlmacen.Factor).
END.
/* ********************************************************************************************************* */
/* ********************************************************************************************************* */
FOR EACH t-almacen_stocks:
    DISPLAY t-Almacen_Stocks.codalm t-Almacen_Stocks.codmat t-Almacen_Stocks.TrfTransito.
END.
