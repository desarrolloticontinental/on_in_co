DEF BUFFER B-CMOV FOR Almcmov.      
DEF BUFFER B-DMOV FOR Almdmov.      

FOR EACH almacen NO-LOCK WHERE codcia = 1:
    FOR EACH almcmov WHERE codcia = 1 
        AND codalm = almacen.codalm
        AND tipmov = 'S'
        AND codmov = 14:
        FOR EACH almdmov OF almcmov, FIRST almmmatg OF almdmov NO-LOCK:
            /* VALORIZACION DE LA SALIDA */
            FIND LAST Almstkge WHERE Almstkge.codcia = 1
                AND Almstkge.codmat = almdmov.CodMat
                AND Almstkge.fecha <= almcmov.FchDoc
                NO-LOCK NO-ERROR.
            IF AVAILABLE Almstkge 
                THEN ASSIGN
                almdmov.PreUni = AlmStkge.CtoUni * almdmov.factor
                almdmov.ImpCto = almdmov.CanDes * almdmov.PreUni.
            /* VALORIZACION DEL INGRESO */
            FIND B-DMOV WHERE B-DMOV.codcia = Almdmov.codcia
                AND B-DMOV.codalm = Almdmov.codalm
                AND B-DMOV.tipmov = "I"
                AND B-DMOV.codmov = Almdmov.codmov
                AND B-DMOV.nroser = INTEGER(SUBSTRING(Almcmov.nroref,1,3))
                AND B-DMOV.nrodoc = INTEGER(SUBSTRING(Almcmov.nroref,4))
                AND B-DMOV.codmat = Almdmov.codant
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE B-DMOV THEN DO:
                ASSIGN 
                    B-DMOV.ImpCto = almdmov.ImpCto
                    B-DMOV.PreUni = (B-DMOV.ImpCto / B-DMOV.CanDes).
            END.
            ELSE MESSAGE almcmov.codalm SUBSTRING(almcmov.nroref,1,3) SUBSTRING(almcmov.nroref,4)
                almdmov.codant.
        END.
    END.
END.
