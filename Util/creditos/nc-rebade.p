/* NOTAS DE CREDIT OTRAS POR REBADE */

DEF TEMP-TABLE t-cdocu LIKE ccbcdocu.
DEF TEMP-TABLE t-ddocu LIKE ccbddocu.

DEF VAR x-linea AS CHAR FORMAT 'x(100)'.

FIND FIRST Faccfggn WHERE codcia = 1 NO-LOCK.
FIND Faccorre WHERE faccorre.codcia = 1
    AND Faccorre.coddoc = 'N/C'
    AND Faccorre.nroser = 001
    EXCLUSIVE-LOCK.

INPUT FROM c:\tmp\multicopias.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea <> '' THEN DO:
        FIND ccbcdocu WHERE codcia = 1
            AND coddoc = SUBSTRING(x-linea,1,3)
            AND nrodoc = SUBSTRING(x-linea,11,9)
            NO-LOCK.
        CREATE t-cdocu.
        ASSIGN
            t-cdocu.codcia = 1
            t-cdocu.coddoc = 'N/C'
            t-cdocu.coddiv = '00000'
            t-cdocu.nrodoc = STRING(Faccorre.nroser, '999') + STRING(Faccorre.correlativo, '999999')
            t-cdocu.fchdoc = 11/30/2011     /* OJO */
            t-cdocu.codcli = ccbcdocu.codcli
            t-cdocu.ruccli = ccbcdocu.ruccli
            t-cdocu.nomcli = ccbcdocu.nomcli
            t-cdocu.dircli = ccbcdocu.dircli
            t-cdocu.porigv = ( IF ccbcdocu.porigv > 0 THEN ccbcdocu.porigv ELSE 18.00 )
            t-cdocu.codmon = 1
            t-cdocu.usuario = 'ADMIN'
            t-cdocu.tpocmb = Faccfggn.tpocmb[1]
            t-cdocu.codref = ccbcdocu.coddoc
            t-cdocu.nroref = ccbcdocu.nrodoc
            t-cdocu.codven = ccbcdocu.codven
            t-cdocu.cndcre = 'N'
            t-cdocu.fmapgo = ccbcdocu.fmapgo.
        /* ACTUALIZAMOS EL CENTRO DE COSTO */
        FIND GN-VEN WHERE GN-VEN.codcia = 1
            AND GN-VEN.codven = t-cdocu.codven NO-LOCK NO-ERROR.
        IF AVAILABLE GN-VEN THEN t-cdocu.cco = GN-VEN.cco.
        /* GLOSA */
/*         IF SUBSTRING(x-linea,20,9) <> '' THEN DO:                  */
/*             t-cdocu.glosa = 'REF. N/C ' + SUBSTRING(x-linea,20,9). */
/*         END.                                                       */
        t-cdocu.glosa = 'DESCUENTO POR VOLUMEN'.

        ASSIGN
            Faccorre.correlativo = Faccorre.correlativo + 1.
        FIND CcbTabla WHERE CcbTabla.CodCia = 1
            AND CcbTabla.Tabla  = 'N/C' 
            AND CcbTabla.Codigo = '00002' NO-LOCK.
        CREATE t-ddocu.
        BUFFER-COPY t-cdocu TO t-ddocu.
        ASSIGN
            t-ddocu.codmat = CcbTabla.Codigo
            t-ddocu.factor = 1
            t-ddocu.candes = 1
            t-ddocu.preuni = DECIMAL(SUBSTRING(x-linea,20))
            t-ddocu.implin = t-ddocu.CanDes * t-ddocu.PreUni.
        IF CcbTabla.Afecto THEN
            ASSIGN
            t-ddocu.AftIgv = Yes
            t-ddocu.ImpIgv = (t-ddocu.CanDes * t-ddocu.PreUni) * ((t-cdocu.PorIgv / 100) / (1 + (t-cdocu.PorIgv / 100))).
        ELSE
            ASSIGN
            t-ddocu.AftIgv = No
            t-ddocu.ImpIgv = 0.
        t-ddocu.NroItm = 1.
        ASSIGN
          t-cdocu.ImpBrt = 0
          t-cdocu.ImpExo = 0
          t-cdocu.ImpDto = 0
          t-cdocu.ImpIgv = 0
          t-cdocu.ImpTot = 0.
        FOR EACH t-ddocu OF t-cdocu NO-LOCK:
          ASSIGN
                t-cdocu.ImpBrt = t-cdocu.ImpBrt + (IF t-ddocu.AftIgv = Yes THEN t-ddocu.PreUni * t-ddocu.CanDes ELSE 0)
                t-cdocu.ImpExo = t-cdocu.ImpExo + (IF t-ddocu.AftIgv = No  THEN t-ddocu.PreUni * t-ddocu.CanDes ELSE 0)
                t-cdocu.ImpDto = t-cdocu.ImpDto + t-ddocu.ImpDto
                t-cdocu.ImpIgv = t-cdocu.ImpIgv + t-ddocu.ImpIgv
                t-cdocu.ImpTot = t-cdocu.ImpTot + t-ddocu.ImpLin.
        END.
        ASSIGN 
            t-cdocu.ImpVta = t-cdocu.ImpBrt - t-cdocu.ImpIgv
            t-cdocu.ImpBrt = t-cdocu.ImpBrt - t-cdocu.ImpIgv + t-cdocu.ImpDto
            t-cdocu.SdoAct = t-cdocu.ImpTot
            t-cdocu.FlgEst = 'P'.
    END.
END.
INPUT CLOSE.

FOR EACH t-cdocu:
    CREATE ccbcdocu.
    BUFFER-COPY t-cdocu TO ccbcdocu.
    FOR EACH t-ddocu OF t-cdocu:
        CREATE ccbddocu.
        BUFFER-COPY t-ddocu TO ccbddocu.
    END.
    DISPLAY ccbcdocu.nrodoc.
END.
