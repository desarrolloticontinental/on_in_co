DEF VAR Fill-in-dep AS CHAR FORMAT 'x(20)'.
DEF VAR Fill-in-prov AS CHAR FORMAT 'x(20)'.
DEF VAR Fill-in-dis AS CHAR FORMAT 'x(20)'.
DEF VAR x-inicio AS DATE.
DEF VAR x-fin AS DATE.

DEF BUFFER bdocu FOR ccbcdocu.

ASSIGN
    x-inicio = 08/01/2013
    x-fin    = 08/30/2013.

/* G/R POR VENTAS */
OUTPUT TO c:\tmp\hr-ventas.txt.
FOR EACH gn-divi WHERE codcia = 1 NO-LOCK:
    FOR EACH di-rutac NO-LOCK WHERE di-rutac.codcia = 1
        AND di-rutac.coddiv = gn-divi.coddiv
        AND di-rutac.fchdoc >= x-inicio
        AND di-rutac.fchdoc <= x-fin
        AND di-rutac.flgest <> 'A',
        FIRST gn-vehic NO-LOCK WHERE gn-vehic.codcia = di-rutac.codcia
        AND gn-vehic.placa = di-rutac.codveh,
        di-rutad OF di-rutac NO-LOCK,
        FIRST ccbcdocu NO-LOCK USE-INDEX llave01 WHERE ccbcdocu.codcia = di-rutac.codcia
        AND ccbcdocu.coddoc = DI-RutaD.CodRef
        AND ccbcdocu.nrodoc = di-rutad.nroref,
        FIRST gn-clie NO-LOCK WHERE gn-clie.codcia = 0
        AND gn-clie.codcli = ccbcdocu.codcli,
        FIRST bdocu NO-LOCK WHERE bdocu.codcia = di-rutac.codcia
        AND bdocu.coddoc = ccbcdocu.codref
        AND bdocu.nrodoc = ccbcdocu.nroref:
        FIND TabDepto WHERE TabDepto.CodDepto = gn-clie.coddept
            NO-LOCK NO-ERROR.
        IF AVAILABLE TabDepto 
            THEN Fill-in-dep = TabDepto.NomDepto.
        ELSE Fill-in-dep = "".
        FIND Tabprovi WHERE Tabprovi.CodDepto = gn-clie.CodDept
            AND Tabprovi.Codprovi = gn-clie.codprov NO-LOCK NO-ERROR.
        IF AVAILABLE Tabprovi 
            THEN fill-in-prov = Tabprovi.Nomprovi.
        ELSE fill-in-prov = "".
        FIND Tabdistr WHERE Tabdistr.CodDepto = gn-clie.CodDept
            AND Tabdistr.Codprovi = gn-clie.codprov
            AND Tabdistr.Coddistr = gn-clie.coddist NO-LOCK NO-ERROR.
        IF AVAILABLE Tabdistr 
            THEN Fill-in-dis = Tabdistr.Nomdistr .
        ELSE Fill-in-dis = "".

        DISPLAY
            di-rutac.coddiv
            di-rutac.nrodoc
            di-rutac.fchdoc
            di-rutac.flgest
            di-rutac.codveh FORMAT 'x(10)'
            gn-vehic.marca FORMAT 'x(15)'
            di-rutac.nomtra FORMAT 'x(30)'
            di-rutad.codref
            di-rutad.nroref
            di-rutad.flgest FORMAT 'x(2)'
            ccbcdocu.nomcli FORMAT 'x(40)'
            /*gn-clie.dircli FORMAT 'x(30)'*/
            Fill-in-dep
            fill-in-prov
            fill-in-dis
            ccbcdocu.codref
            ccbcdocu.nroref
            bdocu.codmon
            bdocu.imptot
            WITH STREAM-IO NO-BOX WIDTH 600.
    END.
END.
OUTPUT CLOSE.

/* G/R POR TRANSFERENCIAS */
OUTPUT TO c:\tmp\hr-transf.txt.
FOR EACH gn-divi WHERE codcia = 1 NO-LOCK:
    FOR EACH di-rutac NO-LOCK WHERE di-rutac.codcia = 1
        AND di-rutac.coddiv = gn-divi.coddiv
        AND di-rutac.fchdoc >= x-inicio
        AND di-rutac.fchdoc <= x-fin
        AND di-rutac.flgest <> 'A',
        FIRST gn-vehic NO-LOCK WHERE gn-vehic.codcia = di-rutac.codcia
        AND gn-vehic.placa = di-rutac.codveh,
        di-rutag OF di-rutac NO-LOCK,
        FIRST Almcmov WHERE Almcmov.CodCia = Di-RutaG.CodCia
        AND Almcmov.CodAlm = Di-RutaG.CodAlm
        AND Almcmov.TipMov = Di-RutaG.Tipmov
        AND Almcmov.CodMov = Di-RutaG.Codmov
        AND Almcmov.NroSer = Di-RutaG.serref
        AND Almcmov.NroDoc = Di-RutaG.nroref:
        DISPLAY
            di-rutac.coddiv
            di-rutac.nrodoc
            di-rutac.fchdoc
            di-rutac.flgest
            di-rutac.codveh FORMAT 'x(10)'
            gn-vehic.marca
            di-rutac.nomtra
            di-rutag.serref
            di-rutag.nroref
            di-rutag.flgest FORMAT 'x(2)'
            di-rutag.codalm
            almcmov.almdes
            almcmov.fchdoc
            WITH STREAM-IO NO-BOX WIDTH 320.
    END.
END.
OUTPUT CLOSE.


