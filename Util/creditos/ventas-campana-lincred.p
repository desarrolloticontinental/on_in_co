DEF VAR x-codcli AS CHAR FORMAT 'x(11)'.
DEF VAR s-codcia AS INT INIT 001.
DEF VAR cl-codcia AS INT INIT 000.
DEF VAR x-Item  AS INT.
DEF VAR c2015 AS DEC.
DEF VAR c2016 AS DEC.
DEF VAR c2017 AS DEC.
DEF VAR lcMonto AS DEC.
DEF VAR lcDesde AS DATE.
DEF VAR lcHasta AS DATE.
DEF VAR DeudaNac AS DEC.
DEF VAR DeudaExt AS DEC.
DEF VAR x-NomCli AS CHAR.

x-ITEM = 0.
INPUT FROM d:\tmp\clientes.prn.
OUTPUT TO d:\tmp\resumen.txt.
REPEAT :
    IMPORT x-codcli.
    IF x-codcli = '' THEN LEAVE.
    FIND gn-clie WHERE gn-clie.codcia = cl-codcia
        AND gn-clie.codcli = x-codcli
        NO-LOCK NO-ERROR.
    x-NomCli = ''.
    IF AVAILABLE gn-clie THEN x-NomCli = gn-clie.nomcli.
    ASSIGN
        x-ITEM = x-Item + 1
        c2015 = 0
        c2016 = 0
        c2017 = 0
        lcMonto = 0
        lcDesde = ?
        lcHasta = ?
        DeudaNac = 0
        DeudaExt = 0.
    /* Ventas Campaña */
    FOR EACH VentasxCliente NO-LOCK WHERE VentasxCliente.CodCli = x-codcli
        AND VentasxCliente.DateKey >= DATE(10,01,2014)
        AND VentasxCliente.DateKey <= DATE(03,31,2015):
        c2015 = c2015 + VentasxCliente.ImpNacCIGV.
    END.
    FOR EACH VentasxCliente NO-LOCK WHERE VentasxCliente.CodCli = x-codcli
        AND VentasxCliente.DateKey >= DATE(10,01,2015)
        AND VentasxCliente.DateKey <= DATE(03,31,2016):
        c2016 = c2016 + VentasxCliente.ImpNacCIGV.
    END.
    FOR EACH VentasxCliente NO-LOCK WHERE VentasxCliente.CodCli = x-codcli
        AND VentasxCliente.DateKey >= DATE(10,01,2016)
        AND VentasxCliente.DateKey <= DATE(03,31,2017):
        c2017 = c2017 + VentasxCliente.ImpNacCIGV.
    END.
    FOR EACH Gn-ClieL NO-LOCK WHERE Gn-ClieL.CodCia = cl-codcia
        AND Gn-ClieL.CodCli = x-codcli
        AND Gn-ClieL.FchFin <= DATE(03,31,2017):
        ASSIGN
            lcMonto = Gn-ClieL.ImpLC
            lcDesde = Gn-ClieL.FchIni 
            lcHasta = Gn-ClieL.FchFin.
    END.
    FOR EACH ccbcdocu USE-INDEX llave06  NO-LOCK WHERE ccbcdocu.codcia = s-codcia
        AND ccbcdocu.codcli = x-codcli
        AND ccbcdocu.flgest = 'P':
        CASE ccbcdocu.codmon:
            WHEN 1 THEN DO:
                IF LOOKUP(ccbcdocu.coddoc, 'FAC,BOL,LET,N/D,CHQ,DCO') > 0 
                    THEN DeudaNac = DeudaNac + ccbcdocu.sdoact.
                IF LOOKUP(ccbcdocu.coddoc, 'N/C,A/C,BD,A/R') > 0 
                    THEN DeudaNac = DeudaNac - ccbcdocu.sdoact.
            END.
            WHEN 2 THEN DO:
                IF LOOKUP(ccbcdocu.coddoc, 'FAC,BOL,LET,N/D,CHQ,DCO') > 0 
                    THEN DeudaExt = DeudaExt + ccbcdocu.sdoact.
                IF LOOKUP(ccbcdocu.coddoc, 'N/C,A/C,BD,A/R') > 0 
                    THEN DeudaExt = DeudaExt - ccbcdocu.sdoact.
            END.
        END CASE.
    END.
    PUT UNFORMATTED
        x-item '|'
        x-codcli '|'
        x-NomCli '|'
        c2015 '|'
        c2016 '|'
        c2017 '|'
        lcMonto '|'
        lcDesde '|'
        lcHasta '|'
        DeudaNac '|'
        DeudaExt
        SKIP.
END.
INPUT CLOSE.
OUTPUT CLOSE.

