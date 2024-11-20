OUTPUT TO c:\tmp\vales.txt.
FOR EACH vtadtickets NO-LOCK WHERE codcia = 1,
    EACH ccbdcaja NO-LOCK WHERE ccbdcaja.codcia = 001
    AND ccbdcaja.coddoc = vtadtickets.codref
    AND ccbdcaja.nrodoc = vtadtickets.nroref,
    FIRST gn-prov WHERE gn-prov.codcia = 000
    AND gn-prov.codpro = vtadtickets.codpro:
    DISPLAY
        VtaDTickets.CodDiv 
        VtaDTickets.CodRef 
        VtaDTickets.NroRef 
        date(VtaDTickets.Fecha ) LABEL 'Fecha'
        ccbdcaja.codref
        ccbdcaja.nroref
        VtaDTickets.CodPro 
        gn-prov.nompro
        VtaDTickets.Producto
        VtaDTickets.NroTck 
        VtaDTickets.Valor
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
