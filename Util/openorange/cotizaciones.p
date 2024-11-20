OUTPUT TO c:\tmp\detallado.txt.
PUT UNFORMATTED
    "NRO OPEN|NRO PROGRESS|CLIENTE|NOMBRE|EMISION|ENTREGA|CONDICION|VENDEDOR" +
    "|CODIGO|DESRIPCION|CANTIDAD|UNITARIO|DTO1|DTO2|DTO3|IMPORTE|UNIDAD"
    SKIP.
FOR EACH opencexpo NO-LOCK WHERE flagmigracion = 'S' AND fchped >= 01/06/2015,
    EACH opendexpo NO-LOCK WHERE opendexpo.Nrotrans = opencexpo.Nrotrans,
    FIRST almmmatg OF opendexpo:
    PUT UNFORMATTED
        OpenCExpo.NroTrans '|'
        OpenCExpo.NroPed '|'
        OpenCExpo.CodCli '|'        
        OpenCExpo.NomCli '|'
        OpenCExpo.FchPed '|'
        OpenCExpo.FchEnt '|'
        OpenCExpo.FmaPgo '|'
        OpenCExpo.CodVen '|'
        OpenDExpo.Codmat '|'
        almmmatg.desmat  '|'
        OpenDExpo.CanPed '|'
        OpenDExpo.PreUni '|'
        OpenDExpo.PorDto1 '|'
        OpenDExpo.PorDto2 '|'
        OpenDExpo.PorDto3 '|'
        OpenDExpo.ImpLin  '|'
        OpenDExpo.UndVta
        SKIP.
END.
OUTPUT CLOSE.

