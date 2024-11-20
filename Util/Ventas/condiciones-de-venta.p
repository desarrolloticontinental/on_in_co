DEF VAR x-propios AS DEC.
DEF VAR x-terceros AS DEC.
OUTPUT TO d:\tmp\condvta.txt.
PUT UNFORMATTED
    'ESTADO|CODIGO|DESCRIPCION|DIAS|VENCIMIENTOS|%DCTO PROPIOS|%DTO TERCEROS'
    SKIP.
FOR EACH gn-convt NO-LOCK:
    ASSIGN x-propios = 0 x-terceros = 0.
    FIND FIRST dsctos WHERE  Dsctos.CndVta = gn-ConVt.Codig
        AND Dsctos.clfCli = 'P'
        NO-LOCK NO-ERROR.
    IF AVAILABLE dsctos THEN x-propios = Dsctos.PorDto.
    FIND FIRST dsctos WHERE  Dsctos.CndVta = gn-ConVt.Codig
        AND Dsctos.clfCli = 'T'
        NO-LOCK NO-ERROR.
    IF AVAILABLE dsctos THEN x-terceros = Dsctos.PorDto.
    PUT UNFORMATTED
        gn-ConVt.Estado '|'
        gn-ConVt.Codig  '|'
        gn-ConVt.Nombr  '|'
        gn-ConVt.TotDias   '|'
        gn-ConVt.Vencmtos '|'
        x-propios         '|'
        x-terceros '|'
        SKIP.

END.
