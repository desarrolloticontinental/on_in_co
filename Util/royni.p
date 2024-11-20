def var x-linea as char format 'x(500)'.

input from c:\tmp\producto.prn.
repeat:
    import unformatted x-linea.
    create pr-mcpp.
    assign
        Pr-Mcpp.CodCia = 001
        Pr-Mcpp.codmat = substring(x-linea,1,6)
        Pr-Mcpp.Periodo = 2007
        Pr-Mcpp.ProPro = decimal(substring(x-linea,8,9))
        Pr-Mcpp.ProVta = decimal(substring(x-linea,17,8))
        Pr-Mcpp.ProVtaCan[1] = decimal(substring(x-linea,25,8))
        Pr-Mcpp.ProVtaCan[2] = decimal(substring(x-linea,33,8))
        Pr-Mcpp.ProVtaCan[3] = decimal(substring(x-linea,41,8))
        Pr-Mcpp.ProVtaCan[5] = decimal(substring(x-linea,49,8))
        Pr-Mcpp.ProVtaCan[8] = decimal(substring(x-linea,57,8))
        Pr-Mcpp.ProVtaCan[11] = decimal(substring(x-linea,65,8))
        Pr-Mcpp.ProVtaCan[12] = decimal(substring(x-linea,73,8))
        Pr-Mcpp.ProVtaCan[14] = decimal(substring(x-linea,81,8))
        Pr-Mcpp.ProVtaCan[15] = decimal(substring(x-linea,89,8))
        Pr-Mcpp.ProVtaCan[16] = decimal(substring(x-linea,97,8))
        Pr-Mcpp.ProVtaCanAte[1] = decimal(substring(x-linea,105,8))
        Pr-Mcpp.ProVtaCanAte[2] = decimal(substring(x-linea,113,8))
        Pr-Mcpp.ProVtaCanAte[3] = decimal(substring(x-linea,121,8))
        Pr-Mcpp.ProVtaCanAte[4] = decimal(substring(x-linea,129,8))
        Pr-Mcpp.ProVtaCanAte[10] = decimal(substring(x-linea,137,8)).
end.
input close.


