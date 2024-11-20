OUTPUT TO d:\condiciones.txt.
PUT UNFORMATTED
    'TIPO|%DCTO|CODIGO|ESTADO|DESCRIPCION|VENCIMIENTOS|TOTAL DIAS|'
    'TIPVTA|ACEPTA FAI|CONDVTA UNICA CAMPAÑA|ANTICIPO CAMPAÑA|'
    'CANT LETRAS|FECHVTO CAMPAÑA' SKIP.
FOR EACH dsctos NO-LOCK WHERE LOOKUP(dsctos.clfcli, 'P,T') > 0, 
    FIRST gn-convt NO-LOCK WHERE gn-ConVt.Codig = Dsctos.CndVta:
    PUT UNFORMATTED
         dsctos.clfcli '|'
         dsctos.pordto '|'
         dsctos.cndvta '|'
         gn-convt.estado '|'
         gn-convt.nombr '|'
         gn-convt.vencmtos '|'
         gn-convt.totdias '|'
         gn-convt.tipvta '|'
         gn-convt.libre_l01 '|'
         gn-convt.libre_l02 '|'
         gn-convt.libre_l03 '|'
         gn-convt.libre_d01 '|'
         gn-convt.libre_f01 '|'
         SKIP.
 END.
 OUTPUT CLOSE.

