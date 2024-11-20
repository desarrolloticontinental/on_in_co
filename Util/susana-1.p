def var x-linea as char format 'x(249)'.

output to c:\tmp\susana-1.txt.
for each ccbcdocu no-lock where codcia = 1 and coddoc = 'fac' and
     fchdoc >= date(04,01,2004) and fchdoc <= date(04,15,2004)
     and flgest <> 'a' and coddiv = '00000',
     first gn-convt where gn-ConVt.Codig = fmapgo:
    if codmon = 1 and imptot > 400 then next.
    if codmon = 2 and tpocmb * imptot > 400 then next.
    x-linea = coddoc + '|' + nrodoc + '|' + nomcli + '|' + NroRef + '|' + gn-ConVt.Nombr + '|' + string(codmon, '9') + '|' + string(imptot, '>>>,>>>,>>9.99') + '|' + flgest.
    display x-linea with no-labels no-box stream-io width 250.
end.
output close.
