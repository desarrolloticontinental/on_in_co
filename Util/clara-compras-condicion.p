def var x-impnac as dec format '>>>,>>>,>>9.99' no-undo.
def var x-impusa as dec format '>>>,>>>,>>9.99' no-undo.

output to c:\tmp\compras2006.txt.
for each lg-cocmp no-lock where codcia = 001 and coddiv = '00000'
    and flgsit <> 'a' and fchdoc >= 01/01/2006
    and fchdoc <= 12/31/2006,
    first gn-concp no-lock where gn-concp.codig = lg-cocmp.cndcmp
    break  by LG-COCmp.CndCmp:
    if first-of(cndcmp)
    then assign
            x-impnac = 0
            x-impusa = 0.
    if codmon = 1
    then x-impnac = x-impnac + imptot.
    else x-impusa = x-impusa + imptot.
    if last-of(cndcmp) then do:
        display
            Gn-ConCp.Codig Gn-ConCp.Nombr x-impnac x-impusa
            with stream-io no-box width 200.
    end.
end.
output close.
