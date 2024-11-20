def var x-ruc as char.
def var x-apepat as char.
def var x-apemat as char.
def var x-nombre as char.
def var x-nroser as char.
def var x-nrodoc as char.
def var x-fchdoc as date.
def var x-imptot as dec.
def var x-para1 as char format 'x'.

output to c:\tmp\062120100038146200602.txt.
for each cb-dmov where codcia = 001
    and periodo = 2006
    and nromes = 02
    and codope = '073'
    and codcta begins '46',
    first gn-prov where gn-prov.codcia = 000
        and gn-prov.codpro = cb-dmov.codaux no-lock:
    assign
        x-ruc = gn-prov.ruc
        x-apepat = gn-prov.apepat
        x-apemat = gn-prov.apemat        
        x-nombre = gn-prov.nombre
        x-nroser = substring(cb-dmov.nrodoc,1,index(cb-dmov.nrodoc, '-') - 1)
        x-nrodoc = substring(cb-dmov.nrodoc,index(cb-dmov.nrodoc, '-') + 1)
        x-fchdoc = cb-dmov.fchdoc
        x-imptot = cb-dmov.impmn1.
    x-para1 = if x-imptot <= 700 then '0' else '1'.
    put string(x-ruc,'x(11)') + '|' +
        string(x-apepat, 'x(20)') + '|' +
        string(x-apemat, 'x(20)') + '|' +
        string(x-nombre, 'x(20)') + '|' +
        string(x-nroser, 'x(4)') + '|' +
        string(x-nrodoc, 'x(8)') + '|' +
        string(x-fchdoc, '99/99/9999') + '|' +
        string(x-imptot, '>>>>>>>>>>>9.99') + '|' +
        string(x-para1, 'x') + '|' +
        '10|||' format 'x(200)' skip.
end.    
output close.
