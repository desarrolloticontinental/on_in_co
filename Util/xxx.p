def temp-table t-ccja like ccbccaja.
def temp-table t-dcja like ccbdcaja.

def var x-coddoc as char.
def var x-nrodoc as char.

def var i as int no-undo.

x-coddoc = 'bol,bol,bol,bol,bol,bol'.
x-nrodoc = '050003874,050003875,050005225,050005372,050005714,050005721'.

do i = 1 to num-entries(x-coddoc):
    for each ccbdcaja where codcia = 001
        and codref = entry(i, x-coddoc)
        and nroref = entry(i, x-nrodoc) no-lock:
        create t-dcja.
        buffer-copy ccbdcaja to t-dcja.
    end.        
end.

for each t-dcja ,
    first ccbccaja of t-dcja no-lock
    break by t-dcja.nrodoc:
    if first-of(t-dcja.nrodoc) then do:
        create t-ccja.
        buffer-copy ccbccaja to t-ccja.
    end.
end.

output to c:\tmp\ccbccaja.d.
for each t-ccja:
    export t-ccja.
end.
output close.    
output to c:\tmp\ccbdcaja.d.
for each t-dcja:
    export t-dcja.
end.
output close.
    
