def var x-impnac as dec no-undo.
def var x-impusa as dec no-undo.

output to c:\tmp\viru.txt.
for each faccpedi no-lock where codcia = 001
    and coddiv = '00000'
    and coddoc = 'ped'
    and codven = '151'
    and lookup(trim(flgest),'a,x,w,r,g') = 0
    and fchped >= 01/01/2007
    and fchped <= 03/31/2007:
    assign
        x-impnac = 0
        x-impusa = 0.
    if codmon = 1 
    then x-impnac = imptot.
    else x-impusa = imptot.
    display 
        fchped 
        nroped
        codcli 
        nomcli 
        glosa   view-as fill-in
        x-impnac when codmon = 1
        x-impusa when codmon = 2
        with stream-io no-box width 200.
end.   
output close.
