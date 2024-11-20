output to c:\tmp\menu-logistica.txt.
for each pf-g002 no-lock where PF-G002.Aplic-Id = 'lgc'
        and index(PF-G002.Seguridad-Grupos, 'S99') = 0:
    display
        PF-G002.Aplic-Id 
        PF-G002.CodMnu 
        PF-G002.Etiqueta  format 'x(50)'
        PF-G002.Seguridad-Grupos format 'x(250)'
        with stream-io no-box width 320.
end.
output close.
