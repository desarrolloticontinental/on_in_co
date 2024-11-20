output to c:\tmp\menu.txt.
for each pf-g002 no-lock where index(PF-G002.Seguridad-Grupos, 'S99') = 0:
    PUT UNFORMATTED
        PF-G002.Aplic-Id '|'
        PF-G002.CodMnu '|'
        PF-G002.Tipo '|'
        PF-G002.Programa '|' 
        PF-G002.Etiqueta  '|'
        PF-G002.Seguridad-Grupos
        SKIP.
end.
output close.
